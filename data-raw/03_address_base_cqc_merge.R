
# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = cqc_data)

# Create a lazy table addressbase data
ab_plus_db <- con %>%
  tbl(from = ab_plus_data)

# Get 4 values for final output
ab_epoch = pull_date_string(ab_plus_db, EPOCH)
cqc_date = pull_date_string(cqc_db, CQC_DATE)

# Part one: Process cqc data ---------------------------------------------------

# Distinct SLA and postcode per uprn and location-id 

# CAVEAT: A relatively small number of entries (264) were removed. We only kept
# the latest record for each combination of postcode and single line address.
# Latest is determined by latest inspection if it exists, or registration date
# if it does not. Of these removed entries, 11 individual care homes had changed
# their type from residential to nursing home, or vice versa.
cqc_db_raw <- cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    LAST_INSPECTION_DATE = TO_DATE(LAST_INSPECTION_DATE, "YYYY-MM-DD"),
    CH_FLAG = 1L
  ) %>% 
  filter(
    #!is.na(UPRN),  # Do not exclude records with null UPRNs, as these will be used for CH/non-CH level analysis
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  mutate(ROW_ID = rank(LOCATION_ID))

cqc_db_trimmed <- cqc_db_raw %>% 
  mutate(TEMP_DECIDER = coalesce(LAST_INSPECTION_DATE, REGISTRATION_DATE)) %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  summarise(
    N_DISTINCT_UPRN = n_distinct(UPRN),
    .groups = "drop"
  ) %>% 
  slice_max(
    TEMP_DECIDER,
    with_ties = FALSE
  ) %>% 
  ungroup() %>% 
  select(-TEMP_DECIDER) %>% 
  mutate(
    EXCLUDE_FOR_CH_LEVEL_ANALYSIS = case_when(
      is.na(UPRN) ~ "CQC SLA with a null UPRN",
      N_DISTINCT_UPRN > 1 ~ "CQC SLA associated with 2+ UPRNs", 
      # ...of which all UPRNs except one have already been discarded at this point
      TRUE ~ NULL
    )
  )

# Validation and checking

# The whole commented out section below is just some checking of the data at
# this stage. So this can be either removed in the final workflow, or left in if
# deemed useful, e.g. to get numbers for caveats.

# ## Get all removed entries. Count is 264.
# cqc_db_removed <- cqc_db_raw %>% 
#   anti_join(
#     cqc_db_trimmed,
#     by = c("ROW_ID")
#   )
# 
# removed_count = cqc_db_removed %>% collect() %>% nrow()
# 
# print(glue("Number of entries removed: {removed_count}"))
# 
# ## Get all entries for which the postcode/SLA combination is among the removed
# ## rows. This is simply for sanity checking by visual inspection.
# cqc_db_trimmed_check <- cqc_db_trimmed %>% 
#   transmute(
#     POSTCODE,
#     SINGLE_LINE_ADDRESS,
#     REGISTRATION_DATE,
#     DEREGISTRATION_DATE,
#     LAST_INSPECTION_DATE,
#     NURSING_HOME_FLAG,
#     RESIDENTIAL_HOME_FLAG,
#     CURRENT_RATING
#   ) %>%
#   collect()
# 
# cqc_db_removed_check <- cqc_db_removed %>% 
#   transmute(
#     POSTCODE,
#     SINGLE_LINE_ADDRESS,
#     REGISTRATION_DATE,
#     DEREGISTRATION_DATE,
#     LAST_INSPECTION_DATE,
#     NURSING_HOME_FLAG,
#     RESIDENTIAL_HOME_FLAG,
#     CURRENT_RATING
#   ) %>%
#   collect()
# 
# cqc_db_check <- cqc_db_removed_check %>% 
#   left_join(
#     cqc_db_trimmed_check,
#     by = c("POSTCODE", "SINGLE_LINE_ADDRESS"),
#     suffix = c(".REMOVED", ".KEPT")
#   )
# 
# cqc_db_check <- cqc_db_check %>%
#   pivot_longer(
#     cols = c(ends_with(".REMOVED"), ends_with(".KEPT")),
#     names_to = c(".value", "STATUS"),
#     names_pattern = "(.*)\\.(.*)"
#   ) %>% 
#   arrange(POSTCODE, SINGLE_LINE_ADDRESS, STATUS)
# 
# ## Find care homes which changed type - count is 11
# cqc_changed_type <- cqc_db_raw %>%
#   mutate(
#     NH = if_else(NURSING_HOME_FLAG == 1, "NH", NA_character_),
#     RH = if_else(RESIDENTIAL_HOME_FLAG == 1, "RH", NA_character_),
#   ) %>% 
#   collect() %>% 
#   unite(HOME_TYPE, NH, RH, sep = " ", na.rm = TRUE) %>% 
#   select(
#     POSTCODE,
#     SINGLE_LINE_ADDRESS,
#     HOME_TYPE,
#     NURSING_HOME_FLAG,
#     RESIDENTIAL_HOME_FLAG
#   ) %>% 
#   group_by(POSTCODE, SINGLE_LINE_ADDRESS, HOME_TYPE) %>%
#   summarise(n = n()) %>%
#   summarise(n = n()) %>%
#   ungroup() %>% 
#   filter(n > 1) %>% 
#   select(-n)
# 
# changed_type_count = cqc_changed_type %>%
#   collect() %>%
#   nrow()
# 
# print(glue("Number of homes changing type: {changed_type_count}"))
# 

# Part Two: Process ab plus data and stack with cqc data -----------------------

# Get postcodes to filter ABP
postcodes_db = ab_plus_db %>% 
  filter(CH_FLAG == 1) %>% 
  select(POSTCODE) %>% 
  union_all(cqc_db_trimmed %>% select(POSTCODE)) %>% 
  distinct()

## Checking POSTCODE & CH_FLAG - we currently take the AB+ values forward
# ab_plus_cqc_db_check = ab_plus_db %>%
#   inner_join(postcodes_db, by = "POSTCODE") %>%
#   select(-EPOCH) %>%
#   left_join(cqc_db_trimmed, by = "UPRN") %>%
#   collect() %>% 
#   transmute(
#     POSTCODE.x,
#     POSTCODE.y,
#     POSTCODE_EQUAL = case_when(
#       (POSTCODE.x == POSTCODE.y) | (is.na(POSTCODE.x) & is.na(POSTCODE.y)) ~ "TRUE",
#       is.na(POSTCODE.x) ~ "NA AB+",
#       is.na(POSTCODE.y) ~ "NA CQC",
#       (POSTCODE.x != POSTCODE.y) ~ "FALSE",
#       TRUE ~ NA_character_
#     ),
#     CH_FLAG.x,
#     CH_FLAG.y,
#     CH_FLAG_EQUAL = case_when(
#       (CH_FLAG.x == CH_FLAG.y) | (is.na(CH_FLAG.x) & is.na(CH_FLAG.y)) ~ "TRUE",
#       is.na(CH_FLAG.x) ~ "NA AB+",
#       is.na(CH_FLAG.y) ~ "NA CQC",
#       (CH_FLAG.x != CH_FLAG.y) ~ "FALSE",
#       TRUE ~ NA_character_
#     )
#   )
# 
# ab_plus_cqc_db_check_summary <- ab_plus_cqc_db_check %>% 
#   group_by(POSTCODE_EQUAL, CH_FLAG_EQUAL) %>% 
#   summarise(n = n())
# 
#   POSTCODE_EQUAL CH_FLAG_EQUAL      n
#   <chr>          <chr>          <int>
# 1 FALSE          FALSE              1
# 2 FALSE          TRUE             343
# 3 NA CQC         NA CQC        521151
# 4 TRUE           FALSE            564
# 5 TRUE           TRUE           13588
##

# Remove columns no longer needed
cqc_db_trimmed <- cqc_db_trimmed %>% 
  select(-c(
    ROW_ID,
    POSTCODE,
    CH_FLAG,
    ODS_CODE,
    REGULATED_ACTIVITIES_NAMES,
    starts_with("KEY_QUESTION")
  ))

# Add cqc data then pivot SLA long
ab_plus_cqc_db = ab_plus_db %>%
  inner_join(postcodes_db, by = "POSTCODE") %>% 
  select(-EPOCH) %>% 
  left_join(
    cqc_db_trimmed,
    by = "UPRN"
  ) %>%
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>% 
  select(-ADDRESS_TYPE) %>% 
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE) %>% 
  union_all(
    cqc_db_trimmed %>% 
      mutate(
        UPRN = as.numeric(UPRN),
        PARENT_UPRN = NA_real_
      )
  ) %>% 
  # Get unique SLAs from among AB & CQC tables
  # (individual SLAs may come from either/all of: up to 3 variants in AB table and 1 variant in CQC table;
  #  label not included due to potential for overlap)
  # ...and keep one UPRN per unique SLA (in case any SLAs have 2+ UPRNs)
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_max(order_by = UPRN, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(
    START_DATE = start_date,
    END_DATE = end_date,
    AB_DATE = ab_epoch,
    CQC_DATE = cqc_date
  ) %>% 
  select(-N_DISTINCT_UPRN)

# Part Three: Save as table in dw ----------------------------------------------

# Specify db table name
table_name = paste0(
  "INT646_ABP_CQC_", gsub("-", "", start_date), "_", gsub("-", "", end_date)
  )

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to the DB with indexes
ab_plus_cqc_df <- ab_plus_cqc_db %>%
  collect()

ab_plus_cqc_df %>%
  mutate(
    across(where(is.POSIXct), as.character)
  ) %>% 
  write_table_long_chars(con, table_name)

con %>% add_indexes(table_name, c("UPRN", "POSTCODE"))

# Grant access
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MIGAR"))
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO ADNSH"))
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MAMCP"))

# Disconnect from database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
