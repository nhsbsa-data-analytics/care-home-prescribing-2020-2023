# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = cqc_data)

# Create a lazy table addressbase data
ab_plus_db <- con %>%
  # Temporarily using Adnan's table as I dropped mine!
  tbl(in_schema("ADNSH", ab_plus_data))

# Get 4 values for final output
ab_epoch = pull_date_string(ab_plus_db, EPOCH)
cqc_date = pull_date_string(cqc_db, CQC_DATE)

# Distinct SLA and postcode per uprn and location-id 

# CAVEAT: A relatively small number of entries (264) were removed. We only kept
# the latest record for each combination of postcode and single line address.
# Latest is determined by latest inspection if it exists, or registration date
# if it does not. Of these removed entries, 11 individual care homes had changed
# their type from residential to nursing home, or vice versa.
cqc_raw_db <- cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    LAST_INSPECTION_DATE = TO_DATE(LAST_INSPECTION_DATE, "YYYY-MM-DD"), # NEW
    CH_FLAG = 1L
  ) %>% 
  filter(
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  # Adding ROW_ID for later validation purposes. Ultimately will not be needed,
  # unless we do decide to keep some validation code in the process?
  mutate(ROW_ID = rank(LOCATION_ID)) # NEW

# I have split off here in order to keep the raw data for the later validation.
# If validation code is not kept, can be joined into one longer pipe as before.

# The treatment of ratings and other summary calculations are dropped due to
# change in logic, using only most recent record per POSTCODE, SLA.
cqc_trimmed_db <- cqc_raw_db %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  # NOTE: Only one POSTCODE, SLA combination has more than one distinct UPRN
  mutate(
    N_DISTINCT_UPRN = n_distinct(UPRN),
    TEMP_DECIDER = coalesce(LAST_INSPECTION_DATE, REGISTRATION_DATE)
  ) %>%
  slice_max(
    TEMP_DECIDER,
    with_ties = FALSE
  ) %>% 
  ungroup()

# At this point cqc_trimmed_db has the most recent entry for each POSTCODE, SLA
# combination. But the UPRN is NOT distinct, some UPRNs occur up to 6 times. The
# reason for this is either typos, or as I can see from inspecting the data that
# names and addresses change over time for the same physical property.

# To handle this, again taking the view that the most recent entry is likely to
# be correct, we can again slice_max on the most recent entry. But since some
# 1k+ entries have NA UPRN, these need to be set aside and added back.

cqc_na_uprn_db <- cqc_trimmed_db %>% 
  filter(is.na(UPRN))

cqc_trimmed_db <- cqc_trimmed_db %>% 
  group_by(UPRN) %>% 
  slice_max(
    TEMP_DECIDER,
    with_ties = FALSE
  ) %>%
  select(-TEMP_DECIDER) %>% 
  ungroup() %>% 
  mutate(
    EXCLUDE_FOR_CH_LEVEL_ANALYSIS = case_when(
      is.na(UPRN) ~ "CQC SLA with a null UPRN",
      N_DISTINCT_UPRN > 1 ~ "CQC SLA associated with 2+ UPRNs", 
      # ...of which all UPRNs except one have already been discarded at this point
      TRUE ~ NULL
    )
  ) %>% 
  union_all(cqc_na_uprn_db)

# At this point cqc_trimmed_db has the most recent entry for each POSTCODE, SLA
# combination and the most recent entry per UPRN (where it exists), plus all
# entries with NA UPRN. This effectively replaces the need for the attributes
# table generation.

# Note that what is done here follows the same steps as previous code, i.e. get
# one entry by POSTCODE, SLA. Then get one entry per UPRN. It is just the logic
# deciding which row to keep at each step. Using the most recent may not be 100%
# foolproof (due to typos), but it is better than an arbitrary decision.

# It is simple to explain, i.e. "we take the most recent entry per postcode and
# single line address, followed by the most recent entry per UPRN; most recent
# is defined as the latest of last inspection date if it exists, or registration
# date if it does not exist"

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


# # From above processed data add residential and nursing home flag where possible
# cqc_attributes_db = cqc_db %>%
#   group_by(UPRN) %>%
#   mutate(
#     N_RATING = n_distinct(CURRENT_RATING),
#     CURRENT_RATING = ifelse(N_RATING > 1, NA, CURRENT_RATING)
#   ) %>% 
#   summarise(
#     LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
#     NURSING_HOME_FLAG = max(NURSING_HOME_FLAG, na.rm = TRUE),
#     RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG, na.rm = TRUE),
#     CURRENT_RATING = max(CURRENT_RATING, na.rm = TRUE),
#     NUMBER_OF_BEDS = max(NUMBER_OF_BEDS, na.rm = TRUE)
#   ) %>%
#   ungroup()

# Part Two: Process ab plus data and stack with cqc data -----------------------

# Get postcodes to filter ABP
postcodes_db = ab_plus_db %>% 
  filter(CH_FLAG == 1) %>% 
  select(POSTCODE) %>% 
  union_all(cqc_trimmed_db %>% select(POSTCODE)) %>% 
  distinct() 

# Instead of having a separate attributes table, we now have just one table for
# cqc, with distinct UPRNs. So this can be left joined to AB+, for the same
# result as previous attributes table. But, we now have ALL columns. So, we need
# to decide what to carry forward with.

# Remove columns no longer needed
cqc_trimmed_db <- cqc_trimmed_db %>% 
  select(-c(
    ROW_ID,
    # ODS_CODE, # This will be in cqc table as was added in script 01, whch I have yet to run...
    REGULATED_ACTIVITIES_NAMES,
    starts_with("KEY_QUESTION")
    # We should look at what is in data and remove any further ones not needed in base table
  ))

# Pivot SLA long
ab_plus_cqc_db = ab_plus_db %>% 
  inner_join(postcodes_db, by = "POSTCODE") %>% 
  select(-EPOCH) %>% 
  left_join(cqc_trimmed_db %>% select(-c(POSTCODE, CH_FLAG)), by = "UPRN") %>% 
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>% 
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>% 
  select(-ADDRESS_TYPE) %>% 
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE) %>% 
  union_all(
    cqc_trimmed_db %>% 
      mutate(
        UPRN = as.numeric(UPRN),
        PARENT_UPRN = NA_real_
      )
  ) %>%  
  # Get unique SLAs from among AB & CQC tables (individual SLAs may come from
  # either/all of: up to 3 variants in AB table and 1 variant in CQC table;
  # label not included due to potential for overlap) ...and keep one UPRN per
  # unique SLA (in case any SLAs have 2+ UPRNs)
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
table_name = gsub('-', '', paste0("INT646_ABP_CQC_", start_date, "_", end_date))

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Collect table as need to use more than default varchars2 for a couple of
# columns.
ab_plus_cqc_df <- ab_plus_cqc_db %>%
  collect() %>% 
  mutate(across(where(is.POSIXct), as.character))

# Write the table back to the DB
ab_plus_cqc_df %>%
  write_table_long_chars(con, table_name)

# Add indexes
con %>% add_indexes(table_name, c("UPRN", "POSTCODE"))

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% lapply(
  \(x) {
    DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO ", x))
  }
) %>% invisible()

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
