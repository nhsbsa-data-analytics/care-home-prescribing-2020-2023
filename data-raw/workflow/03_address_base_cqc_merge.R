# TO DO:
# Bring new CQC columns added in script 01

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = cqc_data)

# Create a lazy table addressbase data
ab_plus_db <- con %>%
  tbl(from = ab_plus_data) %>% 
  rename(EPOCH = RELEASE_DATE)

# Get 4 values for final output
ab_epoch = pull_date_string(ab_plus_db, EPOCH)
cqc_date = pull_date_string(cqc_db, CQC_DATE)

# Part one: Process cqc data ---------------------------------------------------

# Distinct SLA and postcode per uprn and location-id
cqc_df = cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    LAST_INSPECTION_DATE = TO_DATE(LAST_INSPECTION_DATE, "YYYY-MM-DD"),
    CH_FLAG = 1L,
    TEMP_DECIDER = coalesce(LAST_INSPECTION_DATE, REGISTRATION_DATE)
  ) %>% 
  filter(
    #!is.na(UPRN),  # Do not exclude records with null UPRNs, as these will still be used for CH/non-CH level analysis
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  collect() %>% # Collected, because function arrange() does not work with dbplyr
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  arrange(TEMP_DECIDER) %>% 
  # After arranging by TEMP_DECIDER, we fill NA values downwards by carrying the
  # last non NA value downward
  fill(
    LOCATION_ID,
    UPRN,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    .direction = "down" # only need down, since we want the last row
  ) %>% 
  # We take last value of each group (equivalent to taking last row since NAs
  # were filled)
  summarise(
    LOCATION_ID = last(LOCATION_ID, order_by = TEMP_DECIDER),
    N_DISTINCT_UPRN = n_distinct(UPRN),
    UPRN = last(as.numeric(UPRN), order_by = TEMP_DECIDER),
    NURSING_HOME_FLAG = last(as.integer(NURSING_HOME_FLAG), order_by = TEMP_DECIDER),
    RESIDENTIAL_HOME_FLAG = last(as.integer(RESIDENTIAL_HOME_FLAG), order_by = TEMP_DECIDER),
    CH_FLAG = last(CH_FLAG),
    .groups = "drop"
  ) %>%
  mutate(
    # Note, this is done after summarise(), so we'd later exclude only SLAs that
    # had null UPRNs in all CQC records, not just one record
    EXCLUDE_FOR_CH_LEVEL_ANALYSIS = case_when(
      is.na(UPRN) ~ "CQC SLA where UPRN was null in all records pulled",
      N_DISTINCT_UPRN > 1 ~ "CQC SLA associated with 2+ UPRNs",
      # ...of which all UPRNs except one have already been discarded at this point
      T ~ NA_character_
    )
  )

# Extract one row of CQC attributes per UPRN (e.g. residential and nursing home flags)
cqc_attributes_df = cqc_db %>%
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    LAST_INSPECTION_DATE = TO_DATE(LAST_INSPECTION_DATE, "YYYY-MM-DD"),
    TEMP_DECIDER = coalesce(LAST_INSPECTION_DATE, REGISTRATION_DATE)
  ) %>% 
  filter(
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>%
  group_by(UPRN) %>%
  collect() %>% # Collected, because function arrange() does not work with dbplyr
  group_by(UPRN) %>%
  arrange(TEMP_DECIDER) %>% 
  # After arranging by TEMP_DECIDER, we fill NA values downwards by carrying the
  # last non NA value downward
  fill(
    LOCATION_ID,
    UPRN,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    .direction = "down" # only need down, since we want the last row
  ) %>% 
  # We take last value of each group (equivalent to taking last row since NAs
  # were filled)
  summarise(
    LOCATION_ID = last(LOCATION_ID, order_by = TEMP_DECIDER),
    NURSING_HOME_FLAG = last(NURSING_HOME_FLAG, order_by = TEMP_DECIDER),
    RESIDENTIAL_HOME_FLAG = last(RESIDENTIAL_HOME_FLAG, order_by = TEMP_DECIDER),
    .groups = "drop"
  )

# The tables above needed to be processed locally due to inadequate dbplyr
# translation of the function last() (as of v.2.3.2); these tables are now
# copied into the DB temporarily to be used as lazy tables downstream; temp
# tables are removed at end of script; local dfs are removed now

cqc_table_temp <- "CQC_TEMP"
con %>% copy_to(
  cqc_df,
  name = cqc_table_temp,
  temporary = FALSE,
  overwrite = TRUE
)
cqc_db <- con %>% tbl(from = cqc_table_temp); rm(cqc_df)

cqc_attr_table_temp <- "CQC_ATTR_TEMP"
con %>% copy_to(
  cqc_attributes_df,
  name = cqc_attr_table_temp,
  temporary = FALSE,
  overwrite = TRUE
)
cqc_attributes_db <- con %>% tbl(from = cqc_attr_table_temp); rm(cqc_attributes_df)

# Add PARENT_UPRN to CQC data from ABP data, such that, when we later
# select one record per SLA (which could come from either CQC or ABP)
# it would always have PARENT_UPRN
cqc_db <- cqc_db |> left_join(
  ab_plus_db |> select(UPRN, PARENT_UPRN),
  by = "UPRN"
)

# Part Two: Process ab plus data and stack with cqc data -----------------------

# Get postcodes to filter ABP
postcodes_db = ab_plus_db %>% 
  filter(CH_FLAG == 1) %>% 
  select(POSTCODE) %>% 
  union_all(cqc_db %>% select(POSTCODE)) %>% 
  distinct()

# Add cqc attributes then pivot SLA long
ab_plus_cqc_db = ab_plus_db %>% 
  inner_join(postcodes_db, by = "POSTCODE") %>% 
  select(-EPOCH) %>% 
  left_join(cqc_attributes_db, by = "UPRN") %>% 
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>% 
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>% 
  select(-ADDRESS_TYPE) %>% 
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE) %>% 
  union_all(cqc_db) %>% 
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
  select(-N_DISTINCT_UPRN) %>% 
  personMatchR::format_postcode_db(POSTCODE)

# Part Three: Save as table in dw ----------------------------------------------

# Specify db table name
table_name = gsub('-', '', paste0("INT646_ABP_CQC_", start_date, "_", end_date))

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to the DB with indexes
ab_plus_cqc_db %>%
  compute(
    name = table_name,
    indexes = c("UPRN", "POSTCODE"),
    temporary = FALSE
  )

# Drop temp tables
drop_table_if_exists_db(cqc_table_temp)
drop_table_if_exists_db(cqc_attr_table_temp)

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% grant_table_access (table_name)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
