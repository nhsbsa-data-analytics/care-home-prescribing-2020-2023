
# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

cqc_date = "20230217"

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = cqc_data) %>% 
  mutate(CQC_DATE = cqc_date)

# Create a lazy table addressbase data
ab_plus_db <- con %>%
  tbl(from = ab_plus_data)

# Get 4 values for final output
ab_epoch = pull_date_string(ab_plus_db, EPOCH)
start_date = pull_date_string(ab_plus_db, START_DATE)
end_date = pull_date_string(ab_plus_db, END_DATE)
cqc_date = pull_date_string(cqc_db, CQC_DATE)

# Part one: Process cqc data ---------------------------------------------------

# Distinct SLA and postcode per uprn and location-id
cqc_db = cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    CH_FLAG = 1L
  ) %>% 
  filter(
    #!is.na(UPRN),  # Do not exclude records with null UPRNs, as these will be used for CH/non-CH level analysis
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    CH_FLAG = max(CH_FLAG, na.rm = TRUE),
    LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
    N_DISTINCT_UPRN = n_distinct(UPRN),
    UPRN = max(as.integer(UPRN), na.rm = TRUE), # One UPRN is retained, chosen arbitrarily
    NURSING_HOME_FLAG = max(as.integer(NURSING_HOME_FLAG), na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(as.integer(RESIDENTIAL_HOME_FLAG), na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    # Note, this is done after summarise(), so we'd later exclude only SLAs that had null UPRNs in all CQC records, not just one record
    EXCLUDE_FOR_CH_LEVEL_ANALYSIS = case_when(
      is.na(UPRN) ~ "CQC SLA with a null UPRN",
      N_DISTINCT_UPRN > 1 ~ "CQC SLA associated with 2+ UPRNs",  # ...of which all UPRNs except one have already been discarded at this point
      T ~ NULL
      )
  )

# From above processed data add residential and nursing home flag where possible
cqc_attributes_db = cqc_db %>%
  group_by(UPRN) %>%
  summarise(
    LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
    NURSING_HOME_FLAG = max(NURSING_HOME_FLAG, na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG, na.rm = TRUE)
  ) %>%
  ungroup()

# Part Two: Process ab plus data and stack with cqc data -----------------------

# Add cqc attributes then pivot SLA long
ab_plus_cqc_db = ab_plus_db %>% 
  select(-EPOCH) %>% 
  left_join(cqc_attributes_db, by = "UPRN") %>% 
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>% 
  select(-ADDRESS_TYPE) %>% 
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE) %>% 
  union_all(cqc_db) %>% 
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
  )

# Part Three: Save as table in dw ----------------------------------------------

# Specify db table name
table_name = paste0("INT646_ABP_CQC_", start_date, "_", end_date)

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to the DB with indexes
ab_plus_cqc_db %>%
  compute(
    name = table_name,
    indexes = list(c("UPRN", c("POSTCODE"))),
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove objects and clean environment
rm(list = ls()); gc()
