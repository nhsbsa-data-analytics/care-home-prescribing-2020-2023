
# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = cqc_data)

# Create a lazy table addressbase data
ab_plus_db <- con %>%
  tbl(from = ab_plus_data)

# Part one: Process cqc data ---------------------------------------------------

# Distinct SLA and postcode per uprn and location-id
cqc_db = cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    CH_FLAG = 1L
  ) %>% 
  filter(
    !is.na(UPRN),
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    CH_FLAG = max(CH_FLAG, na.rm = TRUE),
    LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
    UPRN = max(as.integer(UPRN), na.rm = TRUE),
    N_DISTINCT_UPRN = n_distinct(UPRN),
    NURSING_HOME_FLAG = max(as.integer(NURSING_HOME_FLAG), na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(as.integer(RESIDENTIAL_HOME_FLAG), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(N_DISTINCT_UPRN == 1)

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
  left_join(cqc_attributes_db) %>% 
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
  ungroup()

# Part Three: Save as table in dw ----------------------------------------------

# Specify db table name
year_month = get_year_month_from_date(end_date)
table_name = paste0("INT646_AB_PLUS_CQC_", year_month)

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back ot the db ...")

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
