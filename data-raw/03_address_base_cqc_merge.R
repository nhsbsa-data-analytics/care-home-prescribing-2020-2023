
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
  # Set rating to null if multiple present per SLA-postcode
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(
    N_RATING = n_distinct(CURRENT_RATING),
    # Why not use the most recent current rating? Has any analysis been done on ratings yet? Will it, if not?
    CURRENT_RATING = ifelse(N_RATING > 1, NA, CURRENT_RATING)
  ) %>% 
  # Why the ungroup and then group by same columns?
  # ungroup() %>%
  # group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    # At this point CH_FLAG == 1L for all rows
    # CH_FLAG = max(CH_FLAG, na.rm = TRUE),
    # Why not use the most recent location ID? Has any analysis been done on location ID yet? Will it, if not?
    LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
    N_DISTINCT_UPRN = n_distinct(UPRN),
    UPRN = max(as.integer(UPRN), na.rm = TRUE), # One UPRN is retained, chosen arbitrarily
    # There are ~200 entries with both NHF and RHF == 1. How is this handled, are they treated as both?
    NURSING_HOME_FLAG = max(as.integer(NURSING_HOME_FLAG), na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(as.integer(RESIDENTIAL_HOME_FLAG), na.rm = TRUE),
    # Why not use the most recent number of beds? Has any analysis been done on beds yet? Will it, if not?
    NUMBER_OF_BEDS = max(NUMBER_OF_BEDS, na.rm = TRUE),
    # The current rating is a string, so I believe lexicographic ordering is used, unless I missed some sort of levels being created for this?
    # Why is the max according to lexicographical ordering kept?
    # Why not use the most recent current rating? Has any analysis been done on ratings yet? Will it, if not?
    CURRENT_RATING = max(CURRENT_RATING, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    # Note, this is done after summarise(), so we'd later exclude only SLAs that had null UPRNs in all CQC records, not just one record
    # Just an observation, that there is only a single SLA with 2 UPRNS. Typo?
    # Better to manually fix the typo than exclude. I know one such exclusion
    # will make very little difference at a classification level, but when at
    # the CH level it may be missed.
    EXCLUDE_FOR_CH_LEVEL_ANALYSIS = case_when(
      is.na(UPRN) ~ "CQC SLA with a null UPRN",
      N_DISTINCT_UPRN > 1 ~ "CQC SLA associated with 2+ UPRNs",  # ...of which all UPRNs except one have already been discarded at this point
      T ~ NULL
    )
  )

# From above processed data add residential and nursing home flag where possible
cqc_attributes_db = cqc_db %>%
  group_by(UPRN) %>%
  mutate(
    # Observation: There is a 1:1 correspondence btwn CQC SLA with a null UPRN and N_RATING == 6...surprising and why?
    N_RATING = n_distinct(CURRENT_RATING),
    # Why not use the most recent current rating? Has any analysis been done on ratings yet? Will it, if not?
    CURRENT_RATING = ifelse(N_RATING > 1, NA, CURRENT_RATING)
  ) %>% 
  summarise(
    # Why not use the most recent location ID? Has any analysis been done on location ID yet? Will it, if not?
    LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
    # There are ~200 entries with both NHF and RHF == 1. How is this handled, are they treated as both?
    NURSING_HOME_FLAG = max(NURSING_HOME_FLAG, na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG, na.rm = TRUE),
    # Why not use the most recent current rating? Has any analysis been done on ratings yet? Will it, if not?
    CURRENT_RATING = max(CURRENT_RATING, na.rm = TRUE),
    # Why not use the most recent number of beds? Has any analysis been done on beds yet? Will it, if not?
    NUMBER_OF_BEDS = max(NUMBER_OF_BEDS, na.rm = TRUE)
  ) %>%
  ungroup()

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
  select(-ADDRESS_TYPE) %>% 
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE) %>% 
  union_all(cqc_db) %>% 
  # Get unique SLAs from among AB & CQC tables
  # (individual SLAs may come from either/all of: up to 3 variants in AB table and 1 variant in CQC table;
  #  label not included due to potential for overlap)
  # ...and keep one UPRN per unique SLA (in case any SLAs have 2+ UPRNs)
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  # Why not use the most recent UPRN?
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
table_name = gsub('-', '', glue("INT646_ABP_CQC_{start_date}_{end_date}"))

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to the DB with indexes
ab_plus_cqc_db %>%
  compute(
    name = table_name,
    # Why not include SLA in index? Why the extra c()?
    indexes = list(c("UPRN", "POSTCODE")),
    temporary = FALSE
  )

# Grant access
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MIGAR"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO ADNSH"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MAMCP"))

# Disconnect from database
DBI::dbDisconnect(con)

# Print that table has been created
print(glue("This script has created table: {table_name}"))

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
