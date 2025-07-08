# TO DO:
# Bring new CQC columns added in script 01

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = cqc_tbl)

# Create a lazy table addressbase data
ab_plus_db <- con %>%
  tbl(from = abp_tbl) %>% 
  rename(EPOCH = RELEASE_DATE)

# Get 4 values for final output
ab_epoch = pull_date_string(ab_plus_db, EPOCH)
cqc_date = pull_date_string(cqc_db, CQC_DATE)

# Part one: Process cqc data ---------------------------------------------------

# We need some way of tie-breaking for de-duplicating rows (partial duplication).
# Without being sure, it seems likely that the higher the location ID, the later
# it was added to the data.
# Also, almost all locations with same postcode and SLA will have a different
# set of dates. Seems best to prefer last inspection, and if the same use
# registration then de-registration.
order_cols <- c(
  "LAST_INSPECTION_DATE",
  "REGISTRATION_DATE",
  "DEREGISTRATION_DATE",
  "LOCATION_ID"
)

cqc_dedup_db <- cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    LAST_INSPECTION_DATE = TO_DATE(LAST_INSPECTION_DATE, "YYYY-MM-DD"),
    CH_FLAG = 1L
  ) %>% 
  filter(
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  addressMatchR::tidy_postcode(POSTCODE) %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  window_order(!!!syms(order_cols)) %>% 
  # After arranging by date columns, we fill NA values downwards by carrying the
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
  mutate(
    LOCATION_ID = last(LOCATION_ID, order_by = order_cols),
    N_DISTINCT_UPRN = n_distinct(UPRN),
    UPRN = last(as.numeric(UPRN), order_by = order_cols),
    NURSING_HOME_FLAG = last(NURSING_HOME_FLAG, order_by = order_cols),
    RESIDENTIAL_HOME_FLAG = last(RESIDENTIAL_HOME_FLAG, order_by = order_cols),
    CH_FLAG = last(CH_FLAG)
  ) %>%
  slice_max(tibble(!!!syms(order_cols)), with_ties = FALSE) %>% 
  ungroup() %>%
  assert.alt(
    is_uniq.alt,
    LOCATION_ID,
    UPRN,
    pred_args = list(.by = c(POSTCODE, SINGLE_LINE_ADDRESS))
  ) %>% 
  mutate(
    # Flag records having either no, or multiple, UPRN
    EXCLUDE_FOR_CH_LEVEL_ANALYSIS = case_when(
      is.na(UPRN) ~ "CQC SLA where UPRN was null in all records pulled",
      N_DISTINCT_UPRN > 1 ~ "CQC SLA associated with 2+ UPRNs",
      # ...of which all UPRNs except one have already been discarded at this point
      T ~ NA_character_
    )
  ) %>% 
  select(
    POSTCODE,
    SINGLE_LINE_ADDRESS,
    LOCATION_ID,
    UPRN,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    CH_FLAG,
    EXCLUDE_FOR_CH_LEVEL_ANALYSIS
  )

cqc_attributes_db <- cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    LAST_INSPECTION_DATE = TO_DATE(LAST_INSPECTION_DATE, "YYYY-MM-DD")
  ) %>% 
  filter(
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  group_by(UPRN) %>%
  window_order(!!!syms(order_cols)) %>% 
  # After arranging by date columns, we fill NA values downwards by carrying the
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
  mutate(
    LOCATION_ID = last(LOCATION_ID, order_by = order_cols),
    NURSING_HOME_FLAG = last(NURSING_HOME_FLAG, order_by = order_cols),
    RESIDENTIAL_HOME_FLAG = last(RESIDENTIAL_HOME_FLAG, order_by = order_cols),
  ) %>%
  slice_max(tibble(!!!syms(order_cols)), with_ties = FALSE) %>% 
  ungroup() %>%
  assert.alt(is_uniq.alt, LOCATION_ID) %>% 
  select(
    UPRN,
    LOCATION_ID,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG
  )

# Add PARENT_UPRN to CQC data from ABP data, such that, when we later
# select one record per SLA (which could come from either CQC or ABP)
# it would always have PARENT_UPRN
cqc_db <- cqc_dedup_db %>%
  left_join(
    ab_plus_db %>% select(UPRN, PARENT_UPRN),
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
  addressMatchR::tidy_postcode(POSTCODE) %>% 
  mutate(NOT_NA = ifelse(!is.na(LOCATION_ID) & !is.na(UPRN), 1, 0)) %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_max(order_by = tibble("NOT_NA", "UPRN"), with_ties = FALSE) %>% 
  ungroup() %>% 
  select(-NOT_NA) %>% 
  assert.alt(
    is_uniq.alt,
    UPRN,
    pred_args = list(.by = c(POSTCODE, SINGLE_LINE_ADDRESS))
  ) %>% 
  mutate(
    START_DATE = start_date,
    END_DATE = end_date,
    AB_DATE = ab_epoch,
    CQC_DATE = cqc_date
  ) %>% 
  # There can be multiple identical rows following the postcode formatting, which
  # replaces commonly mistaken characters following some postcode logic
  distinct()


# Part Three: Save as table in dw ----------------------------------------------

# Specify db table name
table_name = address_tbl

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
