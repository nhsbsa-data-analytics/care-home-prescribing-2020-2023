
# Get data
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level FACT table
pat_db <- con %>%
  tbl(from = "INT646_FORMS_20210401_20220331")

# Create a lazy table from the AddressBase Plus and CQC care home table
add_db <- con %>%
  tbl(from = "INT646_ABP_CQC_20210401_20220331")

# Postcodes with a care home
postcodes_db = add_db %>% 
  select(POSTCODE) %>% 
  distinct()

# Create sample with form count
sample_hyphen = pat_db %>% 
  inner_join(postcodes_db) %>% 
  filter(
    CALC_AGE >= 65,
    REGEXP_LIKE(SINGLE_LINE_ADDRESS, "-")
  ) %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  summarise(FORMS = n_distinct(PF_ID)) %>% 
  ungroup() %>% 
  collect()

# Save as table
DBI::dbWriteTable(con, "INT646_HYPHEN_SAMPLE", sample_hyphen, overwrite = TRUE)

# Connect to sample data
pat_db <- con %>%
  tbl(from = "INT646_HYPHEN_SAMPLE")

# Match output one
match_one = calc_match_addresses(
  primary_df = pat_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "SINGLE_LINE_ADDRESS",
  lookup_df = add_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SINGLE_LINE_ADDRESS"
  ) %>% 
  # Reconcile top joint scores, take non-ch over ch for caution
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_min(
    order_by = CH_FLAG,
    with_ties = FALSE
  ) %>%
  ungroup() %>% 
  # Select and rename
  select(
    POSTCODE,
    SINGLE_LINE_ADDRESS,
    FORMS,
    ONE_SLA = SINGLE_LINE_ADDRESS_LOOKUP,
    ONE_UPRN = UPRN,
    ONE_CH_FLAG = CH_FLAG,
    ONE_SCORE = SCORE,
    ONE_MATCH_TYPE = MATCH_TYPE
  ) %>% 
  filter(ONE_CH_FLAG == 1) %>% 
  collect()
  

# Remove patient address hyphens
pat_two_db = pat_db %>% 
  mutate(
    SINGLE_LINE_ADDRESS = REPLACE(SINGLE_LINE_ADDRESS, "-", " "),
    SINGLE_LINE_ADDRESS = REGEXP_REPLACE(SINGLE_LINE_ADDRESS, "( ){2,}", " ")
    )

# Remove lookup hyphens
add_two_db = add_db %>% 
  mutate(
    SINGLE_LINE_ADDRESS = REPLACE(SINGLE_LINE_ADDRESS, "-", " "),
    SINGLE_LINE_ADDRESS = REGEXP_REPLACE(SINGLE_LINE_ADDRESS, "( ){2,}", " ")
  )

# Match output one
match_two = calc_match_addresses(
  primary_df = pat_two_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "SINGLE_LINE_ADDRESS",
  lookup_df = add_two_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SINGLE_LINE_ADDRESS"
  ) %>% 
  # Reconcile top joint scores, take non-ch over ch for caution
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_min(
    order_by = CH_FLAG,
    with_ties = FALSE
  ) %>%
  ungroup() %>% 
  # Select and rename
  select(
    POSTCODE,
    SINGLE_LINE_ADDRESS,
    FORMS,
    TWO_SLA = SINGLE_LINE_ADDRESS_LOOKUP,
    TWO_UPRN = UPRN,
    TWO_CH_FLAG = CH_FLAG,
    TWO_SCORE = SCORE,
    TWO_MATCH_TYPE = MATCH_TYPE
  ) %>% 
  filter(TWO_CH_FLAG == 1) %>% 
  collect()

# Keep hyphens has +33k form count
match_one %>% summarise(n = sum(FORMS))
match_two %>% summarise(n = sum(FORMS))

# Remove hyphen has +70k exact match
match_one %>% group_by(ONE_MATCH_TYPE) %>% summarise(n = sum(FORMS))
match_two %>% group_by(TWO_MATCH_TYPE) %>% summarise(n = sum(FORMS))

# Final output
output = match_one %>% 
  rename(SINGLE_LINE_ADDRESS_CLEAN = SINGLE_LINE_ADDRESS) %>% 
  mutate(
    SINGLE_LINE_ADDRESS = gsub("-", " ", SINGLE_LINE_ADDRESS_CLEAN),
    SINGLE_LINE_ADDRESS = gsub("( ){2,}", " ", SINGLE_LINE_ADDRESS)
  ) %>% 
  # Lose one record as 2 hyphen address grouped as 1 
  full_join(match_two) %>% 
  select(
    POSTCODE,
    SINGLE_LINE_ADDRESS_CLEAN,
    SINGLE_LINE_ADDRESS,
    everything()
  )

# Save as table
DBI::dbWriteTable(con, "INT646_HYPHEN_OUTPUT", output, overwrite = TRUE)

# Disconnect and clean
dbDisconnect(con); rm(list = ls()); gc()
