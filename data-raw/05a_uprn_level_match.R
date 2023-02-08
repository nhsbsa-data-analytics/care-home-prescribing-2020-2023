
# Load packages and global variables
source("R/analysis_packages.R")
source("R/workflow_helpers.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level FACT table
patient_db <- con %>%
  tbl(from = "INT646_FORM_LEVEL_FACT")

# Create a lazy table from the AddressBase Plus and CQC care home table
address_db <- con %>%
  tbl(from = "INT646_AB_PLUS_CQC_STACK")

# Get distinct patient-level address-postcode information
patient_address_db = patient_db %>% 
  select(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  distinct()

# Match the patients address to the AddressBase Plus and CQC care home addresses
match_db <- addressMatchR::calc_match_addresses(
  primary_df = patient_address_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "SINGLE_LINE_ADDRESS",
  lookup_df = address_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SINGLE_LINE_ADDRESS"
)

# Reconcile top joint scores, take non-ch over ch for caution
match_db = match_db %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_min(
    order_by = CH_FLAG,
    with_ties = FALSE
  ) %>%
  ungroup() %>% 
  # Rearrange location of address fields
  relocate(SINGLE_LINE_ADDRESS_LOOKUP, .after = SINGLE_LINE_ADDRESS) %>% 
  # Manually override the care home flag for select strings
  mutate(
    CH_FLAG = ifelse(
      test =
        CH_FLAG == 1L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT") > 0L,
      yes = 0L,
      no = CH_FLAG
    ),
    NURSING_HOME_FLAG = ifelse(
      test = CH_FLAG == 0L,
      yes = NA_integer_,
      no = NURSING_HOME_FLAG
    ),
    RESIDENTIAL_HOME_FLAG = ifelse(
      test = CH_FLAG == 0L,
      yes = NA_integer_,
      no = RESIDENTIAL_HOME_FLAG
    )
  )

# Join the matches back to the patient addresses
patient_match_db <- patient_db %>%
  left_join(y = match_db) %>% 
  tidyr::replace_na(list(CH_FLAG = 0L, MATCH_TYPE = "NO MATCH"))

# Define table name
table_name = "INT646_UPRN_MATCH"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Write the table back to DALP with indexes
patient_match_db %>%
  compute(
    name = table_name,
    indexes = list(c("POSTCODE")),
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()
