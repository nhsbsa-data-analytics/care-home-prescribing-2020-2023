
# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level FACT table
patient_db <- con %>%
  tbl(from = patient_address_data)

# Create a lazy table from the AddressBase Plus and CQC care home table
address_db <- con %>%
  tbl(from = lookup_address_data) %>% 
  rename(AB_FLAG = CH_FLAG)

# Get distinct patient-level address-postcode information
patient_address_db = patient_db %>% 
  # If the address is NA we don't want to consider it
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  # Add monthly patient count
  group_by(YEAR_MONTH, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(MONTHLY_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup(YEAR_MONTH) %>% 
  # Get max monthly patient count
  summarise(MAX_MONTHLY_PATIENTS = max(MONTHLY_PATIENTS, na.rm = TRUE)) %>%
  ungroup()

# Match the patients address to the AddressBase Plus and CQC care home addresses
match_db <- addressMatchR::calc_match_addresses(
  primary_df = patient_address_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "SINGLE_LINE_ADDRESS",
  lookup_df = address_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SINGLE_LINE_ADDRESS"
)

# Define keyword list for case statements
care_home_keywords = "CARE HOME|CARE-HOME|NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME"
global_exclusion_keywords = "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT"
extra_exclusion_keywords = "CONVENT|HOSPITAL|MARINA|MONASTERY|RECOVERY"

# Reconcile top joint scores, take non-ch over ch for caution
match_db = match_db %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_min(
    order_by = AB_FLAG,
    with_ties = FALSE
  ) %>%
  ungroup() %>% 
  mutate(
    # One: generate new match types
    MATCH_TYPE = case_when(
      # Rank 1: both sides key word
      AB_FLAG == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, care_home_keywords) > 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, extra_exclusion_keywords) == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS_LOOKUP, care_home_keywords) > 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS_LOOKUP, global_exclusion_keywords) == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS_LOOKUP, extra_exclusion_keywords) == 0L ~ "DOUBLE_KEYWORD",
      
      # Rank 2: only patient address keyword
      AB_FLAG == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, care_home_keywords) > 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, extra_exclusion_keywords) == 0L ~ "SINGLE_KEYWORD",
      
      # Rank 3: max monthy patient count greater than 5
      AB_FLAG == 0L &
        MAX_MONTHLY_PATIENTS >= 5L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, extra_exclusion_keywords) == 0L ~ "PATIENT_COUNT",
      
      # Else: just the existing match type 
      T ~ MATCH_TYPE
    ),
    
    # Two: generate UPRN flag
    UPRN_FLAG = case_when(
      REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) > 0L ~ 0L,
      MATCH_TYPE == "DOUBLE_KEYWORD" ~ 1L,
      TRUE ~ AB_FLAG
    ),
    
    # Three: generate 'general' CARE HOME flag (i.e. any care home)
    CH_FLAG = case_when(
      MATCH_TYPE == "PATIENT_COUNT" ~ 1L,
      MATCH_TYPE == "SINGLE_KEYWORD" ~ 1L,
      TRUE ~ UPRN_FLAG
    )
  )

# Join the matches back to the patient addresses
patient_match_db <- patient_db %>%
  left_join(y = match_db, by = c("POSTCODE", "SINGLE_LINE_ADDRESS")) %>% 
  tidyr::replace_na(
    list(
      AB_FLAG = 0L,
      UPRN_FLAG = 0L,
      CH_FLAG = 0L, 
      MATCH_TYPE = "NO MATCH"
      )
    )  %>% 
  # Rearrange location of address fields
  relocate(SINGLE_LINE_ADDRESS_LOOKUP, .after = SINGLE_LINE_ADDRESS) %>% 
  relocate(UPRN_FLAG, .after = AB_FLAG) %>% 
  relocate(CH_FLAG, .after = UPRN_FLAG) 

# Define table name
year_month = get_year_month_form_date(end_date)
table_name = paste0("INT646_MATCH_", year_month)

# Remove table if exists
drop_table_if_exists_db(table_name)

# Write the table back to DALP with indexes
patient_match_db %>%
  compute(
    name = table_name,
    indexes = list(c("POSTCODE")),
    temporary = FALSE
  )

# Grant access
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MIGAR"))

# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()
