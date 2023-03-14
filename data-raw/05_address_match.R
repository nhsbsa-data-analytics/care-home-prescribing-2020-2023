
# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Get start and end dates
start_date = stringr::str_extract_all(patient_address_data, "\\d{8}")[[1]][1]
end_date = stringr::str_extract_all(patient_address_data, "\\d{8}")[[1]][2]

# Create a lazy table from the item level FACT table
patient_db <- con %>%
  tbl(from = patient_address_data)

# Create a lazy table from the AddressBase Plus and CQC care home table
address_db <- con %>%
  tbl(from = lookup_address_data) %>% 
  rename(AB_FLAG = CH_FLAG)

# Process and match address data -----------------------------------------------

# Get distinct patient-level address-postcode information
patient_address_db = patient_db %>% 
  # If the address is NA we don't want to consider it
  filter(
    !is.na(SINGLE_LINE_ADDRESS),
    CALC_AGE >= 65,
    POSTCODE_CH == 1
    ) %>%
  # Add monthly patient count
  group_by(YEAR_MONTH, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(MONTHLY_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup(YEAR_MONTH) %>% 
  # Get max monthly patient count
  summarise(MAX_MONTHLY_PATIENTS = max(MONTHLY_PATIENTS, na.rm = TRUE)) %>% 
  ungroup()

# Match the patients address to the AddressBase Plus and CQC care home addresses
match_db = addressMatchR::calc_match_addresses(
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
      MATCH_TYPE == "DOUBLE_KEYWORD" ~ 1L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) > 0L ~ 0L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS_LOOKUP, global_exclusion_keywords) > 0L ~ 0L,
      TRUE ~ AB_FLAG
    ),
    
    # Apply threshold and exclude some uprn
    UPRN_FLAG = case_when(
      SCORE <= 0.5 ~ 0, 
      !is.na(EXCLUDE_FOR_CH_LEVEL_ANALYSIS) ~ 0,
      T ~ UPRN_FLAG
    ),
    
    # Remove non-uprn-flag uprn
    UPRN = case_when(
      UPRN_FLAG == 0 ~ NULL, 
      T ~ UPRN
    ),
    
    # Three: generate 'general' CARE HOME flag (i.e. any care home)
    CH_FLAG = case_when(
      MATCH_TYPE == "PATIENT_COUNT" ~ 1L,
      MATCH_TYPE == "SINGLE_KEYWORD" ~ 1L,
      MATCH_TYPE == "DOUBLE_KEYWORD" ~ 1L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) > 0L ~ 0L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS_LOOKUP, global_exclusion_keywords) > 0L ~ 0L,
      TRUE ~ AB_FLAG
    )
  )

# Join the matches back to the patient addresses
patient_match_db <- patient_db %>%
  filter(
    !is.na(SINGLE_LINE_ADDRESS),
    CALC_AGE >= 65,
    POSTCODE_CH == 1
  ) %>%
  left_join(y = match_db, by = c("POSTCODE", "SINGLE_LINE_ADDRESS")) %>% 
  tidyr::replace_na(
    list(
      AB_FLAG = 0L,
      UPRN_FLAG = 0L,
      CH_FLAG = 0L, 
      MATCH_TYPE = "NO MATCH"
      )
    )

# Define table name
table_name = paste0("INT646_MATCH_", start_date, "_", end_date)

# Remove table if exists
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to DALP with indexes
patient_match_db %>%
  compute(
    name = table_name,
    indexes = list(c("PF_ID", "YEAR_MONTH")),
    temporary = FALSE
  )

# Grant access
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MIGAR"))

# Disconnect from database
DBI::dbDisconnect(con)

# Print created table name output
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
