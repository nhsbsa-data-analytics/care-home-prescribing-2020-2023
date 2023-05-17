
# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Get start and end dates
start_date = stringr::str_extract_all(patient_address_data, "\\d{8}")[[1]][1]
end_date = stringr::str_extract_all(patient_address_data, "\\d{8}")[[1]][2]

# Create a lazy table from the item level FACT table
patient_db <- con %>%
  tbl(from = patient_address_data) %>% 
  filter(SUBSTR(POSTCODE, 1, 3) == "AL1")

# Create a lazy table from the AddressBase Plus and CQC care home table
address_db <- con %>%
  tbl(from = lookup_address_data) %>% 
  rename(AB_FLAG = CH_FLAG) %>% 
  filter(SUBSTR(POSTCODE, 1, 3) == "AL1")

# Address Base plus for parent uprn join
parent_db <- con %>% 
  tbl(from = parent_uprn_data) %>% 
  filter(SUBSTR(POSTCODE, 1, 3) == "AL1")

# Process and match address data -----------------------------------------------

# Get parent uprn occurring with AB-CQC lookup data
parent_uprn_db = address_db %>% 
  filter(!is.na(PARENT_UPRN)) %>% 
  select(UPRN = PARENT_UPRN) %>% 
  distinct()

# Get single GEO SLA per parent uprn
parent_db = parent_db %>% 
  inner_join(parent_uprn_db) %>% 
  # Why is only GEO SLA kept in this step? Is there not a need to join parent
  # UPRN on DPA or CORE SLA?
  mutate(SINGLE_LINE_ADDRESS_PARENT = paste(GEO_SINGLE_LINE_ADDRESS, POSTCODE)) %>% 
  group_by(SINGLE_LINE_ADDRESS_PARENT) %>% 
  # Similar to previous scripts, is this not a bit arbitrary taking max UPRN?
  summarise(PARENT_UPRN = max(UPRN)) %>%
  ungroup()

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
  # Fully ungroup - this is not needed, can simply group by new columns directly
  # ungroup() %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  # Get max monthly patient count
  summarise(MAX_MONTHLY_PATIENTS = max(MONTHLY_PATIENTS, na.rm = TRUE)) %>% 
  ungroup()

# drop_table_if_exists_db("PATIENT_ADDRESS_TEST")
# drop_table_if_exists_db("ADDRESS_TEST")
# 
# con %>%
#   copy_to(
#     df = patient_address_db,
#     name = "PATIENT_ADDRESS_TEST",
#     indexes = list(c("POSTCODE")),
#     temporary = FALSE
#   )
# 
# con %>%
#   copy_to(
#     df = address_db,
#     name = "ADDRESS_TEST",
#     indexes = list(c("POSTCODE")),
#     temporary = FALSE
#   )
# 
# patient_address_db <- con %>% 
#   tbl(from = "PATIENT_ADDRESS_TEST")
# 
# address_db <- con %>% 
#   tbl(from = "ADDRESS_TEST")

# Match the patients address to the AddressBase Plus and CQC care home addresses
# Why was JW matching used over the simpler Levenhstein method? Did you try the
# latter at all, only curious... 

# NOTE: there are a mix of all UPPER and mixed case in LOOKUP somehow. I don't
# think the INSTR used in the function is giving expected results: run the below
# after sticking a browser() at start of addressMatchR::calc_match_addresses and
# clicking 'next' until non_exact_match_jw_match_df is first created.

non_exact_match_jw_match_tib <- non_exact_match_jw_match_df %>% as_tibble()
token_match_tib <- non_exact_match_jw_match_tib %>% select(TOKEN, TOKEN_LOOKUP)
token_match_subs_tib <- token_match_tib %>%
  rowwise() %>%
  filter(
    grepl(toupper(TOKEN), toupper(TOKEN_LOOKUP), fixed = TRUE)|
      grepl(toupper(TOKEN_LOOKUP), toupper(TOKEN), fixed = TRUE)
  )

# Compare this to the result of the code block where further filters are applied
# to non_exact_match_jw_match_df - it is empty.
# Removing the INSTR calls and appending below code to the pipe works as expected
# (I am not sure on the performance of such a query...):

# %>% 
# filter(
#   sql("UPPER(TOKEN) LIKE '%' || UPPER(TOKEN_LOOKUP) || '%'") |
#   sql("UPPER(TOKEN_LOOKUP) LIKE '%' || UPPER(TOKEN) || '%'")
# )

# NOTE2: The > 1 RHS condition on the INSTR calls should be >= 1. E.g.
# INSTR("sub", "substring") = 1 and would be discarded for > 1.

# NOTE3: There could be some further eliminations that are easy pickings. E.g.
# run below while browsing in addressMatchR::calc_match_addresses, after the
# above steps in first NOTE:

# token_lens <- token_match_subs_tib %>% transmute(len = nchar(TOKEN)) %>% group_by(len) %>% summarise(n = n())
# token_lookup_lens <- token_match_subs_tib %>% transmute(len = nchar(TOKEN_LOOKUP)) %>% group_by(len) %>% summarise(n = n())

# Quite a few single char tokens...

# NOTE 4: Found some addresses which are for ELECTRICITY SUB STATION e.g.
# ELECTRICITY SUB STATION 43 M FROM HATFIELD NURSING... 

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
      TRUE ~ MATCH_TYPE
    ),
    
    # Two: generate UPRN flag
    UPRN_FLAG = case_when(
      MATCH_TYPE == "DOUBLE_KEYWORD" ~ 1L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) > 0L ~ 0L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS_LOOKUP, global_exclusion_keywords) > 0L ~ 0L,
      TRUE ~ AB_FLAG
    ),
    
    # Apply threshold and exclude some uprn
    # I don't get how we have any with scores <= 0.8
    UPRN_FLAG = case_when(
      SCORE <= 0.5 ~ 0, 
      !is.na(EXCLUDE_FOR_CH_LEVEL_ANALYSIS) ~ 0,
      TRUE ~ UPRN_FLAG
    ),
    
    # Remove non-uprn-flag uprn
    UPRN = case_when(
      UPRN_FLAG == 0 ~ NULL, 
      TRUE ~ UPRN
    ),
    
    # Remove non-uprn-flag parent uprn
    PARENT_UPRN = case_when(
      UPRN_FLAG == 0 ~ NULL, 
      TRUE ~ PARENT_UPRN
    ),
    
    # Three: generate 'general' CARE HOME flag (i.e. any care home)
    CH_FLAG = case_when(
      MATCH_TYPE == "PATIENT_COUNT" ~ 1L,
      MATCH_TYPE == "SINGLE_KEYWORD" ~ 1L,
      MATCH_TYPE == "DOUBLE_KEYWORD" ~ 1L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) > 0L ~ 0L,
      REGEXP_INSTR(SINGLE_LINE_ADDRESS_LOOKUP, global_exclusion_keywords) > 0L ~ 0L,
      TRUE ~ AB_FLAG
    ),
    
    # Complete SLA-lookup for exact matches (which default to null)
    SINGLE_LINE_ADDRESS_LOOKUP = case_when(
      MATCH_TYPE == "EXACT" ~ SINGLE_LINE_ADDRESS,
      TRUE ~ SINGLE_LINE_ADDRESS_LOOKUP
    )
  ) %>% 
  # Generate single postcode-SLA per uprn
  group_by(UPRN) %>% 
  mutate(
    SINGLE_LINE_ADDRESS_STANDARDISED = max(paste(SINGLE_LINE_ADDRESS, POSTCODE))
  ) %>% 
  ungroup() %>% 
  # Ensure 1 uprn per SLA
  group_by(SINGLE_LINE_ADDRESS_STANDARDISED) %>%
  # Using max is arbitrary? Why not most recent?
  mutate(UPRN = max(UPRN)) %>% 
  ungroup() %>% 
  # Get parent uprn info
  left_join(parent_db, by = "PARENT_UPRN")

# Join the matches back to the patient addresses
patient_match_db <- patient_db %>%
  # Filter could be applied once earlier in process to get subset of patients of
  # interest
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
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MIGAR"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO ADNSH"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MAMCP"))

# Disconnect from database
DBI::dbDisconnect(con)

# Print created table name output
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
