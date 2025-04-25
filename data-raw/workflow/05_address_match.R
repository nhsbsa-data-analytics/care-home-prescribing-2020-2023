# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

thousand <- 10^3
million <- 10^6

# Get start and end dates
# NOTE: The existing variables can be used here instead of recalculating
# start_date = stringr::str_extract_all(patient_address_tbl, "\\d{8}")[[1]][1]
# end_date = stringr::str_extract_all(patient_address_tbl, "\\d{8}")[[1]][2]

# Create a lazy table from the item level FACT table
patient_db <- con %>%
  tbl(from = patient_address_tbl)

# Create a lazy table from the AddressBase Plus and CQC care home table
address_db <- con %>%
  tbl(from = lookup_address_tbl) %>%
  assert.alt(not_na.alt, POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  rename(AB_FLAG = CH_FLAG)


# Address Base plus for parent uprn join
parent_db <- con %>% 
  tbl(from = parent_uprn_tbl)

# Process and match address data -----------------------------------------------

# Get parent uprn occurring with AB-CQC lookup data
parent_uprn_db = address_db %>% 
  filter(!is.na(PARENT_UPRN)) %>% 
  select(UPRN = PARENT_UPRN) %>% 
  distinct() #%>% # TEMP REMOVAL WHILE CHECKING E2E PIPELINE
  # verify(nrow.alt(.) > 10 * thousand) # TEMP REMOVAL WHILE CHECKING E2E PIPELINE 

# Get single GEO SLA per parent uprn
parent_db = parent_db %>% 
  inner_join(parent_uprn_db) %>% 
  mutate(SINGLE_LINE_ADDRESS_PARENT = paste0(GEO_SINGLE_LINE_ADDRESS, " ", POSTCODE)) %>% 
  group_by(SINGLE_LINE_ADDRESS_PARENT) %>% 
  summarise(PARENT_UPRN = max(UPRN, na.rm = TRUE)) %>%
  ungroup()

# Get distinct patient-level address-postcode information
patient_address_db = patient_db %>% 
  # If the address is NA we don't want to consider it
  filter(
    !is.na(SINGLE_LINE_ADDRESS),
    CALC_AGE >= 65,
    POSTCODE_CH == 1
  ) %>%
  # No check for null SLA, as already filtered to non-null
  assert.alt(not_na.alt, POSTCODE) %>% 
  # Add monthly patient count
  group_by(YEAR_MONTH, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(MONTHLY_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup() %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  # Get max monthly patient count
  summarise(MAX_MONTHLY_PATIENTS = max(MONTHLY_PATIENTS, na.rm = TRUE)) %>% 
  ungroup() #%>% # TEMP REMOVAL WHILE CHECKING E2E PIPELINE
  # verify(nrow.alt(.) > 240 * thousand) # TEMP REMOVAL WHILE CHECKING E2E PIPELINE

# Original step here was to use addressMatchR::calc_match_addresses. However, at
# some point something has broken. It used to take ~40 mins to run, but now takes
# an infeasible time to run (for context, running with just 5 addresses in each
# table takes several minutes, so projected time to run the full tables would be
# weeks!).

# Due to this, the process is now run direct in the DW.

# Drop any existing temp tables from previous runs
drop_table_if_exists_db("PATIENT_ADDRESS_TEMP")
drop_table_if_exists_db("LOOKUP_ADDRESS_TEMP")
drop_table_if_exists_db("MATCH_TEMP")

# Save the addresses to match (and all additional data) to DW temporarily
patient_address_db %>%
  compute(
    name = "PATIENT_ADDRESS_TEMP",
    temporary = FALSE
  )

# Save the lookup addresses (and all additional data) to DW temporarily
address_db %>%
  compute(
    name = "LOOKUP_ADDRESS_TEMP",
    temporary = FALSE
  )

# Use the temporary tables to calculate matches in a new temporary table,
# and join back to keep all data
query <- DBI::dbSendQuery(
  conn = con,
  statement = "
    create table MATCH_TEMP compress for query high as
    with lookup as (
        select  -- append LOOKUP to differentiate the identical named columns
            postcode POSTCODE_LOOKUP,
            single_line_address SINGLE_LINE_ADDRESS_LOOKUP
        from lookup_address_temp
    )
    select
        pat.POSTCODE,
        pat.SINGLE_LINE_ADDRESS,
        MAX_MONTHLY_PATIENTS,
        UPRN,
        PARENT_UPRN,
        m.SINGLE_LINE_ADDRESS_LOOKUP,
        AB_FLAG,
        LOCATION_ID,
        NURSING_HOME_FLAG,
        RESIDENTIAL_HOME_FLAG,
        EXCLUDE_FOR_CH_LEVEL_ANALYSIS,
        START_DATE,
        END_DATE,
        AB_DATE,
        CQC_DATE,
        SCORE,
        MATCH_TYPE
    from patient_address_temp pat
    left join (
        select * from -- cannot use the macro in 'with' block ...
            lib.f_calc_match_addresses( 
                table_one_name => patient_address_temp, 
                table_one_postcode => columns(POSTCODE), 
                table_one_address => columns(SINGLE_LINE_ADDRESS), 
                table_two_name => lookup, 
                table_two_postcode => columns(POSTCODE_LOOKUP), 
                table_two_address => columns(SINGLE_LINE_ADDRESS_LOOKUP) 
            )
    ) m
    on        m.SINGLE_LINE_ADDRESS = pat.SINGLE_LINE_ADDRESS
    left join lookup_address_temp l
    on        l.SINGLE_LINE_ADDRESS = m.SINGLE_LINE_ADDRESS_LOOKUP
    ; 
  "
)

# Using dbSendQuery requires a separate call to clear result.
dbClearResult(query)

# Drop these now as no longer needed
drop_table_if_exists_db("PATIENT_ADDRESS_TEMP")
drop_table_if_exists_db("LOOKUP_ADDRESS_TEMP")

# Get connection to temporary match table
match_db <- con %>%
  tbl(from = "MATCH_TEMP")

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
    SINGLE_LINE_ADDRESS_STANDARDISED = max(
      paste0(SINGLE_LINE_ADDRESS, " ", POSTCODE)
    )
  ) %>% 
  ungroup() %>% 
  # Ensure 1 uprn per SLA
  group_by(SINGLE_LINE_ADDRESS_STANDARDISED) %>%
  mutate(UPRN = max(UPRN)) %>% 
  ungroup() %>% 
  # Get parent uprn info
  left_join(parent_db, by = "PARENT_UPRN") %>%
  verify(nrow.alt(.) == nrow.alt(patient_address_db))

# Join the matches back to the patient addresses
patient_match_db <- patient_db %>%
  filter(
    !is.na(SINGLE_LINE_ADDRESS),
    CALC_AGE >= 65,
    POSTCODE_CH == 1
  ) %>%
  # verify(nrow.alt(.) > 15 * million) %>% # TEMP REMOVAL WHILE CHECKING E2E PIPELINE
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
table_name = match_tbl

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

########## TEMP CHECKING ##########
if(!is.null(pc_sample)) {
  con %>%
    tbl(from = table_name) %>%
    # Limit data to given postcodes
    assert.alt(
      is_in.alt,
      POSTCODE,
      pred_args = list(.in = pc_sample_f)
    )
}
###################################

# Can now drop the temporary match table
drop_table_if_exists_db("MATCH_TEMP")

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
