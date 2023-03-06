
# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the matched patient address care home table
match_db <- con %>%
  tbl(from = match_data)

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM"))

# Lazy table for paper info
paper_db <- con %>%
  tbl(from = in_schema("DALL_REF", "PX_PAPER_PFID_ADDRESS"))

# Lazy table for fact table
eps_db <- con %>%
  tbl(from = in_schema("SCD2", "SCD2_ETP_DY_PAYLOAD_MSG_DATA"))

# Lazy table for fact table
presc_db <- con %>%
  tbl(from = in_schema("DIM", "CUR_HS_EP_ORG_UNIT_DIM"))

# Get start and end dates
start_date = stringr::str_extract_all(match_data, "\\d{8}")[[1]][1]
end_date = stringr::str_extract_all(match_data, "\\d{8}")[[1]][2]

# Derive start and end year months
start_year_month = as.integer(substr(start_date, 1, 6))
end_year_month = as.integer(substr(end_date, 1, 6))

# Define 'buffered' eps date range: for query efficiency
eps_start_date = as.Date(start_date, format = "%Y%m%d") %m-% months(2)
eps_end_date = (as.Date(end_date, format = "%Y%m%d")+10) %m+% months(2)

# Start and end date as integers
eps_start_int = get_integer_from_date(eps_start_date)
eps_end_int = get_integer_from_date(eps_end_date)

# Define keyword list for case statements
care_home_keywords = "CARE HOME|CARE-HOME|NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME"
global_exclusion_keywords = "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT"
extra_exclusion_keywords = "CONVENT|HOSPITAL|MARINA|MONASTERY|RECOVERY"

# Part one: prepare tables prior to left join onto fact ------------------------

# Get appropriate year month fields as a table
year_month_db = year_month_db %>% 
  select(YEAR_MONTH) %>% 
  filter(
    YEAR_MONTH >= start_year_month,
    YEAR_MONTH <= end_year_month
  )

# Match info minus nhs no
match_db = match_db %>% 
  select(-NHS_NO) %>% 
  rename(
    MATCH_POSTCODE = POSTCODE,
    MATCH_SINGLE_LINE_ADDRESS = SINGLE_LINE_ADDRESS
  )

# Filter to elderly patients in 2020/2021 and required columns
fact_db = fact_db %>%
  inner_join(year_month_db,  by = "YEAR_MONTH") %>% 
  filter(
    CALC_AGE >= 65L,
    PATIENT_IDENTIFIED == "Y",
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N", # remove dummy ldp forms
    ITEM_COUNT >= 1 # remove element-level rows
  ) %>%
  select(
    YEAR_MONTH,
    PF_ID,
    EPS_FLAG,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PDS_GENDER,
    CALC_AGE,
    PATIENT_IDENTIFIED,
    NHS_NO,
    CALC_PREC_DRUG_RECORD_ID,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY,
    PRESC_ID_PRNT,
    PRESC_TYPE_PRNT,
    PRESC_PD_ID,
    PRESC_PD_OUPDT
  )

# Get drug info
drug_db = drug_db %>% 
  inner_join(year_month_db, by = "YEAR_MONTH") %>% 
  select(
    YEAR_MONTH,
    CALC_PREC_DRUG_RECORD_ID = RECORD_ID,
    CHAPTER_DESCR,
    SECTION_DESCR,
    PARAGRAPH_DESCR,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE,
    BASE_NAME
  )

# Process prescriber information
presc_db = presc_db %>% 
  inner_join(year_month_db, by = "YEAR_MONTH") %>% 
  select(
    YEAR_MONTH,
    LVL_5_LTST_TYPE,
    PRCTC_TYPE_HIST_IND_DESC,
    LVL_5_LTST_NM,
    LVL_5_LTST_ALT_CDE,
    HS_TOTAL_LIST_SIZE,
    PRESCRIBER_LTST_TYPE,
    PRESCRIBER_LTST_SUB_TYPE,
    PRESCRIBER_LTST_NM,
    PRESCRIBER_LTST_CDE,
    LVL_5_OU,
    LVL_5_OUPDT,
    PD_CDE,
    PD_OUPDT
  )

# Process paper info
paper_db = paper_db %>% 
  inner_join(year_month_db,  by = "YEAR_MONTH") %>% 
  addressMatchR::tidy_postcode(col = POSTCODE) %>% 
  addressMatchR::tidy_single_line_address(col = ADDRESS) %>% 
  select(
    YEAR_MONTH,
    PF_ID,
    PAPER_SINGLE_LINE_ADDRESS = ADDRESS,
    PAPER_POSTCODE = POSTCODE
  )

# Process eps info
eps_db = eps_db %>%
  # Bring back ETP data
  filter(
    PART_DATE >= eps_start_int,
    PART_DATE <= eps_end_int
  ) %>% 
  # Concatenate fields together by a single space for the single line address
  mutate(
    EPS_SINGLE_LINE_ADDRESS = paste(
      PAT_ADDRESS_LINE1,
      PAT_ADDRESS_LINE2,
      PAT_ADDRESS_LINE3,
      PAT_ADDRESS_LINE4
    )
  ) %>%
  # Tidy postcode and format single line addresses
  addressMatchR::tidy_postcode(col = PAT_ADDRESS_POSTCODE) %>%
  addressMatchR::tidy_single_line_address(col = EPS_SINGLE_LINE_ADDRESS) %>% 
  select(
    EPM_ID,
    PART_DATE,
    EPS_SINGLE_LINE_ADDRESS,
    EPS_POSTCODE = PAT_ADDRESS_POSTCODE
  )

# Part two: multiple left joins, coalesce and identify new keyword matches -----

# Join all tables onto fact then process
fact_join_db = fact_db %>% 
  left_join(y = match_db, by = c("YEAR_MONTH", "PF_ID")) %>% 
  left_join(y = paper_db, by = c("YEAR_MONTH", "PF_ID")) %>% 
  left_join(y = eps_db, by = c("EPM_ID", "PART_DATE")) %>% 
  left_join(y = drug_db, by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID")) %>% 
  left_join(y = presc_db, by = c("YEAR_MONTH" = "YEAR_MONTH",
                                 "PRESC_ID_PRNT" = "LVL_5_OU",
                                 "PRESC_TYPE_PRNT" = "LVL_5_OUPDT",
                                 "PRESC_PD_ID" = "PD_CDE",
                                 "PRESC_PD_OUPDT" = "PD_OUPDT")) %>% 
  mutate(
    BSA_POSTCODE = coalesce(
      EPS_POSTCODE, 
      PAPER_POSTCODE
      ),
    BSA_SINGLE_LINE_ADDRESS = coalesce(
      EPS_SINGLE_LINE_ADDRESS,
      PAPER_SINGLE_LINE_ADDRESS
      ),
    
    # Apply single keyword logic to addresses that haven't been used in matching, 
    # because they don't share a postcode with a known carehome
    CH_FLAG = case_when(
      is.na(AB_FLAG) &
      REGEXP_INSTR(BSA_SINGLE_LINE_ADDRESS, care_home_keywords) > 0L &
      REGEXP_INSTR(BSA_SINGLE_LINE_ADDRESS, global_exclusion_keywords) == 0L &
      REGEXP_INSTR(BSA_SINGLE_LINE_ADDRESS, extra_exclusion_keywords) == 0L ~ 1,
      TRUE ~ CH_FLAG
      ),
    MATCH_TYPE = case_when(
      is.na(AB_FLAG) & CH_FLAG == 1 ~ "SINGLE_KEYWORD",
      TRUE ~ "NO MATCH"
      ),
    AB_FLAG = ifelse(is.na(AB_FLAG), 0, AB_FLAG),
    UPRN_FLAG = ifelse(is.na(UPRN_FLAG), 0, UPRN_FLAG)
    ) |>
  select(-PAPER_POSTCODE,
         -PAPER_SINGLE_LINE_ADDRESS,
         -EPS_POSTCODE,
         -EPS_SINGLE_LINE_ADDRESS)
  
# Part three: generate consistent patient info ---------------------------------

# Get a single gender and age for the period
patient_db <- fact_join_db %>%
  group_by(NHS_NO) %>%
  summarise(
    # Gender
    MALE_COUNT = sum(
      ifelse(PDS_GENDER == 1, 1, 0),
      na.rm = TRUE
    ),
    FEMALE_COUNT = sum(
      ifelse(PDS_GENDER == 2, 1, 0),
      na.rm = TRUE
    ),
    # Take the max age
    AGE = max(
      CALC_AGE,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(
    GENDER = case_when(
      MALE_COUNT > 0 & FEMALE_COUNT == 0 ~ "Male",
      MALE_COUNT == 0 & FEMALE_COUNT > 0 ~ "Female",
      TRUE ~ NA_character_
    ),
    # Add an age band
    AGE_BAND = case_when(
      AGE < 70 ~ "65-69",
      AGE < 75 ~ "70-74",
      AGE < 80 ~ "75-79",
      AGE < 85 ~ "80-84",
      AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  ) %>%
  select(-ends_with("_COUNT"))

# Join fact data to patient level dimension
fact_join_db = fact_join_db %>%
  left_join(y = patient_db, by = c("NHS_NO")) %>% 
  select(
    # Fact metadata
    YEAR_MONTH,
    PART_DATE,
    EPM_ID,
    PF_ID,
    EPS_FLAG,
    # Patient info
    NHS_NO, 
    GENDER,
    AGE,
    AGE_BAND,
    # Match Info
    BSA_POSTCODE,
    MATCH_POSTCODE,
    BSA_SINGLE_LINE_ADDRESS,
    MATCH_SINGLE_LINE_ADDRESS,
    MATCH_TYPE,
    SCORE,
    MAX_MONTHLY_PATIENTS,
    AB_FLAG,
    UPRN_FLAG,
    CH_FLAG,
    # AB and CQC info
    UPRN,
    LOCATION_ID,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    NUMBER_OF_BEDS,
    CURRENT_RATING,
    AB_DATE,
    CQC_DATE,
    # Item info
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY,
    # Drug info
    CALC_PREC_DRUG_RECORD_ID,
    CHAPTER_DESCR,
    SECTION_DESCR,
    PARAGRAPH_DESCR,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE,
    BASE_NAME,
    # Prescriber info
    LVL_5_LTST_TYPE,
    PRCTC_TYPE_HIST_IND_DESC,
    LVL_5_LTST_NM,
    LVL_5_LTST_ALT_CDE,
    HS_TOTAL_LIST_SIZE,
    PRESCRIBER_LTST_TYPE,
    PRESCRIBER_LTST_SUB_TYPE,
    PRESCRIBER_LTST_NM,
    PRESCRIBER_LTST_CDE
  )

# Part four: save output -------------------------------------------------------

# Define table name
table_name = paste0("INT646_BASE_", start_date, "_", end_date)

# Remove table if exists
drop_table_if_exists_db(table_name)

# Write the table back to DALP
fact_join_db %>%
  compute(
    name = table_name,
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
