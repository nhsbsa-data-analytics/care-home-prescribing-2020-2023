
# Load packages and global variables
source("R/analysis_packages.R")
source("R/workflow_helpers.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table addressbase data
ab_plus_cqc_db <- con %>%
  tbl(from = "INT646_AB_PLUS_CQC_STACK")

# Create a lazy table from year month dim table in DWCP
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the matched patient address care home table
match_db <- con %>%
  tbl(from = "INT646_UPRN_MATCH")

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM"))

# Lazy table for paper info
paper_db <- con %>%
  tbl(from = in_schema("DALL_REF", "PX_PAPER_PFID_ADDRESS"))

# Lazy table for fact table
eps_db <- con %>%
  tbl(from = in_schema("SCD2", "SCD2_ETP_DY_PAYLOAD_MSG_DATA"))

# Define start and end dates
start_date = "2021-04-01"
end_date = "2022-03-31"

# Derive start and end year months
start_year_month = get_year_month_from_date(start_date)
end_year_month = get_year_month_from_date(end_date)

# Define 'buffered' eps date range: for query efficiency
eps_start_date = as.Date(start_date) %m-% months(2)
eps_end_date = (as.Date(end_date)+10) %m+% months(2)

# Start and end date as integers
eps_start_int = get_integer_from_date(eps_start_date)
eps_end_int = get_integer_from_date(eps_end_date)

# Define keyword list for case statements
care_home_keywords = "CARE HOME|CARE-HOME|NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME"
global_exclusion_keywords = "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT"
extra_exclusion_keywords = "CONVENT|HOSPITAL|MARINA|MONASTERY|RECOVERY"

# Part one: non ch postcodes, paper addresses ----------------------------------

# Get appropriate year month fields as a table
year_month_db = year_month_db %>% 
  select(YEAR_MONTH) %>% 
  filter(
    YEAR_MONTH >= start_year_month,
    YEAR_MONTH <= end_year_month
  )

# Filter to elderly patients in 2020/2021 and required columns
fact_db = fact_db %>%
  inner_join(year_month_db) %>% 
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
    ITEM_CALC_PAY_QTY
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
    BNF_CHEMICAL_SUBSTANCE
  )

# Process paper info
paper_plus_match_db = paper_db %>% 
  inner_join(year_month_db) %>% 
  anti_join(match_db %>% select(YEAR_MONTH, PF_ID)) %>% 
  addressMatchR::tidy_postcode(col = POSTCODE) %>% 
  addressMatchR::tidy_single_line_address(col = ADDRESS) %>% 
  select(
    YEAR_MONTH,
    PF_ID,
    SINGLE_LINE_ADDRESS = ADDRESS,
    POSTCODE
  ) %>% 
  mutate(
    AB_FLAG = 0,
    UPRN_FLAG = 0,
    CH_FLAG = case_when(
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, care_home_keywords) > 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, extra_exclusion_keywords) == 0L ~ 1,
        TRUE ~ 0
    ),
    MATCH_TYPE = ifelse(CH_FLAG == 1, "SINGLE_KEYWORD", "NO MATCH")
  ) %>% 
  union_all(match_db) %>% 
  inner_join(fact_db, by = c("YEAR_MONTH", "PF_ID")) %>% 
  left_join(y = drug_db, by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID")) %>% 
  # Replace nas
  tidyr::replace_na(
    list(
      AB_FLAG = 0L,
      UPRN_FLAG = 0L,
      CH_FLAG = 0L, 
      MATCH_TYPE = "NO MATCH"
    )
  ) %>% 
  # Reorder drug information
  relocate(
    CHAPTER_DESCR:BNF_CHEMICAL_SUBSTANCE,
    .before = CALC_PREC_DRUG_RECORD_ID
  ) %>%
  select(-CALC_PREC_DRUG_RECORD_ID)

# Part two: non ch postcodes eps addresses -------------------------------------

# Proces eps info
eps_db = eps_db %>%
  # Bring back ETP data
  filter(
    PART_DATE >= eps_start_int,
    PART_DATE <= eps_end_int
  ) %>% 
  # Concatenate fields together by a single space for the single line address
  mutate(
    SINGLE_LINE_ADDRESS = paste(
      PAT_ADDRESS_LINE1,
      PAT_ADDRESS_LINE2,
      PAT_ADDRESS_LINE3,
      PAT_ADDRESS_LINE4
    )
  ) %>%
  # Tidy postcode and format single line addresses
  addressMatchR::tidy_postcode(col = PAT_ADDRESS_POSTCODE) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS) %>% 
  select(
    YEAR_MONTH,
    NHS_NO,
    PF_ID,
    SINGLE_LINE_ADDRESS,
    POSTCODE = PAT_ADDRESS_POSTCODE
  ) %>% 
  mutate(
    AB_FLAG = 0,
    UPRN_FLAG = 0,
    CH_FLAG = case_when(
      REGEXP_INSTR(SINGLE_LINE_ADDRESS, care_home_keywords) > 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, global_exclusion_keywords) == 0L &
        REGEXP_INSTR(SINGLE_LINE_ADDRESS, extra_exclusion_keywords) == 0L ~ 1,
      TRUE ~ 0
    ),
    MATCH_TYPE = ifelse(CH_FLAG == 1, "SINGLE_KEYWORD", "NO MATCH")
  )

fact_eps_db = fact_item_db %>% 
  filter(EPS_FLAG == "YES") %>% 
  anti_join(match_db %>% select(YEAR_MONTH, PF_ID)) %>% 
  inner_join(eps_info_db, by = c("EPM_ID", "PART_DATE")) %>% 
  left_join(y = drug_db, by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID")) %>% 
  # Replace nas
  tidyr::replace_na(
    list(
      AB_FLAG = 0L,
      UPRN_FLAG = 0L,
      CH_FLAG = 0L, 
      MATCH_TYPE = "NO MATCH"
    )
  ) %>% 
  # Reorder drug information
  relocate(
    CHAPTER_DESCR:BNF_CHEMICAL_SUBSTANCE,
    .before = CALC_PREC_DRUG_RECORD_ID
  ) %>%
  select(-CALC_PREC_DRUG_RECORD_ID)
  

# Part three: stack data and generate consistent info --------------------------

# Union paper and eps info
fact_stack_db = fact_eps_db %>% 
  union_all(paper_plus_match_db)

# Get a single gender and age for the period
patient_db <- fact_stack_db %>%
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
    )
  ) %>%
  select(-ends_with("_COUNT")) %>%
  # Add an age band
  mutate(
    AGE_BAND = case_when(
      AGE < 70 ~ "65-69",
      AGE < 75 ~ "70-74",
      AGE < 80 ~ "75-79",
      AGE < 85 ~ "80-84",
      AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  )

# Join fact data to patient level dimension
fact_stack_db = fact_stack_db %>%
  left_join(y = patient_db) %>%
  relocate(GENDER, .after = PDS_GENDER) %>%
  relocate(AGE_BAND, AGE, .after = CALC_AGE) %>%
  select(-c(PDS_GENDER, CALC_AGE))

# Part three: save output ------------------------------------------------------

# Define table name
#table_name = name = "INT646_UPRN_BASE_TABLE"

# Remove table if exists
#drop_table_if_exists_db(table_name)

# Write the table back to DALP
tic()
fact_item_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )
toc()

# Grant access
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MIGAR"))

# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()

#-------------------------------------------------------------------------------