source("R/utils_helpers.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

million <- 10^6

# Create a lazy table from year month dim table in DWCP
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the item level FACT table
pat_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the matched patient address care home table
match_db <- con %>%
  tbl(from = match_tbl)

# Create a lazy table from the matched patient address care home table
form_db <- con %>%
  tbl(from = patient_tbl)

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM"))

# Lazy table for prescriber table
presc_db <- con %>%
  tbl(from = in_schema("DIM", "CUR_HS_EP_ORG_UNIT_DIM"))

# Lazy table for dispenser table
disp_db <- con %>%
  tbl(from = in_schema("DIM", "HS_DY_LEVEL_5_FLAT_DIM"))

# Lazy table from the geography lookup table for appropriate FY
postcode_db <- con %>%
  tbl(from = "INT646_POSTCODE_LOOKUP")

# Get start and end dates
# NOTE: The existing variables can be used here instead of recalculating
# start_date = stringr::str_extract_all(match_tbl, "\\d{8}")[[1]][1]
# end_date = stringr::str_extract_all(match_tbl, "\\d{8}")[[1]][2]

# Derive start and end year months
start_year_month = as.integer(substr(start_str, 1, 6))
end_year_month = as.integer(substr(end_str, 1, 6))

# Define 'buffered' eps date range: for query efficiency
eps_start_date = as.Date(start_date, format = "%Y%m%d") %m-% months(2)
eps_end_date = (as.Date(end_date, format = "%Y%m%d")+10) %m+% months(2)

# Start and end date as integers
eps_start_int = get_integer_from_date(eps_start_date)
eps_end_int = get_integer_from_date(eps_end_date)

# Get year_month vector
year_month = year_month_db %>% 
  inner_join(year_month_db,  by = "YEAR_MONTH") %>% 
  select(YEAR_MONTH) %>% 
  filter(
    YEAR_MONTH >= start_year_month,
    YEAR_MONTH <= end_year_month
  ) %>% 
  pull()

# Define keyword list for case statements
care_home_keywords = "CARE HOME|CARE-HOME|NURSING HOME|NURSING-HOME|RESIDENTIAL HOME|RESIDENTIAL-HOME|REST HOME|REST-HOME"
global_exclusion_keywords = "ABOVE|CARAVAN|CHILDREN|HOLIDAY|MOBILE|NO FIXED ABODE|RESORT"
extra_exclusion_keywords = "CONVENT|HOSPITAL|MARINA|MONASTERY|RECOVERY"

# Part one: prepare tables prior to left join onto fact ------------------------

# Match info minus nhs no
match_db = match_db %>% 
  select(
    YEAR_MONTH_MATCH = YEAR_MONTH,
    PF_ID_MATCH = PF_ID,
    MATCH_SLA = SINGLE_LINE_ADDRESS_LOOKUP,
    MATCH_SLA_STD = SINGLE_LINE_ADDRESS_STANDARDISED,
    MATCH_SLA_PARENT = SINGLE_LINE_ADDRESS_PARENT,
    MATCH_TYPE,
    SCORE,
    MAX_MONTHLY_PATIENTS,
    AB_FLAG,
    CH_FLAG,
    UPRN_FLAG,
    UPRN,                         
    PARENT_UPRN,
    LOCATION_ID,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    AB_DATE,
    CQC_DATE
  ) %>% 
  assert.alt(not_na.alt, PF_ID_MATCH) %>% 
  assert.alt(is_uniq.alt, PF_ID_MATCH)

# Filter to elderly patients in current year and required columns
fact_db = fact_db %>%
  filter(
    CALC_AGE >= 65L,
    YEAR_MONTH %in% year_month,
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
    PAY_DRUG_RECORD_ID,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY,
    PRESC_ID_PRNT,
    PRESC_TYPE_PRNT,
    PRESC_PD_ID,
    PRESC_PD_OUPDT,
    DISP_CODE = DISPENSER_CODE,
    DISP_ID,
    DISP_OUPDT_TYPE
  ) %>%
  verify(nrow.alt(.) > 500 * million)
  

# Get drug info
drug_db = drug_db %>% 
  filter(YEAR_MONTH %in% year_month) %>% 
  mutate(
    CHAPTER_1_4_6_10_CAT = case_when(
      as.integer(
        substr(BNF_CHEMICAL_SUBSTANCE, 1, 2)
      ) %in% c(1:4, 6:10) ~ 1,
      TRUE ~ 0
    ),
    ACB_CAT = case_when(
      BNF_CHEMICAL_SUBSTANCE %in% acb_drugs ~ 1,
      TRUE ~ 0
    ),
    DAMN_CAT = case_when(
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^100101') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205051') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205052') > 0 ~ 1,
      BNF_CHEMICAL_SUBSTANCE %in% other_drug_vec ~ 1,
      TRUE ~ 0
    ),
    FALLS_CAT = case_when(
      (SECTION_DESCR %in% falls_section_vec |
         PARAGRAPH_DESCR %in% falls_paragraph_vec |
         CHEMICAL_SUBSTANCE_BNF_DESCR %in% falls_chem_vec) &
        !CHEMICAL_SUBSTANCE_BNF_DESCR %in% falls_exclude_chem_vec ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(
    YEAR_MONTH,
    PAY_DRUG_RECORD_ID = RECORD_ID,
    CHAPTER_DESCR,
    SECTION_DESCR,
    PARAGRAPH_DESCR,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE,
    BASE_NAME,
    CHAPTER_1_4_6_10_CAT,
    ACB_CAT,
    DAMN_CAT,
    FALLS_CAT
  ) %>%
  verify(nrow.alt(.) > 3 * million) %>%
  # Checking combination of cols is unique requires merging to a single column
  mutate(YM_RECORD_ID = paste(YEAR_MONTH, PAY_DRUG_RECORD_ID)) %>% 
  assert.alt(is_uniq.alt, YM_RECORD_ID) %>% 
  select(-YM_RECORD_ID)

# Process prescriber information
presc_db = presc_db %>% 
  filter(YEAR_MONTH %in% year_month) %>%
  select(
    YEAR_MONTH,
    LVL_5_OU,
    LVL_5_OUPDT,
    PD_CDE,
    PD_OUPDT,
    PRESC_SLA = LVL_5_HIST_FULL_ADDRESS,
    PRESC_POSTCODE = LVL_5_HIST_POSTCODE,
    PRESC_ORG_TYPE = LVL_5_LTST_TYPE,
    PRESC_ORG_SUB_TYPE = PRCTC_TYPE_HIST_IND_DESC,
    PRESC_ORG_NM = LVL_5_LTST_NM,
    PRESC_ORG_CODE = LVL_5_LTST_ALT_CDE,
    PRESC_ORG_LIST_SIZE = HS_TOTAL_LIST_SIZE,
    PRESCRIBER_TYPE = PRESCRIBER_LTST_TYPE,
    PRESCRIBER_SUB_TYPE = PRESCRIBER_LTST_SUB_TYPE,
    PRESCRIBER_NM = PRESCRIBER_LTST_NM,
    PRESCRIBER_CODE = PRESCRIBER_LTST_CDE
  ) %>%
  verify(nrow.alt(.) > 2 * million) %>% 
  # Checking combination of cols is unique requires merging to a single column
  mutate(
    YM_OU_PD = paste(
      YEAR_MONTH,
      LVL_5_OU,
      LVL_5_OUPDT,
      PD_CDE,
      PD_OUPDT
    )
  ) %>% 
  assert.alt(is_uniq.alt, YM_OU_PD) %>% 
  select(-YM_OU_PD)

# Process form fact
# NOTE: End up with around 100k NA postcodes/125k NA SLAs for 20/21.
# Should these be removed?
form_db = form_db %>% 
  select(
    YEAR_MONTH_FORMS = YEAR_MONTH,
    PF_ID_FORMS = PF_ID,
    BSA_POSTCODE = POSTCODE,
    BSA_SLA = SINGLE_LINE_ADDRESS
  ) %>% 
  assert.alt(is_uniq.alt, PF_ID_FORMS) %>% 
  assert.alt(not_na.alt, PF_ID_FORMS)

# Process Dispenser data
disp_db = disp_db %>% 
  filter(YEAR_MONTH %in% year_month) %>%
  mutate(
    DISP_TYPE = case_when(
      DIST_SELLING_DISPENSER_HIST == "Y" ~ "PHARMACY CONTRACTOR: DISTANCE SELLING",
      LPS_DISPENSER_HIST == "Y" ~ "PHARMACY CONTRACTOR: LPS",
      APPLIANCE_DISPENSER_HIST == "Y" ~ "PHARMACY CONTRACTOR: APPLIANCE",
      OOH_DISPENSER_HIST == "Y" ~ "PHARMACY CONTRACTOR: OOH",
      PRIVATE_DISPENSER_HIST == "Y" ~ "PHARMACY CONTRACTOR: PRIVATE",
      T ~ LVL_5_LTST_TYPE
    )
  ) %>% 
  select(
    YEAR_MONTH,
    LVL_5_OU,
    LVL_5_OUPDT,
    DISP_TYPE,
    DISP_NM = LVL_5_LTST_NM,
    DISP_TRADING_NM = TRADING_LTST_NM,
    DISP_SLA = LVL_5_HIST_FULL_ADDRESS,
    DISP_POSTCODE = LVL_5_HIST_POSTCODE
  ) %>% 
  verify(nrow.alt(.) > 1 * million) %>% 
  assert.alt(not_na.alt, LVL_5_OU) %>% 
  assert.alt(not_na.alt, LVL_5_OUPDT) %>%
  # Checking combination of cols is unique requires merging to a single column
  mutate(
    YM_OU = paste(
      YEAR_MONTH,
      LVL_5_OU,
      LVL_5_OUPDT
    )
  ) %>% 
  assert.alt(is_uniq.alt, YM_OU) %>% 
  select(-YM_OU)


# Get a single latest gender and age for the period 
pat_db <- pat_db %>% 
  filter(
    CALC_AGE >= 65L,
    YEAR_MONTH %in% year_month,
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
  mutate(PRESCRIBED_DATE_NA = ifelse(is.na(PRESCRIBED_DATE), 0, 1)) %>% 
  group_by(NHS_NO) %>%
  window_order(
    desc(YEAR_MONTH),
    # For YMs where EPS & Paper forms occur together,
    # take the value as of the latest known EPS prescribed date,
    # if not available, take the value from an arbitrary paper form
    # with missing prescribed date
    desc(PRESCRIBED_DATE_NA),
    desc(PRESCRIBED_DATE)) %>%
  mutate(
    RN = row_number(),
    AGE = max(CALC_AGE, na.rm = TRUE)
  ) %>%
  filter(RN == 1) %>%
  ungroup() %>% 
  mutate(
    GENDER = case_when(
      PDS_GENDER == 1 ~ "Male",
      PDS_GENDER == 2 ~ "Female",
      PDS_GENDER == 0 ~ "Unknown",
      PDS_GENDER == 9 ~ "Indeterminate",
      TRUE ~ NA_character_
    ),
    # Add an age band
    AGE_BAND = case_when(
      AGE == -1 ~ "UNKNOWN",
      AGE < 65 ~ "<65",
      AGE < 70 ~ "65-69",
      AGE < 75 ~ "70-74",
      AGE < 80 ~ "75-79",
      AGE < 85 ~ "80-84",
      AGE < 90 ~ "85-89",
      TRUE ~ "90+"
    )
  ) %>%
  select(NHS_NO, GENDER, AGE, AGE_BAND) %>%
  verify(nrow.alt(.) > 8 * million) %>% 
  assert.alt(is_uniq.alt, NHS_NO) %>% 
  assert.alt(not_na.alt, NHS_NO)

# Part two: multiple left joins, coalesce and identify new keyword matches -----

# Join all tables onto fact then process
fact_join_db = fact_db %>% 
  left_join(y = form_db, by = c("YEAR_MONTH" = "YEAR_MONTH_FORMS", 
                                "PF_ID" = "PF_ID_FORMS")) %>% 
  left_join(y = match_db, by = c("YEAR_MONTH" = "YEAR_MONTH_MATCH", 
                                 "PF_ID" = "PF_ID_MATCH")) %>% 
  left_join(y = drug_db, by = c("YEAR_MONTH", "PAY_DRUG_RECORD_ID")) %>% 
  left_join(y = pat_db, by = c("NHS_NO")) %>% 
  left_join(y = presc_db, by = c("YEAR_MONTH" = "YEAR_MONTH",
                                 "PRESC_ID_PRNT" = "LVL_5_OU",
                                 "PRESC_TYPE_PRNT" = "LVL_5_OUPDT",
                                 "PRESC_PD_ID" = "PD_CDE",
                                 "PRESC_PD_OUPDT" = "PD_OUPDT")) %>% 
  left_join(y = disp_db, by = c("YEAR_MONTH",
                                "DISP_ID" = "LVL_5_OU",
                                "DISP_OUPDT_TYPE" = "LVL_5_OUPDT")) %>%
  # Inner join so only prescription forms with an *English* postcode are included
  inner_join(y = postcode_db, by = c("BSA_POSTCODE" = "POSTCODE")) %>%
  mutate(
    # Apply single keyword logic to addresses that haven't been used in matching, 
    # because they don't share a postcode with a known carehome
    CH_FLAG = case_when(
      is.na(AB_FLAG) &
      REGEXP_INSTR(BSA_SLA, care_home_keywords) > 0L &
      REGEXP_INSTR(BSA_SLA, global_exclusion_keywords) == 0L &
      REGEXP_INSTR(BSA_SLA, extra_exclusion_keywords) == 0L ~ 1,
      TRUE ~ CH_FLAG
    ),
    MATCH_TYPE = case_when(
      is.na(AB_FLAG) & CH_FLAG == 1 ~ "SINGLE_KEYWORD",
      TRUE ~ MATCH_TYPE
    ),
    #Zeroes for nas after left-join
    AB_FLAG = case_when(is.na(AB_FLAG) ~ 0, T ~ AB_FLAG),
    UPRN_FLAG = case_when(is.na(UPRN_FLAG) ~ 0, T ~ UPRN_FLAG),
    CH_FLAG = case_when(is.na(CH_FLAG) ~ 0, T ~ CH_FLAG),
    CH_FLAG = case_when(
      RESIDENTIAL_HOME_FLAG == 1 | NURSING_HOME_FLAG == 1 ~ 1,
      TRUE ~ CH_FLAG
    ),
    MATCH_TYPE = case_when(is.na(MATCH_TYPE) ~ "NO MATCH", T ~ MATCH_TYPE)
  ) %>%
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
    BSA_SLA,
    MATCH_SLA,
    MATCH_SLA_STD,
    MATCH_SLA_PARENT,
    MATCH_TYPE,
    SCORE,
    MAX_MONTHLY_PATIENTS,
    AB_FLAG,
    UPRN_FLAG,
    CH_FLAG,
    # AB and CQC info
    UPRN,
    PARENT_UPRN,
    LOCATION_ID,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    AB_DATE,
    CQC_DATE,
    # Item info
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    ITEM_CALC_PAY_QTY,
    # Drug info
    PAY_DRUG_RECORD_ID,
    CHAPTER_DESCR,
    SECTION_DESCR,
    PARAGRAPH_DESCR,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE,
    BASE_NAME,
    CHAPTER_1_4_6_10_CAT,
    ACB_CAT,
    DAMN_CAT,
    FALLS_CAT,
    # Prescriber info
    PRESC_SLA,
    PRESC_POSTCODE,
    PRESC_ORG_TYPE,
    PRESC_ORG_SUB_TYPE,
    PRESC_ORG_NM,
    PRESC_ORG_CODE,
    PRESC_ORG_LIST_SIZE,
    PRESCRIBER_TYPE,
    PRESCRIBER_SUB_TYPE,
    PRESCRIBER_NM,
    PRESCRIBER_CODE,
    # Dispenser info
    DISP_CODE,
    DISP_TYPE,
    DISP_NM,
    DISP_TRADING_NM,
    DISP_SLA,
    DISP_POSTCODE,
    # Geographic attributes
    PCD_REGION_CODE,
    PCD_REGION_NAME,
    PCD_ICB_CODE,
    PCD_ICB_NAME,
    PCD_LAD_CODE,
    PCD_LAD_NAME,
    IMD_DECILE
  )

# Part four: save output -------------------------------------------------------

# Define table name
table_name = paste0("INT646_BASE_", start_str, "_", end_str)

# Remove table if exists
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to DALP
fact_join_db %>% compute_with_parallelism(table_name, 32)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% grant_table_access (table_name)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
