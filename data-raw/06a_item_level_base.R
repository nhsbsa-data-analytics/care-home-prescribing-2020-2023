
# Load packages and global variables
source("R/analysis_packages.R")
source("R/workflow_helpers.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Create a lazy table from the item level FACT table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Create a lazy table from the item level FACT table
form_db <- con %>%
  tbl(from = "INT646_FORM_LEVEL_FACT")

# Create a lazy table from the matched patient address care home table
match_db <- con %>%
  tbl(from = "INT646_UPRN_MATCH")

# Create a lazy table from the drug DIM table
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM"))

# Define start and end dates
start_date = "2021-04-01"
end_date = "2022-03-31"

# Derive start and end year months
start_year_month = get_year_month_from_date(start_date)
end_year_month = get_year_month_from_date(end_date)

# Create item level FACT table -------------------------------------------------

# Get relevant pf ids
form_db = form_db %>% 
  select(PF_ID)

# Get appropriate year month fields as a table
year_month_db = year_month_db %>% 
  select(YEAR_MONTH) %>% 
  filter(
    YEAR_MONTH >= start_year_month,
    YEAR_MONTH <= end_year_month
  )

# Get drug info
drug_db = drug_db %>% 
  inner_join(year_month_db) %>% 
  select(
    YEAR_MONTH,
    CALC_PREC_DRUG_RECORD_ID = RECORD_ID,
    CHAPTER_DESCR,
    SECTION_DESCR,
    PARAGRAPH_DESCR,
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    BNF_CHEMICAL_SUBSTANCE
  )

# Filter to elderly patients in 2020/2021 and required columns
fact_item_db <- fact_db %>%
  inner_join(year_month_db) %>% 
  inner_join(form_db) %>% 
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
  ) %>% 
  # Rejoin back to original table and fill in care home flag
  left_join(y = match_db, by = "PF_ID") %>%
  tidyr::replace_na(list(CH_FLAG = 0L, MATCH_TYPE = "NO MATCH")) %>% 
  # Add the drug information
  left_join(y = drug_db, by = c("YEAR_MONTH", "CALC_PREC_DRUG_RECORD_ID")) %>% 
  # Reorder drug information
  relocate(
    CHAPTER_DESCR:BNF_CHEMICAL_SUBSTANCE,
    .before = CALC_PREC_DRUG_RECORD_ID
  ) %>%
  select(-CALC_PREC_DRUG_RECORD_ID)

# Get a single gender and age for the period
patient_db <- fact_item_db %>%
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
fact_item_db <- fact_item_db %>%
  left_join(y = patient_db) %>%
  relocate(GENDER, .after = PDS_GENDER) %>%
  relocate(AGE_BAND, AGE, .after = CALC_AGE) %>%
  select(-c(PDS_GENDER, CALC_AGE))

# Define table name
table_name = name = "INT646_ITEM_LEVEL_BASE"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Write the table back to DALP
item_fact_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()