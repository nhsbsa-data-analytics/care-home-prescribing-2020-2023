# Libraries
library(dplyr)
library(dbplyr)
library(highcharter)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
data <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

# Get chapter info
chapters = con %>% 
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM")) %>% 
  filter(YEAR_MONTH == 202201) %>% 
  select(BNF_CHAPTER, CHAPTER_DESCR) %>% 
  distinct() %>% 
  collect() %>% 
  filter(BNF_CHAPTER %in% c('01','02','03','04','06','07','08','09','10')) %>% 
  select(CHAPTER_DESCR) %>% 
  pull()

# Get column names
cols = data.frame(COL = names(data))

# High-level figures
df_overall = data %>% 
  filter(UPRN_FLAG == 1) %>% 
  group_by(
    MATCH_SLA_STD,
    UPRN
  ) %>% 
  summarise(
    PARENT_UPRN = max(PARENT_UPRN),
    NURSING_HOME_FLAG = max(NURSING_HOME_FLAG),
    RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG),
    MAX_MONTHLY_PATIENTS = max(MAX_MONTHLY_PATIENTS),
    NUMBER_OF_BEDS = max(NUMBER_OF_BEDS),
    MONTHS = n_distinct(YEAR_MONTH),
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    NIC = sum(ITEM_PAY_DR_NIC)
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(SIZE = ifelse(PATS <= 10, "SMALL", "LARGE"))

# Pat features
df_pat = data %>% 
  filter(UPRN_FLAG == 1) %>% 
  group_by(MATCH_SLA_STD, UPRN) %>% 
  summarise(
    MALE = n_distinct(NHS_NO[GENDER == "Male"]),
    FEMALE = n_distinct(NHS_NO[GENDER == "Female"]),
    UNKNOWN = n_distinct(NHS_NO[is.na(GENDER)]),
    AGE_65_69 = n_distinct(NHS_NO[AGE_BAND == "65-69"]),
    AGE_70_74 = n_distinct(NHS_NO[AGE_BAND == "70-74"]),
    AGE_75_79 = n_distinct(NHS_NO[AGE_BAND == "75-79"]),
    AGE_80_84 = n_distinct(NHS_NO[AGE_BAND == "80-84"]),
    AGE_85_89 = n_distinct(NHS_NO[AGE_BAND == "85-89"]),
    AGE_90_PLUS = n_distinct(NHS_NO[AGE_BAND == "90+"])
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(
    PAT_TOTAL = UNKNOWN + MALE + FEMALE,
    MALE_PROP = MALE / PAT_TOTAL,
    FEMALE_PROP = FEMALE / PAT_TOTAL,
    UNKNOWN_PROP = UNKNOWN / PAT_TOTAL,
    AGE_65_69_PROP = AGE_65_69 / PAT_TOTAL,
    AGE_70_74_PROP = AGE_70_74 / PAT_TOTAL,
    AGE_75_79_PROP = AGE_75_79 / PAT_TOTAL,
    AGE_80_84_PROP = AGE_80_84 / PAT_TOTAL,
    AGE_85_89_PROP = AGE_85_89 / PAT_TOTAL,
    AGE_90_PLUS_PROP = AGE_90_PLUS / PAT_TOTAL
  )

# Cost PPM
df_cost_items_ppm = data %>% 
  filter(UPRN_FLAG == 1) %>% 
  group_by(MATCH_SLA_STD, UPRN, NHS_NO) %>% 
  summarise(
    MEAN_PATIENT_MONTHLY_COST = sum(ITEM_PAY_DR_NIC * 0.01, na.rm = TRUE) / n_distinct(YEAR_MONTH),
    MEAN_PATIENT_MONTHLY_ITEMS = sum(ITEM_COUNT, na.rm = TRUE) / n_distinct(YEAR_MONTH)
  ) %>%
  ungroup() %>% 
  group_by(MATCH_SLA_STD, UPRN) %>% 
  summarise(
    COST_PPM = mean(MEAN_PATIENT_MONTHLY_COST),
    ITEMS_PPM = mean(MEAN_PATIENT_MONTHLY_ITEMS)
  ) %>% 
  ungroup() %>% 
  collect()

# Unique meds PPM
df_meds_ppm = data %>% 
  filter(
    UPRN_FLAG == 1, 
    CHAPTER_DESCR %in% chapters
  ) %>% 
  # Number of unique meds per patient month
  group_by(MATCH_SLA_STD, UPRN, NHS_NO, YEAR_MONTH) %>% 
  summarise(MONTHLY_UNIQUE_MEDS =  n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>%
  ungroup() %>% 
  # Mean unique monthly meds per patient
  group_by(MATCH_SLA_STD, UPRN, NHS_NO) %>% 
  summarise(MEAN_MONTHLY_UNIQUE_MEDS = mean(MONTHLY_UNIQUE_MEDS)) %>% 
  ungroup() %>% 
  # UPRN-level unqiue meds PPM
  group_by(MATCH_SLA_STD, UPRN) %>% 
  summarise(MEDS_PPM = mean(MEAN_MONTHLY_UNIQUE_MEDS)) %>% 
  ungroup() %>% 
  collect()

# 10 or more uique meds
df_ten_ppm = data %>% 
  filter(
    UPRN_FLAG == 1, 
    CHAPTER_DESCR %in% chapters
  ) %>% 
  # Number of unique meds per patient month
  group_by(MATCH_SLA_STD, UPRN, NHS_NO, YEAR_MONTH) %>% 
  summarise(MONTHLY_UNIQUE_MEDS =  n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>%
  ungroup() %>% 
  # Mean unique monthly meds per patient
  group_by(MATCH_SLA_STD, UPRN, NHS_NO) %>% 
  summarise(MEAN_MONTHLY_UNIQUE_MEDS = mean(MONTHLY_UNIQUE_MEDS)) %>% 
  ungroup() %>% 
  mutate(TEN_PLUS = ifelse(MEAN_MONTHLY_UNIQUE_MEDS >= 10, 1, 0)) %>% 
  group_by(MATCH_SLA_STD, UPRN) %>% 
  summarise(TEN_UNIQUE_PPM = sum(TEN_PLUS) / n()) %>% 
  ungroup() %>% 
  collect()




# Plot each ppm metric distribution
hchart(df_cost_items_ppm$COST_PPM) %>% 
  hc_xAxis(max = 500)

hchart(df_cost_items_ppm$ITEMS_PPM) %>% 
  hc_xAxis(max = 40)

hchart(df_meds_ppm$MEDS_PPM)

hchart(df_ten_ppm$TEN_UNIQUE_PPM)

# Findings ---------------------------------------------------------------------

# 1. Care homes can be split into 2 roughly even groups
# These are <=10 patients or >10 patients
# This could be useful, if 'smaller' CH need special/specific consideration
# For example, each group could be clustered separately
# And outliers could be identified within the context of each group

# Small CH count
df_overall %>% 
  filter(PATS <= 10) %>% 
  tally()

# Large CH count
df_overall %>% 
  filter(PATS > 10) %>% 
  tally()
