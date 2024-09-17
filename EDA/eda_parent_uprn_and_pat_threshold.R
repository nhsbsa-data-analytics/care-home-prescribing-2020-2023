# Libraries
library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)

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
cols = data.frame(COL = names(data)) %>% arrange(COL)

# Part 1: compare data whether grouped by parent uprn or not -------------------

# Grouped by uprn
df_std = data %>% 
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

# Grouped by parent uprn (where applicable
df_merge = data %>% 
  filter(UPRN_FLAG == 1) %>% 
  mutate(
    MATCH_SLA_STD = coalesce(MATCH_SLA_PARENT, MATCH_SLA_STD),
    UPRN = coalesce(PARENT_UPRN, UPRN)
  ) %>% 
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
  mutate(
    SIZE = ifelse(PATS <= 10, "SMALL", "LARGE"),
    PARENT_UPRN = ifelse(is.na(PARENT_UPRN), 0, 1)
    )

# Many of the 'small' CH can be grouped together 
table(df_std$SIZE)
table(df_merge$SIZE)

# Many 'parent groups are still small, there are only 309 'parent groups' 
df_merge %>% 
  filter(SIZE == "LARGE") %>% 
  count(PARENT_UPRN)

# With 'parent groups', 96% of items are now accounted by 'large' CH
df_merge %>% 
  group_by(SIZE) %>% 
  summarise(ITEMS=sum(ITEMS)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(ITEMS),
    PROP = ITEMS / TOTAL
  )

# With 'parent groups', 63% of SLA records are now accounted by 'large' CH
df_merge %>% 
  group_by(SIZE) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(n),
    PROP = n /TOTAL
  )

# If 'larger' CH have 10 or more distinct patients over 12 months
# And we use 'parent uprn' groups where applicable
# 1. We retain 63% of records
# 2. Yet retain 96% of items

# Part 2: generate the 'basic' CH metrics and investigate distributions --------

# Pat features
df_pat = data %>% 
  filter(UPRN_FLAG == 1) %>% 
  mutate(
    MATCH_SLA_STD = coalesce(MATCH_SLA_PARENT, MATCH_SLA_STD),
    UPRN = coalesce(PARENT_UPRN, UPRN)
  ) %>%
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
  mutate(
    MATCH_SLA_STD = coalesce(MATCH_SLA_PARENT, MATCH_SLA_STD),
    UPRN = coalesce(PARENT_UPRN, UPRN)
  ) %>%
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
  mutate(
    MATCH_SLA_STD = coalesce(MATCH_SLA_PARENT, MATCH_SLA_STD),
    UPRN = coalesce(PARENT_UPRN, UPRN)
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

# 10 or more unique meds
df_ten_ppm = data %>% 
  filter(
    UPRN_FLAG == 1, 
    CHAPTER_DESCR %in% chapters
  ) %>% 
  mutate(
    MATCH_SLA_STD = coalesce(MATCH_SLA_PARENT, MATCH_SLA_STD),
    UPRN = coalesce(PARENT_UPRN, UPRN)
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

# Merge all data
df_total = df_merge %>% 
  inner_join(df_cost_items_ppm) %>% 
  inner_join(df_meds_ppm) %>% 
  inner_join(df_ten_ppm)

# 3. Explore PMM metrics distributions -----------------------------------------

# Cost PPM
hchart(df_cost_items_ppm$COST_PPM) %>% hc_xAxis(max = 500)

ggplot(df_total, aes(COST_PPM, fill = SIZE))+
  geom_density(alpha=0.5)+
  xlim(0,500)


# Items PPM 
hchart(df_cost_items_ppm$ITEMS_PPM) %>% hc_xAxis(max = 40)

ggplot(df_total, aes(ITEMS_PPM, fill = SIZE))+
  geom_density(alpha=0.5)


# Meds PPM
hchart(df_meds_ppm$MEDS_PPM)

ggplot(df_total, aes(MEDS_PPM, fill = SIZE))+
  geom_density(alpha=0.5)


# Unique PPM
hchart(df_ten_ppm$TEN_UNIQUE_PPM)

ggplot(df_total, aes(TEN_UNIQUE_PPM, fill = SIZE))+
  geom_density(alpha=0.5)

# Removing 'smmall' CH helps normaise metric distributions
# This could be useful, if 'smaller' CH need special/specific consideration
# Outlier detection may be more effective with normally distributed vars

# 4. Investigate other large/size CH thresholds --------------------------------

# Adjust size value to check other size thresholds
adjust_size_check_dist = function(size_val, vars){
  
  df_total %>% 
    mutate(SIZE = ifelse(PATS < size_val, "SMALL", "LARGE")) %>% 
    ggplot(., aes({{vars}}, fill = SIZE))+
    geom_density(alpha=0.5)
}

# Check with 5 threshold, large CH distributions still normal
adjust_size_check_dist(5, ITEMS_PPM) + xlim(0,40)
adjust_size_check_dist(5, COST_PPM) + xlim(0,400)
adjust_size_check_dist(5, MEDS_PPM)
adjust_size_check_dist(5, TEN_UNIQUE_PPM)

# Small CH count
df_total %>% 
  mutate(SIZE = ifelse(PATS < 5, "SMALL", "LARGE")) %>% 
  group_by(SIZE) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(n),
    PROP = n /TOTAL
  )

# Items and cost by size
df_total %>% 
  mutate(SIZE = ifelse(PATS < 5, "SMALL", "LARGE")) %>% 
  group_by(SIZE) %>% 
  summarise(ITEMS=sum(ITEMS)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(ITEMS),
    PROP = ITEMS / TOTAL
  )

# If 'larger' CH have *5* or more distinct patients over 12 months#
# 1. We retain 70% of records
# 2. Yet retain 97.8% of items

# Repeat with 4 threshold
adjust_size_check_dist(3, ITEMS_PPM) + xlim(0,40)
adjust_size_check_dist(3, COST_PPM) + xlim(0,400)
adjust_size_check_dist(3, MEDS_PPM)
adjust_size_check_dist(3, TEN_UNIQUE_PPM)

# Small CH count
df_total %>% 
  mutate(SIZE = ifelse(PATS < 3, "SMALL", "LARGE")) %>% 
  group_by(SIZE) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(n),
    PROP = n /TOTAL
  )

# Items and cost by size
df_total %>% 
  mutate(SIZE = ifelse(PATS < 3, "SMALL", "LARGE")) %>% 
  group_by(SIZE) %>% 
  summarise(ITEMS=sum(ITEMS)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(ITEMS),
    PROP = ITEMS / TOTAL
  )

# If 'larger' CH have *3* or more distinct patients over 12 months#
# 1. We retain 77.5% of records
# 2. Yet retain 98.8% of items

# 5. Conclusions ---------------------------------------------------------------

# 1. Use 'parent uprn' groups to reduce the number of 'small' CH
# 2. Exclude any CH with <5 patients (still ~98% items)
# 3. If needed, reduce this threshold and exclude any CH with <3 patients

# Disconnect and clean
DBI::dbDisconnect(con)
rm(list = ls()); gc()
