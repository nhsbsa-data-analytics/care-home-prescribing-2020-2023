
# Library
library(dplyr)
library(dbplyr)

# Connect to dalp
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Connect to base data
base = con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20250331"))

# Load headline figures data
load("data/mod_headline_figures_df.rda")

# Load 
load("data/mod_ch_flag_drug_df.rda")

# 1. Latest financial year
latest_fy = readRDS("data-raw/temp/key_findings_fy.rds") %>% 
  select(FY) %>% 
  filter(FY == max(FY)) %>% 
  pull()

# 2. Latest financial year estimate of mean monthly care home patients
ch_monthly_pats = readRDS("data-raw/temp/key_findings_fy.rds") %>% 
  filter(FY == latest_fy) %>% 
  transmute(PATS = round(PATS, -3)) %>% 
  mutate(PATS = format(PATS, big.mark = ",", scientific = FALSE)) %>% 
  pull()

# 3. Latest estimate of annual care home prescribing items in latest financial year
ch_annual_items_m = readRDS("data-raw/temp/key_findings_fy_ch_flag_df.rds") %>% 
  filter(
    CH_FLAG == 1,
    FY == latest_fy
  ) %>% 
  transmute(ITEMS = round(ITEMS / 1000000)) %>% 
  pull()

# 4. Latest estimate of annual care home prescribing cost in latest financial year
ch_annual_cost_m = readRDS("data-raw/temp/key_findings_fy_ch_flag_df.rds") %>% 
  filter(
    CH_FLAG == 1,
    FY == latest_fy
  ) %>% 
  transmute(NIC = round(NIC / 1000000)) %>% 
  pull()

# 5. Number of ch pats with 1+ item in latest fy
ch_distinct_pats = base %>% 
  filter(
    CH_FLAG == 1,
    FY == latest_fy
    ) %>% 
  group_by(NHS_NO) %>% 
  summarise() %>% 
  ungroup() %>% 
  tally() %>% 
  collect() %>% 
  mutate(n = format(n, big.mark = ",", scientific = FALSE)) %>% 
  pull()

# 6. Percentage of care home items prescribed to women across whole time frame
ch_female_items_prop = base %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(GENDER) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP = round(100 * ITEMS / TOTAL_ITEMS, 1)
  ) %>% 
  filter(GENDER == "Female") %>% 
  select(PROP) %>% 
  pull()

# 7. Percentage of female 85+ care home patients
ch_female_85_plus_pats_prop = base %>% 
  filter(CH_FLAG == 1) %>% 
  mutate(AGE_BAND = ifelse(AGE_BAND %in% c("85-89", "90+"), "85+", "OTHER")) %>% 
  group_by(GENDER, AGE_BAND) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP = round(100 * ITEMS / TOTAL_ITEMS, 1)
  ) %>% 
  collect() %>% 
  filter(
    GENDER == "Female",
    AGE_BAND == "85+"
    ) %>% 
  select(PROP) %>% 
  pull()
  
# 8. Latest fy percentage ch items
ch_annual_prop_items = base %>% 
  filter(FY == latest_fy) %>% 
  group_by(CH_FLAG) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP = round(100 * ITEMS / TOTAL_ITEMS, 1)
  ) %>% 
  filter(CH_FLAG == 1) %>% 
  select(PROP) %>% 
  pull()

# 9. Latest fy percentage ch cost
ch_annual_prop_cost = base %>% 
  filter(FY == latest_fy) %>% 
  group_by(CH_FLAG) %>% 
  summarise(NIC = sum(ITEM_PAY_DR_NIC)) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(
    TOTAL_NIC = sum(NIC),
    PROP = round(100 * NIC / TOTAL_NIC, 1)
  ) %>% 
  filter(CH_FLAG == 1) %>% 
  select(PROP) %>% 
  pull()

# 10. Number of care patients with 1+ prescription item in latest financial year
latest_estimate_fy_total_ch_pats = base %>% 
  filter(
    CH_FLAG == 1,
    FY == latest_fy
  ) %>% 
  group_by(NHS_NO) %>% 
  summarise() %>% 
  ungroup() %>% 
  tally() %>% 
  collect() %>% 
  mutate(
    n = round(n, -3),
    n = format(n, big.mark = ",", scientific = FALSE)
    ) %>% 
  pull()

# 11.1. Max monthly pats (df)
ch_peak_pats = mod_headline_figures_df %>% 
  filter(
    TYPE == "Monthly sum",
    PATS == max(PATS)
    ) %>% 
  mutate(
    YEAR = sapply(strsplit(TIME, " - "), `[`, 1),
    MONTH = sapply(strsplit(TIME, " - "), `[`, 2),
    MONTH = month.name[match(MONTH, month.abb)],
    TIME = paste0(MONTH, " ", YEAR)
  ) %>% 
  select(TIME, PATS)

# 11.2. Individual value 1
ch_peak_pats_time = ch_peak_pats %>%
  select(TIME) %>% 
  pull()
  
# 11.3. Individual value 2
ch_peak_pats_count = ch_peak_pats %>%
  collect() %>% 
  transmute(PATS = format(PATS, big.mark = ",", scientific = FALSE)) %>% 
  pull()

# 12.1. Max monthly items (df)
ch_peak_items = mod_headline_figures_df %>% 
  filter(
    TYPE == "Monthly sum",
    ITEMS == max(ITEMS)
  ) %>% 
  mutate(
    YEAR = sapply(strsplit(TIME, " - "), `[`, 1),
    MONTH = sapply(strsplit(TIME, " - "), `[`, 2),
    MONTH = month.name[match(MONTH, month.abb)],
    TIME = paste0(MONTH, " ", YEAR)
  ) %>% 
  select(TIME, ITEMS)

# 12.2. Individual value 2
ch_peak_items_time = ch_peak_items %>% 
  select(TIME) %>% 
  pull()

# 12.3. Individual value 2
ch_peak_items_count = ch_peak_items %>% 
  select(ITEMS) %>% 
  transmute(ITEMS = signif(ITEMS, 2) / 1000000) %>% 
  pull()

# 13.1. Care home type proportions (df)
ch_type_item_prop = base %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(NURSING_HOME_FLAG, RESIDENTIAL_HOME_FLAG) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP = round(100 * ITEMS / TOTAL_ITEMS, 1)
  )

# 13.2. Individual value 1
ch_type_prop_nursing = ch_type_item_prop %>% 
  filter(
    NURSING_HOME_FLAG == 1,
    RESIDENTIAL_HOME_FLAG == 0
  ) %>% 
  select(PROP) %>% 
  pull() %>% 
  round(1) %>% 
  format(nsmall = 1)

# 13.3. Indiviudual value 2
ch_type_prop_residential = ch_type_item_prop %>% 
  filter(
    NURSING_HOME_FLAG == 0,
    RESIDENTIAL_HOME_FLAG == 1
  ) %>% 
  select(PROP) %>% 
  pull() %>% 
  round(1) %>% 
  format(nsmall = 1)

# 13.4. Individual value 3
ch_type_prop_both = ch_type_item_prop %>% 
  filter(
    NURSING_HOME_FLAG == 1,
    RESIDENTIAL_HOME_FLAG == 1
  ) %>% 
  select(PROP) %>% 
  pull() %>% 
  round(1) %>% 
  format(nsmall = 1)

# 13.5. Individual value 4
ch_type_prop_neither = ch_type_item_prop %>% 
  filter(
    is.na(NURSING_HOME_FLAG),
    is.na(RESIDENTIAL_HOME_FLAG)
  ) %>% 
  select(PROP) %>% 
  pull() %>% 
  round(1) %>% 
  format(nsmall = 1)

# 14.1. Greatest % of items by ch_flag in latest year (df)
chem_sub_most_items = mod_ch_flag_drug_df %>% 
  filter(
    METRIC == "% of total annual number of prescription items",
    BNF_PARENT == "Chemical substance",
    FY == latest_fy
  ) %>% 
  slice_max(by = CH_FLAG, n = 2, order_by = VALUE) %>% 
  group_by(FY, CH_FLAG) %>% 
  mutate(RANK = rank(desc(VALUE))) %>% 
  ungroup()

# 14.2. Individual value 1
chem_sub_ch_items_rank_one = chem_sub_most_items %>% 
  filter(
    CH_FLAG == 1,
    RANK == 1
  ) %>% 
  select(BNF_CHILD) %>% 
  pull()

# 14.3. Individual value 2
chem_sub_ch_items_rank_two = chem_sub_most_items %>% 
  filter(
    CH_FLAG == 1,
    RANK == 2
  ) %>% 
  select(BNF_CHILD) %>% 
  pull()

# 14.4. Individual value 3
chem_sub_non_ch_items_rank_one = chem_sub_most_items %>% 
  filter(
    CH_FLAG == 0,
    RANK == 1
  ) %>% 
  select(BNF_CHILD) %>% 
  pull()

# 15.1. Greatest % of cost by ch_flag in latest year (df)
chem_sub_most_cost = mod_ch_flag_drug_df %>% 
  filter(
    METRIC == "% of total annual drug cost",
    BNF_PARENT == "Chemical substance",
    FY == latest_fy
  ) %>% 
  slice_max(by = CH_FLAG, n = 2, order_by = VALUE) %>% 
  group_by(FY, CH_FLAG) %>% 
  mutate(RANK = rank(desc(VALUE))) %>% 
  ungroup()

# 15.2.
chem_sub_ch_cost_rank_one = chem_sub_most_cost %>% 
  filter(
    CH_FLAG == 1,
    RANK == 1
  ) %>% 
  select(BNF_CHILD) %>% 
  pull()

# 15.3.
chem_sub_non_ch_cost_rank_one = chem_sub_most_cost %>% 
  filter(
    CH_FLAG == 0,
    RANK == 1
  ) %>% 
  select(BNF_CHILD) %>% 
  pull()

# Save to config file -----------------------------------------------------

# Saving figures for dynamic inclusion, just a couple as example
# Each separate figure should be a single named value, so the df outputs would 
# need processed accordingly...
yaml::write_yaml(
  list(
    latest_fy = latest_fy,
    ch_monthly_pats = ch_monthly_pats,
    ch_annual_items_m = ch_annual_items_m,
    ch_annual_cost_m = ch_annual_cost_m,
    ch_distinct_pats = ch_distinct_pats,
    ch_female_items_prop = ch_female_items_prop,
    ch_female_85_plus_pats_prop = ch_female_85_plus_pats_prop,
    ch_annual_prop_items = ch_annual_prop_items,
    ch_annual_prop_cost = ch_annual_prop_cost,
    latest_estimate_fy_total_ch_pats = latest_estimate_fy_total_ch_pats,
    ch_peak_pats_time = ch_peak_pats_time,
    ch_peak_pats_count = ch_peak_pats_count,
    ch_peak_items_time = ch_peak_items_time,
    ch_peak_items_count = ch_peak_items_count,
    ch_type_prop_nursing = ch_type_prop_nursing,
    ch_type_prop_residential = ch_type_prop_residential,
    ch_type_prop_both = ch_type_prop_both,
    ch_type_prop_neither = ch_type_prop_neither,
    chem_sub_ch_items_rank_one = chem_sub_ch_items_rank_one,
    chem_sub_ch_items_rank_two = chem_sub_ch_items_rank_two,
    chem_sub_non_ch_items_rank_one = chem_sub_non_ch_items_rank_one,
    chem_sub_ch_cost_rank_one = chem_sub_ch_cost_rank_one,
    chem_sub_non_ch_cost_rank_one = chem_sub_non_ch_cost_rank_one
  ),
  "data/latest_figures.yaml"
)

# Clean environment
DBI::dbDisconnect(con); rm(list = ls()); gc()
