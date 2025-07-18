
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
  mutate(n = round(n, -3)) %>% 
  pull()

# 11. Max monthly pats
ch_peak_pats = mod_headline_figures_df %>% 
  filter(
    TYPE == "Monthly sum",
    PATS == max(PATS)
    ) %>% 
  select(TIME, PATS)

# 12. Max monthly items
ch_peak_items = mod_headline_figures_df %>% 
  filter(
    TYPE == "Monthly sum",
    ITEMS == max(ITEMS)
  ) %>% 
  select(TIME, ITEMS)

# 13. Care home type proportions
ch_type_item_prop = base %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(NURSING_HOME_FLAG, RESIDENTIAL_HOME_FLAG) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP = 100 * ITEMS / TOTAL_ITEMS
  )

# 14. Greatest % of items by ch_flag in latest year
chem_sub_most_items = mod_ch_flag_drug_df %>% 
  filter(
    METRIC == "% of total annual number of prescription items",
    BNF_PARENT == "Chemical substance",
    FY == latest_fy
  ) %>% 
  slice_max(by = CH_FLAG, n = 2, order_by = VALUE) %>% 
  arrange(CH_FLAG, desc(VALUE))

# 15. Greatest % of cost by ch_flag in latest year
chem_sub_most_cost = mod_ch_flag_drug_df %>% 
  filter(
    METRIC == "% of total annual drug cost",
    BNF_PARENT == "Chemical substance",
    FY == latest_fy
  ) %>% 
  slice_max(by = CH_FLAG, n = 2, order_by = VALUE) %>% 
  arrange(CH_FLAG, desc(VALUE))

# Clean environment so only relevant data remains
DBI::dbDisconnect(con); rm(con, base, mod_ch_flag_drug_df, mod_headline_figures_df)

# Final clean after figures extracted
rm(list = ls()); gc()
