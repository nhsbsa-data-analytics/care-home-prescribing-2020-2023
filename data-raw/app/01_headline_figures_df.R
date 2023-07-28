
# Library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
data_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# Key findings used within analysis summary text
data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(FY, YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    NIC = sum(ITEM_PAY_DR_NIC) / 100
  ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  group_by(FY) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  arrange(FY)
  
# Key findings used within analysis summary text
data_db %>% 
  group_by(FY, CH_FLAG) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    NIC = sum(ITEM_PAY_DR_NIC) / 100
  ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  group_by(FY) %>% 
  mutate(
    TOTAL_NIC = sum(NIC),
    TOTAL_ITEMS = sum(ITEMS),
    PROP_ITEMS = ITEMS / TOTAL_ITEMS * 100,
    PROP_NIC = NIC /TOTAL_NIC * 100
  ) %>% 
  arrange(FY)

# Annual data df
annual_df = data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(TIME = FY, YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  select(-YEAR_MONTH) %>% 
  group_by(TIME) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  mutate(
    # Patients nearest 100, Items 1,000, Cost 10,000
    PATS = janitor::round_half_up(PATS, -2),
    ITEMS = janitor::round_half_up(ITEMS, -3),
    NIC = janitor::round_half_up(NIC, -4),
    TYPE = "Annual monthly mean"
    ) %>% 
  arrange(TIME) %>% 
  select(TIME, TYPE, PATS, ITEMS, NIC)

# Monthly data df
monthly_df = data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(TIME = YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    NIC = sum(ITEM_PAY_DR_NIC) / 100
  ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  mutate(
    TYPE = "Monthly sum",
    # Patients nearest 100, Items 1,000, Cost 10,000
    PATS = janitor::round_half_up(PATS, -2),
    ITEMS = janitor::round_half_up(ITEMS, -3),
    NIC = janitor::round_half_up(NIC, -4),
    ORDER = TIME,
    YEAR = substr(TIME, 1, 4),
    MONTH = substr(TIME, 5, 6),
    MONTH = ifelse(substr(MONTH,1,1) == "0", substr(MONTH,2,2), substr(MONTH,1,2)),
    MONTH = month.abb[as.integer(MONTH)],
    TIME = paste0(YEAR, " - ", MONTH)
  ) %>% 
  arrange(ORDER) %>% 
  select(TIME, TYPE, PATS, ITEMS, NIC)

# Bind dfs together
mod_headline_figures_df = rbind(annual_df, monthly_df)

# Add to data-raw/
usethis::use_data(mod_headline_figures_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con); rm(list = ls()); gc()
