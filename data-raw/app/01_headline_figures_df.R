
# Library
library(dplyr)
library(dbplyr)
library(assertr)
library(assertr.alt)


base_table <- "INT646_BASE_20200401_20250331"
start_year <- substring(base_table, 13, 16)
end_year <- substring(base_table, 22, 25)
EXPECTED_YEARS <- as.integer(end_year) - as.integer(start_year)
EXPECTED_MONTHS <- 12 * EXPECTED_YEARS

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Base table
data_db <- con %>%
  tbl(from = in_schema("DALL_REF", base_table))

data_db %>% 
  verify(nrow.alt(distinct(., FY)) == EXPECTED_YEARS) %>% 
  verify(nrow.alt(distinct(., YEAR_MONTH)) == EXPECTED_MONTHS)
  
# Key findings used within analysis summary text
key_findings_fy <- data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(FY, YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
  ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>%
  select(-YEAR_MONTH) %>% 
  group_by(FY) %>% 
  summarise_all(.funs = \(x) mean(x, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(FY)

if(!dir.exists("data-raw/temp")) dir.create("data-raw/temp")
saveRDS(key_findings_fy, "data-raw/temp/key_findings_fy.rds")
  
# Key findings used within analysis summary text
key_findings_fy_ch_flag <- data_db %>% 
  group_by(FY, CH_FLAG) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
  ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  group_by(FY) %>% 
  mutate(
    TOTAL_NIC = sum(NIC, na.rm = TRUE),
    TOTAL_ITEMS = sum(ITEMS, na.rm = TRUE),
    PROP_ITEMS = ITEMS / TOTAL_ITEMS * 100,
    PROP_NIC = NIC /TOTAL_NIC * 100
  ) %>% 
  arrange(FY)

if(!dir.exists("data-raw/temp")) dir.create("data-raw/temp")
saveRDS(key_findings_fy_ch_flag, "data-raw/temp/key_findings_fy_ch_flag_df.rds")

# Annual data df
annual_df = data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(TIME = FY, YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
    ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  select(-YEAR_MONTH) %>% 
  group_by(TIME) %>% 
  summarise_all(.funs = \(x) mean(x, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # Patients nearest 100, Items 1,000, Cost 10,000
    PATS = janitor::round_half_up(PATS, -2),
    ITEMS = janitor::round_half_up(ITEMS, -3),
    NIC = janitor::round_half_up(NIC, -4),
    TYPE = "Annual monthly mean"
    ) %>% 
  arrange(TIME) %>% 
  select(TIME, TYPE, PATS, ITEMS, NIC) %>%
  verify(nrow.alt(.) == EXPECTED_YEARS) %>% 
  assert.alt(not_na.alt, PATS, ITEMS, NIC)

# Monthly data df
monthly_df = data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(TIME = YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
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
  select(TIME, TYPE, PATS, ITEMS, NIC) %>% 
  verify(nrow.alt(.) == EXPECTED_MONTHS) %>% 
  assert.alt(not_na.alt, PATS, ITEMS, NIC)

# Bind dfs together
mod_headline_figures_df = rbind(annual_df, monthly_df)

# Add to data-raw/
usethis::use_data(mod_headline_figures_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
