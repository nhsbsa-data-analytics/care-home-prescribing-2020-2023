
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

# Annual data df
annual_df = data_db %>% 
  group_by(TIME = FY, YEAR_MONTH, CH_FLAG) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
    ) %>% 
  ungroup() %>% 
  select(-YEAR_MONTH) %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  arrange(TIME, CH_FLAG) %>% 
  group_by(TIME, CH_FLAG) %>% 
  summarise(
    across(
      everything(),
      list(
        TOTAL = \(x) sum(x, na.rm = TRUE),
        MEAN = \(x) mean(x, na.rm = TRUE)
      )
    )
  ) %>% 
  mutate(
    ITEMS_PROP = ITEMS_TOTAL / sum(ITEMS_TOTAL),
    NIC_PROP = NIC_TOTAL / sum(NIC_TOTAL)
  ) %>% 
  ungroup() %>% 
  filter(CH_FLAG == 1) %>% 
  mutate(
    # Patients nearest 100, Items 1,000, Cost 10,000
    PATS = janitor::round_half_up(PATS_MEAN, -2),
    ITEMS = janitor::round_half_up(ITEMS_MEAN, -3),
    NIC = janitor::round_half_up(NIC_MEAN, -4),
    across(ends_with("PROP"), \(x) round(100 * x, 1)),
    TYPE = "Annual"
  ) %>% 
  rename_with(\(x) gsub("PROP", "PERC", x), ends_with("PROP")) %>% 
  select(TIME, TYPE, PATS, ITEMS, NIC, ends_with("PERC")) %>%
  verify(nrow.alt(.) == EXPECTED_YEARS) %>%
  assert.alt(not_na.alt, PATS, ITEMS, NIC)

# Monthly data df
monthly_df = data_db %>% 
  group_by(TIME = YEAR_MONTH, CH_FLAG) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
  ) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  arrange(TIME, CH_FLAG) %>% 
  group_by(TIME) %>%
  mutate(
    ITEMS_PROP = ITEMS / sum(ITEMS),
    NIC_PROP = NIC / sum(NIC)
  ) %>% 
  ungroup() %>% 
  filter(CH_FLAG == 1) %>% 
  mutate(
    # Patients nearest 100, Items 1,000, Cost 10,000
    PATS = janitor::round_half_up(PATS, -2),
    ITEMS = janitor::round_half_up(ITEMS, -3),
    NIC = janitor::round_half_up(NIC, -4),
    across(ends_with("PROP"), \(x) round(100 * x, 1)),
    YEAR = substr(TIME, 1, 4),
    MONTH = substr(TIME, 5, 6),
    MONTH = ifelse(substr(MONTH,1,1) == "0", substr(MONTH,2,2), substr(MONTH,1,2)),
    MONTH = month.abb[as.integer(MONTH)],
    TIME = paste0(YEAR, " - ", MONTH),
    TYPE = "Monthly"
  ) %>% 
  rename_with(\(x) gsub("PROP", "PERC", x), ends_with("PROP")) %>% 
  select(TIME, TYPE, PATS, ITEMS, NIC, ends_with("PERC")) %>% 
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
