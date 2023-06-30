
# Library
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
data_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# Annual data df
annual_df = data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(TIME = FY) %>% 
  summarise(
    PATS = round(n_distinct(NHS_NO), -2),
    ITEMS = round(sum(ITEM_COUNT), -3),
    NIC = round(sum(ITEM_PAY_DR_NIC) / 100, -3)
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(TYPE = "ANNUAL") %>% 
  arrange(TIME)
  
# Monthly data df
monthly_df = data_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(TIME = YEAR_MONTH) %>% 
  summarise(
    PATS = round(n_distinct(NHS_NO), -2),
    ITEMS = round(sum(ITEM_COUNT), -3),
    NIC = round(sum(ITEM_PAY_DR_NIC) / 100, -3)
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(TYPE = "MONTHLY") %>% 
  arrange(TIME) %>% 
  mutate(
    ORDER = TIME,
    YEAR = substr(TIME, 1, 4),
    MONTH = substr(TIME, 5, 6),
    MONTH = ifelse(substr(MONTH,1,1) == "0", substr(MONTH,2,2), substr(MONTH,1,2)),
    MONTH = month.abb[as.integer(MONTH)],
    TIME = paste0(YEAR, " - ", MONTH)
  ) %>% 
  arrange(ORDER) %>% 
  select(-c(YEAR, MONTH, ORDER))
  
# Bind dfs together
mod_headline_figures_df = rbind(annual_df, monthly_df)

# Add to data-raw/
usethis::use_data(mod_headline_figures_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con); rm(list = ls()); gc()
