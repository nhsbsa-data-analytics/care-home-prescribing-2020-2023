
# Library
library(dplyr)
library(dbplyr)

# Connect to dalp
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))
  
# Count care home patients in each decile
mod_patients_by_imd_df <- fact_db %>%
  filter(
    CH_FLAG == 1,
    !is.na(IMD_DECILE)
  ) %>% 
  group_by(FY, IMD_DECILE) %>%
  summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup() %>% 
  group_by(FY) %>% 
  # Sum so annual percentages add to 100 (despite double counting)
  mutate(PCT_PATIENTS = janitor::round_half_up(TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100, 2)) %>% 
  ungroup() %>% 
  collect() %>% 
  arrange(FY, IMD_DECILE) %>% 
  rename(
    `Financial Year` = FY,
    `IMD Decile` = IMD_DECILE,
    `Number of Patients` = TOTAL_PATIENTS,
    `Percentage of Patients` = PCT_PATIENTS
  )

# Add to data-raw/
usethis::use_data(mod_patients_by_imd_df, overwrite = TRUE)

# Disconnect
DBI::dbDisconnect(con); rm(list = ls()); gc()
