# Running time ~25 min

library(dplyr)
library(dbplyr)
devtools::load_all()
source("data-raw/app/data_raw_helpers.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |> tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

metrics_by_age_gender_and_ch_flag_df <- get_metrics(
  base_db,
  first_grouping = c(
    "FY",
    "YEAR_MONTH",
    "NHS_NO",
    "AGE_BAND",
    "GENDER",
    "CH_FLAG"
  ),
  second_grouping = c(
    "FY",
    "AGE_BAND",
    "GENDER",
    "CH_FLAG"
  ),
  nest_cols = c("FY", "GENDER", "AGE_BAND"),
  num_parallel = 32
)

# Format for highcharter
metrics_by_age_gender_and_ch_flag_df <- metrics_by_age_gender_and_ch_flag_df |>
  format_data_raw(c("GENDER", "AGE_BAND", "CH_FLAG"))

# Add to data/
usethis::use_data(
  metrics_by_age_gender_and_ch_flag_df,
  overwrite = T
)

# Disconnect from database
DBI::dbDisconnect(con)
rm(list = ls()); gc()
