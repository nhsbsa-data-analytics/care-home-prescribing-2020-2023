# Running time ~35 min

# Libraries and functions
library(dplyr)
library(dbplyr)
devtools::load_all()
source("data-raw/app/data_raw_helpers.R")


base_table <- "INT646_BASE_20200401_20250331"
start_year <- substring(base_table, 13, 16)
end_year <- substring(base_table, 22, 25)
EXPECTED_YEARS <- as.integer(end_year) - as.integer(start_year)
EXPECTED_MONTHS <- 12 * EXPECTED_YEARS

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |>
  tbl(from = in_schema("DALL_REF", base_table)) %>%
  filter(GENDER %in% c("Male", "Female"))

# Perform validations
base_db %>% 
  verify(nrow.alt(distinct(., FY)) == EXPECTED_YEARS) %>% 
  verify(nrow.alt(distinct(., YEAR_MONTH)) == EXPECTED_MONTHS)
  
# Row validation calculation ---------------------------------------------------

# Distinct categories per grouping variable
distinct_counts <- base_db %>% 
  summarise(
    across(
      c(CH_FLAG, FY, AGE_BAND, GENDER),
      n_distinct
    )
  ) %>%
  collect() %>% 
  rename_with(\(x) glue::glue("EXPECTED_{x}S")) %>% 
  as.list() %>% 
  purrr::iwalk(
    \(x, idx) assign(idx, x, envir = .GlobalEnv)
  )

# Generate expeted row count value
EXPECTED_ROWS <- EXPECTED_YEARS *
  EXPECTED_CH_FLAGS * 
  EXPECTED_AGE_BANDS *
  EXPECTED_GENDERS

Sys.time()
# Get metrics
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
  nest_cols = c("FY", "GENDER", "AGE_BAND")
)

# Format for highcharter
metrics_by_age_gender_and_ch_flag_df <- metrics_by_age_gender_and_ch_flag_df |>
  format_data_raw(c("GENDER", "AGE_BAND", "CH_FLAG")) %>%
  verify(nrow.alt(.) == EXPECTED_ROWS) %>% 
  assert.alt(
    not_na.alt,
    ITEMS_PPM,
    COST_PPM,
    UNIQ_MEDS_PPM,
    UNIQ_MEDS_FALLS_PPM,
    TOTAL_PM,
    TOTAL_PM_ACB,
    TOTAL_PM_DAMN,
    TOTAL_PM_ACAP,
    PCT_PM_GTE_SIX,
    PCT_PM_GTE_TEN,
    PCT_PM_ACB,
    PCT_PM_DAMN,
    PCT_PM_FALLS,
    PCT_ACAP_TWO,
    PCT_ACAP_THREE
  )

# Add to data/
usethis::use_data(
  metrics_by_age_gender_and_ch_flag_df,
  overwrite = T
)

# Disconnect from database
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
