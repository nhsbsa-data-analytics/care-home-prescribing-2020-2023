# Initial setup -----------------------------------------------------------

# Expected run time ~35 minutes @parallel 36
library(dplyr)
library(dbplyr)
library(tidyr)
devtools::load_all()
source("data-raw/app/data_raw_helpers.R")


base_table <- "INT646_BASE_20200401_20250331"
start_year <- substring(base_table, 13, 16)
end_year <- substring(base_table, 22, 25)
EXPECTED_YEARS <- as.integer(end_year) - as.integer(start_year)
EXPECTED_MONTHS <- 12 * EXPECTED_YEARS

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Data prep ---------------------------------------------------------------

## Setup ------------------------------------------------------------------

# Item-level base table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", base_table))

# Split assertr check
base_db %>% 
  verify(nrow.alt(distinct(., FY)) == EXPECTED_YEARS) %>% 
  verify(nrow.alt(distinct(., YEAR_MONTH)) == EXPECTED_MONTHS)

# Row validation calculation ---------------------------------------------------

# Estimate number of rows in final output
EXPECTED_ROWS <- EXPECTED_YEARS *
  4 * # Categories are CH, NON_CH, RH or NH
  2   # Have 2 age band groupings, 65+ and 85+

# Function just for this mod to speed up get metrics (rather than 1 long query)
get_metrics_mod4 = function(df){
  
  get_metrics(
    df,
    first_grouping = c(
      "FY",
      "YEAR_MONTH",
      "NHS_NO",
      "CH_TYPE",
      "AGE_BAND"
    ),
    second_grouping = c(
      "FY",
      "CH_TYPE",
      "AGE_BAND"
    )
  ) %>% 
  select(FY, CH_TYPE, AGE_BAND, everything())
}

# 8 individual df outputs ------------------------------------------------------

# Split total query into chunks
ch_flag_85_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 85,
      CH_FLAG == 1
    ) %>% 
    mutate(
      CH_TYPE = "Care home",
      AGE_BAND = "Ages 85+"
    )
)

# Split total query into chunks
non_ch_flag_85_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 85,
      CH_FLAG == 0
    ) %>% 
    mutate(
      CH_TYPE = "Non-care home",
      AGE_BAND = "Ages 85+"
    )
)

# Split total query into chunks
ch_flag_65_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 65,
      CH_FLAG == 1
    ) %>% 
    mutate(
      CH_TYPE = "Care home",
      AGE_BAND = "Ages 65+"
    )
)

# Split total query into chunks
non_ch_flag_65_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 65,
      CH_FLAG == 0
    ) %>% 
    mutate(
      CH_TYPE = "Non-care home",
      AGE_BAND = "Ages 65+"
    )
)

# Split total query into chunks
res_85_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 85,
      RESIDENTIAL_HOME_FLAG == 1
    ) %>% 
    mutate(
      CH_TYPE = "Residential home",
      AGE_BAND = "Ages 85+"
    )
)

# Split total query into chunks
res_65_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 65,
      RESIDENTIAL_HOME_FLAG == 1
    ) %>% 
    mutate(
      CH_TYPE = "Residential home",
      AGE_BAND = "Ages 65+"
    )
)

# Split total query into chunks
nurs_85_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 85,
      NURSING_HOME_FLAG == 1
    ) %>% 
    mutate(
      CH_TYPE = "Nursing home",
      AGE_BAND = "Ages 85+"
    )
)

# Split total query into chunks
nurs_65_plus_df = get_metrics_mod4(
  base_db %>% 
    filter(
      AGE >= 65,
      NURSING_HOME_FLAG == 1
    ) %>% 
    mutate(
      CH_TYPE = "Nursing home",
      AGE_BAND = "Ages 65+"
    )
)

# Bind outputs into single df and save -----------------------------------------

# Bind all 8 dfs
metrics_by_ch_type_85_split_df = do.call(rbind, list(
  ch_flag_85_plus_df,
  ch_flag_65_plus_df,
  non_ch_flag_85_plus_df,
  non_ch_flag_65_plus_df,
  res_85_plus_df,
  res_65_plus_df,
  nurs_85_plus_df,
  nurs_65_plus_df
  )) %>% 
  # Validate
  verify(nrow.alt(.) == EXPECTED_ROWS) %>% 
  assert.alt(
    not_na.alt,
    CH_TYPE,
    AGE_BAND,
    ITEMS_PPM,
    COST_PPM,
    UNIQ_MEDS_PPM,
    UNIQ_MEDS_FALLS_PPM,
    TOTAL_PM,
    TOTAL_PM_ACB,
    TOTAL_PM_DAMN,
    PCT_PM_GTE_SIX,
    PCT_PM_GTE_TEN,
    PCT_PM_ACB,
    PCT_PM_DAMN,
    PCT_PM_FALLS
  )

## Save ------------------------------------------------------------------------
usethis::use_data(metrics_by_ch_type_85_split_df, overwrite = TRUE)

# Clean-up ---------------------------------------------------------------------

# Disconnect
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
