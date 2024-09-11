# Initial setup -----------------------------------------------------------

# Expected run time ~35 minutes @parallel 36
library(dplyr)
library(dbplyr)
library(tidyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Data prep ---------------------------------------------------------------

## Setup ------------------------------------------------------------------

# Item-level base table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20240331"))

# Initial manipulation to create CH_TYPE column, later to be grouped by
init_db <- base_db %>%
  mutate(NON_CH_FLAG = 1L - CH_FLAG) %>% 
  # Remove unwanted 'FLAG' columns
  select(-c(AB_FLAG, EPS_FLAG, UPRN_FLAG)) %>% 
  # Pivot the remaining 'FLAG' columns
  pivot_longer(
    ends_with("FLAG"),
    names_to = "CH_TYPE"
  ) %>%
  # Only keep rows where value is 1, if it is not then it is not the carehome
  # type in that row
  filter(value == 1L) %>%
  # We only need the pivoted col names going forward
  select(-value) %>%
  mutate(
    CH_TYPE = case_match(
      CH_TYPE,
      "CH_FLAG"               ~ "Care home",
      "NURSING_HOME_FLAG"     ~ "Nursing home",
      "RESIDENTIAL_HOME_FLAG" ~ "Residential home",
      "NON_CH_FLAG"           ~ "Non-care home"
    ),
    AGE_GTE_85 = case_when(
      AGE >= 85 ~ 1L,
      TRUE ~ 0L
    )
  ) 

# Union both initi_db variants
init_db <- init_db %>% 
  union(
    init_db %>% 
      filter(AGE_GTE_85 == 1) %>% 
      mutate(AGE_GTE_85 = 0)
  )

## Process ----------------------------------------------------------------

# Get metrics
metrics_by_ch_type_85_split_df <- get_metrics(
  init_db,
  first_grouping = c(
    "FY",
    "YEAR_MONTH",
    "NHS_NO",
    "CH_TYPE",
    "AGE_GTE_85"
  ),
  second_grouping = c(
    "FY",
    "CH_TYPE",
    "AGE_GTE_85"
  )
)

# Generate age band categories
metrics_by_ch_type_85_split_df <- metrics_by_ch_type_85_split_df %>% 
  mutate(
    AGE_BAND = dplyr::case_match(
      AGE_GTE_85,
      0 ~ "Ages 65+",
      1 ~ "Ages 85+"
    ),
    AGE_GTE_85 = NULL
  ) %>% 
  dplyr::relocate(AGE_BAND, .after = CH_TYPE)
  
## Save ------------------------------------------------------------------------
usethis::use_data(metrics_by_ch_type_85_split_df, overwrite = TRUE)

# Cleanup ----------------------------------------------------------------------

# Disconnect
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
