# Initial setup -----------------------------------------------------------

# Expected run time ~90 minutes @parallel 24

library(dplyr)
library(dbplyr)

devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Data prep ---------------------------------------------------------------

## Setup ------------------------------------------------------------------

# Item-level base table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

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
    )
  )

## Process ----------------------------------------------------------------

metrics_by_ch_type_df <- get_metrics(
  init_db,
  first_grouping = c(
    "FY",
    "YEAR_MONTH",
    "NHS_NO",
    "CH_TYPE"
  ),
  second_grouping = c(
    "FY",
    "CH_TYPE"
  )
)
  
## Save -------------------------------------------------------------------
usethis::use_data(metrics_by_ch_type_df, overwrite = TRUE)

# Cleanup -----------------------------------------------------------------
DBI::dbDisconnect(con)
rm(list = ls())
