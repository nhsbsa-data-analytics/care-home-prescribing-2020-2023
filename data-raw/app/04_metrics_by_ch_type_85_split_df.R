# Initial setup -----------------------------------------------------------

# Expected run time ~35 minutes @parallel 36
library(dplyr)
library(dbplyr)
library(tidyr)
devtools::load_all()


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
  tbl(from = in_schema("DALL_REF", base_table)) %>%
  verify(nrow.alt(distinct(., FY)) == EXPECTED_YEARS) %>% 
  verify(nrow.alt(distinct(., YEAR_MONTH)) == EXPECTED_MONTHS)


# Row validation calculation ----------------------------------------------

EXPECTED_ROWS <- EXPECTED_YEARS *
  4 * # Categories are CH, NON_CH, RH or NH
  2   # Have 2 age band groupings, 65+ and 85+


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
  # Only keep rows where value is 1, if it is not then it is not the care home
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

# Union both age band variants
init_db <- init_db %>%            # Start with all patients and flag for age band
  union(
    init_db %>% 
      filter(AGE_GTE_85 == 1) %>% # Get just those flagged as 85+
      mutate(AGE_GTE_85 = 0)      # And unflag, effectively including them in 65+ band
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
  dplyr::relocate(AGE_BAND, .after = CH_TYPE) %>%
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

# Clean-up ----------------------------------------------------------------------

# Disconnect
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
