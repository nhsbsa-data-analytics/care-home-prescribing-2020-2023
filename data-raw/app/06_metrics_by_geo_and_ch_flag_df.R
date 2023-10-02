# Initial setup -----------------------------------------------------------

# Expected run time ~40 minutes @parallel 24

library(dplyr)
library(dbplyr)
library(stringr)
library(glue)
library(purrr)

devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Data validation ---------------------------------------------------------

## Setup ------------------------------------------------------------------

PCD <- con %>%
  tbl(from = "INT646_POSTCODE_LOOKUP") %>%
  select(ends_with("CODE"), ends_with("NAME")) %>%
  distinct() %>%
  collect()

transform_PCD <- function(data, geography) {
  data %>%
    select(starts_with(glue("PCD_{geography}"))) %>%
    distinct() %>%
    rename_with(
      \(x) str_replace(x, glue("PCD_{geography}"), "SUB_GEOGRAPHY")
    ) %>%
    filter(!is.na(SUB_GEOGRAPHY_NAME))
}

PCD_list <- list(
  REGION = PCD %>% transform_PCD("REGION"),
  ICB    = PCD %>% transform_PCD("ICB"),
  LAD    = PCD %>% transform_PCD("LAD")
)

GIS_list <- geo_data_validation

# Check sub-geography codes and names match exactly between PCD and GIS; script
# will stop if not

## Check sub-geography codes ----------------------------------------------

check_sub_geo_codes <- list(
  in_GIS_only = map2(
    GIS_list,
    PCD_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
  ),
  in_PCD_only = map2(
    PCD_list,
    GIS_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
  )
)

stopifnot(
  "Some difference in geo codes: check `check_sub_geo_codes`"= {
    character(0) == check_sub_geo_codes %>%
      map(unique) %>%
      map(unique)
  }
)

## Check sub-geography names ----------------------------------------------

check_sub_geo_names <- list(
  in_GIS_only = map2(
    GIS_list,
    PCD_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
  ),
  in_PCD_only = map2(
    PCD_list,
    GIS_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
  )
)

stopifnot(
  "Some difference in geo names: check `check_sub_geo_names`"= {
    character(0) == check_sub_geo_names %>%
      map(unique) %>%
      map(unique)
  }
)

# Data prep ---------------------------------------------------------------

## Setup ------------------------------------------------------------------

# Item-level base table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# Aggregate by a geography
aggregate_by_geo <- function(geography_name) {
  # Identify geography cols
  geography_cols <- geographies[[geography_name]] %>%
    set_names(
      nm = str_replace(
        string = names(.),
        pattern = "BREAKDOWN",
        replacement = "GEOGRAPHY"
      )
    )
  
  get_metrics(
    base_db %>% 
      mutate(
        GEOGRAPHY = geography_name,
        # Remove any NHS or ICB acronyms
        !!sym(geography_cols[["SUB_GEOGRAPHY_NAME"]]) := REGEXP_REPLACE(
          !!sym(geography_cols[["SUB_GEOGRAPHY_NAME"]]),
          "NHS | ICB",
          ""
        )
      ) %>% 
      filter(!!sym(geography_cols[["SUB_GEOGRAPHY_NAME"]]) != "Isles of Scilly"),
    first_grouping = c(
      "FY",
      "YEAR_MONTH",
      "GEOGRAPHY",
      unname(geography_cols),
      "CH_FLAG",
      "NHS_NO"
    ),
    second_grouping = c(
      "FY",
      "GEOGRAPHY",
      unname(geography_cols),
      "CH_FLAG"
    ),
    nest_cols = c("GEOGRAPHY", unname(geography_cols))
  ) %>% 
    rename(!!!geography_cols)
}

## Process ----------------------------------------------------------------
metrics_by_geo_and_ch_flag_df <- names(geographies)[-1] %>% 
  map(aggregate_by_geo) %>%
  list_rbind()

## Format -----------------------------------------------------------------
metrics_by_geo_and_ch_flag_df <- metrics_by_geo_and_ch_flag_df %>%
  mutate(CH_FLAG = as.logical(CH_FLAG)) %>% 
  filter(!is.na(SUB_GEOGRAPHY_NAME)) %>% 
  format_data_raw("CH_FLAG") %>% 
  suppressWarnings() # We do not have Overall in this data

## Save -------------------------------------------------------------------
usethis::use_data(metrics_by_geo_and_ch_flag_df, overwrite = TRUE)

# Cleanup -----------------------------------------------------------------
DBI::dbDisconnect(con)
rm(list = ls())
