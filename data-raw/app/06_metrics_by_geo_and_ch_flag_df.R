# Initial setup ----------------------------------------------------------------

# Expected run time ~40 minutes @parallel 24

# Library and functions
library(dplyr)
library(dbplyr)
library(stringr)
library(glue)
library(purrr)
devtools::load_all()
source("data-raw/app/data_raw_helpers.R")


base_table <- "INT646_BASE_20200401_20250331"
start_year <- substring(base_table, 13, 16)
end_year <- substring(base_table, 22, 25)
EXPECTED_YEARS <- as.integer(end_year) - as.integer(start_year)
EXPECTED_MONTHS <- 12 * EXPECTED_YEARS

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Data validation --------------------------------------------------------------

## Setup -----------------------------------------------------------------------

# Transform data for specified geography
transform_geo_data <- function(data, geography) {
  data %>%
    select(starts_with(glue("PCD_{geography}"))) %>%
    distinct() %>%
    rename_with(
      \(x) stringr::str_replace(x, glue("PCD_{geography}"), "SUB_GEOGRAPHY")
    ) %>%
    filter(!is.na(SUB_GEOGRAPHY_NAME))
}

### PCD - the data in Data Warehouse -------------------------------------------

# Distinct geography fields
PCD <- con %>%
  tbl(from = "INT646_POSTCODE_LOOKUP") %>%
  select(ends_with("_CODE"), ends_with("_NAME")) %>%
  distinct() %>%
  collect()

# Get data for each geography in same format
PCD_list <- list(
  REGION = PCD %>% transform_geo_data("REGION"),
  ICB    = PCD %>% transform_geo_data("ICB"),
  LAD    = PCD %>% transform_geo_data("LAD")
)

### CHD - the data in base table -----------------------------------------------

# Distinct geography fields
CHD <- con %>%
  tbl(from = in_schema("DALL_REF", base_table)) %>% 
  select(starts_with("PCD")) %>% 
  distinct() %>% 
  collect()

# Get data for each geography in same format
CHD_list <- list(
  REGION = CHD %>% transform_geo_data("REGION"),
  ICB    = CHD %>% transform_geo_data("ICB"),
  LAD    = CHD %>% transform_geo_data("LAD")
)

### GIS - the data from ArcGIS -------------------------------------------------

# Get data for each geography in same format (already done in geo_data.R)
GIS_list <- geo_data_validation

# Check sub-geography codes and names match exactly between all 3 sources;
# script will stop if not

## Validate --------------------------------------------------------------------

### Check sub-geography codes --------------------------------------------------

#### PCD vs GIS ----------------------------------------------------------------
check_pcd_sub_geo_codes <- list(
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

# Stop if check fails
stopifnot(
  "Some difference in geo codes: check `check_pcd_sub_geo_codes`"= {
    2 == check_pcd_sub_geo_codes %>%
      map(unique) %>%
      map(length) %>% 
      reduce(`+`)
  }
)

#### CHD vs GIS ----------------------------------------------------------------
check_chd_sub_geo_codes <- list(
  in_GIS_only = map2(
    GIS_list,
    CHD_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
  ),
  in_CHD_only = map2(
    CHD_list,
    GIS_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
  )
)

# Stop if check fails
stopifnot(
  "Some difference in geo codes: check `check_chd_sub_geo_codes`"= {
    2 == check_chd_sub_geo_codes %>%
      map(unique) %>%
      map(length) %>% 
      reduce(`+`)
  }
)

### Check sub-geography names --------------------------------------------------

#### PCD vs GIS ----------------------------------------------------------------
check_pcd_sub_geo_names <- list(
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

# Stop if check fails
stopifnot(
  "Some difference in geo names: check `check_pcd_sub_geo_names`"= {
    2 == check_pcd_sub_geo_names %>%
      map(unique) %>%
      map(length) %>% 
      reduce(`+`)
  }
)

#### CHD vs GIS ----------------------------------------------------------------
check_chd_sub_geo_names <- list(
  in_GIS_only = map2(
    GIS_list,
    CHD_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
  ),
  in_CHD_only = map2(
    CHD_list,
    GIS_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
  )
)

# Stop if check fails
stopifnot(
  "Some difference in geo codes: check `check_chd_sub_geo_names`"= {
    2 == check_chd_sub_geo_names %>%
      map(unique) %>%
      map(length) %>% 
      reduce(`+`)
  }
)

# Data prep --------------------------------------------------------------------

## Setup -----------------------------------------------------------------------

# Item-level base table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", base_table))

# Split verify to simplify dbplyr run
base_db %>% 
  verify(nrow.alt(distinct(., FY)) == EXPECTED_YEARS) %>% 
  verify(nrow.alt(distinct(., YEAR_MONTH)) == EXPECTED_MONTHS)


# Row validation calculation ----------------------------------------------

# Distinct counts for expected row calculation
distinct_counts <- base_db %>% 
  summarise(
    across(
      c(CH_FLAG, FY, (starts_with("PCD") & ends_with("CODE"))),
      n_distinct
    )
  ) %>%
  collect() %>% 
  rename_with(\(x) glue::glue("EXPECTED_{x}S")) %>% 
  as.list() %>% 
  purrr::iwalk(
    \(x, idx) assign(idx, x, envir = .GlobalEnv)
  )

# Expected row value
EXPECTED_ROWS <- EXPECTED_YEARS *
  EXPECTED_CH_FLAGS * (
    EXPECTED_PCD_REGION_CODES +
    EXPECTED_PCD_ICB_CODES +
    EXPECTED_PCD_LAD_CODES
  )

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
      ),
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

## Process ---------------------------------------------------------------------
metrics_by_geo_and_ch_flag_df <- names(geographies)[-1] %>% 
  map(aggregate_by_geo) %>%
  list_rbind()

## Format ----------------------------------------------------------------------
metrics_by_geo_and_ch_flag_df <- metrics_by_geo_and_ch_flag_df %>%
  mutate(CH_FLAG = as.logical(CH_FLAG)) %>% 
  filter(!is.na(SUB_GEOGRAPHY_NAME)) %>% 
  replace_na(list(UNIQ_MEDS_FALLS_PPM = 0, ANY_ACAP = 0, ACAP_TWO = 0, ACAP_THREE = 0)) %>%
  rename(TOTAL_PM_ACAP = ANY_ACAP) %>% 
  mutate(
    ACAP_TWO = NULL,
    ACAP_THREE = NULL
  ) %>% 
  format_data_raw("CH_FLAG") %>%
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

## Save ------------------------------------------------------------------------
usethis::use_data(metrics_by_geo_and_ch_flag_df, overwrite = TRUE)

# Cleanup ----------------------------------------------------------------------

# Disconnect
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
