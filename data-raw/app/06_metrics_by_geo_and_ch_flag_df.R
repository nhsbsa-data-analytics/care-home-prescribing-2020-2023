# Initial setup ----------------------------------------------------------------

# Expected run time ~40 minutes @parallel 24

# Library and functions
library(dplyr)
library(dbplyr)
library(stringr)
library(glue)
library(purrr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

base_table <- "INT646_BASE_20200401_20250331"

# Data validation --------------------------------------------------------------

## Setup -----------------------------------------------------------------------

# Transform data for specified geography
transform_geo_data <- function(data, geography) {
  data %>%
    select(starts_with(glue("PCD_{geography}"))) %>%
    distinct() %>%
    rename_with(
      \(x) str_replace(x, glue("PCD_{geography}"), "SUB_GEOGRAPHY")
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
  format_data_raw("CH_FLAG")

## Save ------------------------------------------------------------------------
usethis::use_data(metrics_by_geo_and_ch_flag_df, overwrite = TRUE)

# Cleanup ----------------------------------------------------------------------

# Disconnect
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
