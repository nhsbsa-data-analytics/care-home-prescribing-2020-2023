# Initial setup ----------------------------------------------------------------

# Library and functions
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(dbplyr)
library(assertr)
library(assertr.alt)
devtools::load_all()


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
  tbl(from = "INT646_POSTCODE_LOOKUP") %>% # Requires this table in your schema
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

# # Split verify to simplify dbplyr run
base_db %>%
  verify(nrow.alt(distinct(., FY)) == EXPECTED_YEARS) %>%
  verify(nrow.alt(distinct(., YEAR_MONTH)) == EXPECTED_MONTHS)


# Row validation calculation ----------------------------------------------

# Distinct counts for expected row calculation
distinct_counts <- base_db %>%
  summarise(
    across(
      c(FY, (starts_with("PCD") & ends_with("CODE"))),
      n_distinct
    )
  ) %>%
  collect() %>%
  rename_with(\(x) glue::glue("EXPECTED_{x}S")) %>%
  as.list() %>%
  purrr::iwalk(
    \(x, idx) assign(idx, x, envir = .GlobalEnv)
  )

# Expected row values
ANNUAL_EXPECTED_ROWS <- EXPECTED_YEARS *
  (
    EXPECTED_PCD_REGION_CODES +
      EXPECTED_PCD_ICB_CODES +
      EXPECTED_PCD_LAD_CODES
  )

MONTHLY_EXPECTED_ROWS <- EXPECTED_MONTHS *
  (
    EXPECTED_PCD_REGION_CODES +
      EXPECTED_PCD_ICB_CODES +
      EXPECTED_PCD_LAD_CODES
  )

# Aggregate by a geography
aggregate_by_geo <- function(geography_name = c(
                                "Overall", "Region", "ICS", "Local Authority"
                             ),
                             time_interval = c("Annual", "Monthly")) {
  geography_name <- match.arg(geography_name)
  time_interval <- match.arg(time_interval)
  
  # Identify geography cols
  geography_cols <- geographies[[geography_name]] %>%
    set_names(
      nm = str_replace(
        string = names(.),
        pattern = "BREAKDOWN",
        replacement = "GEOGRAPHY"
      )
    )
  
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
    {
      if (time_interval == "Annual") {
        rename(., TIME = FY) %>%
          group_by(
            TIME,
            YEAR_MONTH,
            GEOGRAPHY,
            !!!syms(unname(geography_cols)),
            CH_FLAG
          )
      } else {
        rename(., TIME = YEAR_MONTH) %>%
          group_by(
            TIME,
            GEOGRAPHY,
            !!!syms(unname(geography_cols)),
            CH_FLAG
          )
      }
    } %>%
    summarise(
      PATS = n_distinct(NHS_NO),
      ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
      NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
    ) %>%
    ungroup() %>%
    nhsbsaR::collect_with_parallelism(., 16) %>%
    arrange(
      TIME,
      !!!syms(unname(geography_cols)),
      CH_FLAG
    ) %>%
    {
      if (time_interval == "Annual") {
        group_by(
          .,
          TIME,
          GEOGRAPHY,
          !!!syms(unname(geography_cols)),
          CH_FLAG # Note this grouping column is dropped after the summarise
        ) %>%
          summarise(
            across(
              everything(),
              list(
                TOTAL = \(x) sum(x, na.rm = TRUE),
                MEAN = \(x) mean(x, na.rm = TRUE)
              )
            )
        )
      } else {
        group_by(
          .,
          TIME,
          GEOGRAPHY,
          !!!syms(unname(geography_cols))
        )
      }
    } %>%
    {
      if (time_interval == "Annual") {
        mutate(
          .,
          ITEMS_PROP = ITEMS_TOTAL / sum(ITEMS_TOTAL),
          NIC_PROP = NIC_TOTAL / sum(NIC_TOTAL)
        )
      } else {
        mutate(
          .,
          ITEMS_PROP = ITEMS / sum(ITEMS),
          NIC_PROP = NIC / sum(NIC)
        )
      }
    } %>%
    ungroup() %>%
    filter(CH_FLAG == 1) %>%
    {
      if (time_interval == "Annual") {
        mutate(
          .,
          PATS = PATS_MEAN,
          ITEMS = ITEMS_MEAN,
          NIC = NIC_MEAN,
          TYPE = time_interval
        )
      } else {
        mutate(
          .,
          YEAR = substr(TIME, 1, 4),
          MONTH = substr(TIME, 5, 6),
          MONTH = ifelse(substr(MONTH,1,1) == "0", substr(MONTH,2,2), substr(MONTH,1,2)),
          MONTH = month.abb[as.integer(MONTH)],
          TIME = paste0(YEAR, " - ", MONTH),
          TYPE = time_interval
        )
      }
    } %>%
    rename(!!!geography_cols) %>%
    drop_na(SUB_GEOGRAPHY_NAME) %>%
    select(
      TIME,
      TYPE,
      GEOGRAPHY,
      starts_with("SUB_GEOGRAPHY"), # No SUB_GEOGRAPHY_CODE for Overall
      PATS,
      ITEMS,
      NIC,
      ends_with("PROP")
    )
}


## Process ---------------------------------------------------------------------

# Add a dummy overall column
base_db <- base_db |>
  mutate(OVERALL = "Overall")

### Annual data

annual_df <- names(geographies) %>% 
  map(aggregate_by_geo) %>%
  list_rbind() %>%
  verify(nrow.alt(.) == ANNUAL_EXPECTED_ROWS) %>%
  assert.alt(not_na.alt, PATS, ITEMS, NIC, ITEMS_PERC, NIC_PERC)


### Monthly data

monthly_df <- names(geographies) %>% 
  map(aggregate_by_geo, time_interval = "Monthly") %>%
  list_rbind() %>%
  verify(nrow.alt(.) == MONTHLY_EXPECTED_ROWS) %>%
  assert.alt(not_na.alt, PATS, ITEMS, NIC, ITEMS_PERC, NIC_PERC)

# Bind dfs together
mod_headline_figures_df <- rbind(annual_df, monthly_df) %>% 
  relocate(SUB_GEOGRAPHY_CODE, .before = SUB_GEOGRAPHY_NAME)

# Add levels to geography and sort
mod_headline_figures_df <- mod_headline_figures_df %>% 
  format_data_raw(c())

## Format ----------------------------------------------------------------------
# @Adnan - need to decide right level of rounding
# mod_headline_figures_df_CHECK <- mod_headline_figures_df %>%
#   rename_with(\(x) gsub("PROP", "PERC", x), ends_with("PROP")) 
#   mutate(
#     # Patients nearest 100, Items 1,000, Cost 10,000
#     PATS = janitor::round_half_up(PATS, -2),
#     ITEMS = janitor::round_half_up(ITEMS, -3),
#     NIC = janitor::round_half_up(NIC, -4),
#     across(ends_with("PERC"), \(x) round(100 * x, 1))
#   )

## Save ------------------------------------------------------------------------
usethis::use_data(mod_headline_figures_df, overwrite = TRUE)

# Cleanup ----------------------------------------------------------------------

# Disconnect
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
