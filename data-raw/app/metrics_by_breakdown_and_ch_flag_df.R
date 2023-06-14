# Initial setup -----------------------------------------------------------

library(tidyverse)
library(glue)
library(dbplyr)
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
    rename_with(\(x) str_replace(x, glue("PCD_{geography}"), "SUB_GEOGRAPHY"))
}

PCD_list <- list(
  REGION = PCD %>% transform_PCD("REGION"),
  ICB    = PCD %>% transform_PCD("ICB"),
  LAD    = PCD %>% transform_PCD("LAD")
)

GIS_all <- map_df %>% as_tibble() %>% select(-GEOMETRY)

GIS_list <- list(
  REGION = GIS_all %>% filter(GEOGRAPHY == "Region"),
  ICB    = GIS_all %>%
    filter(GEOGRAPHY == "ICB") %>%
    mutate(
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, "Integrated Care Board", "ICB"),
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, " the ", " The "),
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, " of ", " Of "),
    ),
  LAD    = GIS_all %>% filter(GEOGRAPHY == "Local Authority")
)

# Check sub-geography codes and names match exactly between PCD and GIS; you
# should get character(0) for in_GIS_only and NA or character(0) for in_PCD_only

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
) %>% print()


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
) %>% print()



# Data prep ---------------------------------------------------------------

## Setup ------------------------------------------------------------------

# Item-level base table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# Add a dummy overall column - we keep CH and non-CH records for mod 06
base_db <- base_db %>%
  mutate(OVERALL = "Overall")

# Aggregate by a geography
metrics_by_geo_and_ch_flag <- function(geography_name) {
  # Identify geography cols
  geography_cols <- geographies[[geography_name]] %>%
    purrr::set_names(
      nm = stringr::str_replace(
        string = names(.),
        pattern = "BREAKDOWN",
        replacement = "GEOGRAPHY"
      )
    )
  
  # Group the table
  base_db %>%
    group_by(
      FY,
      YEAR_MONTH,
      GEOGRAPHY = geography_name,
      across(all_of(unname(geography_cols))),
      CH_FLAG,
      NHS_NO
    ) %>%
    rename(!!!geography_cols) %>%
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT),
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      UNIQUE_MEDICINES = n_distinct(
        ifelse(
          test = substr(BNF_CHEMICAL_SUBSTANCE, 1, 2) %in%
            c(01, 02, 03, 04, 06, 07, 08, 09, 10),
            # c(paste0("0", as.character(1:9)), "10"),
          yes = CHEMICAL_SUBSTANCE_BNF_DESCR,
          no = NA_character_
        )
      )
    ) %>%
    ungroup(YEAR_MONTH, NHS_NO) %>%
    summarise(
      # Items and cost
      TOTAL_PATIENTS = n_distinct(NHS_NO), # For SDC
      ITEMS_PER_PATIENT_MONTH = mean(TOTAL_ITEMS),
      COST_PER_PATIENT_MONTH = mean(TOTAL_COST),
      # Unique medicines
      TOTAL_PATIENTS_UNIQUE_MEDICINES = n_distinct(
        ifelse(
          test = UNIQUE_MEDICINES > 1,
          yes = NHS_NO,
          NA
        )
      ),
      UNIQUE_MEDICINES_PER_PATIENT_MONTH = mean(UNIQUE_MEDICINES),
      TOTAL_PATIENTS_TEN_OR_MORE = n_distinct( # For SDC
        ifelse(
          test = UNIQUE_MEDICINES >= 10,
          yes = NHS_NO,
          NA
        )
      )
    ) %>%
    ungroup() %>%
    mutate(
      PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = ifelse(
        test = TOTAL_PATIENTS_UNIQUE_MEDICINES == 0,
        yes = NA_real_,
        no = TOTAL_PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_UNIQUE_MEDICINES * 100
      )
    ) %>%
    nhsbsaR::collect_with_parallelism(32)
}

## Process ----------------------------------------------------------------

### Create local df -------------------------------------------------------
metrics_by_geo_and_ch_flag_df <- names(geographies)[2:4] %>% 
  purrr::map(metrics_by_geo_and_ch_flag) %>%
  purrr::list_rbind()

### Complete  -------------------------------------------------------------
metrics_by_geo_and_ch_flag_df <- metrics_by_geo_and_ch_flag_df %>%
  tidyr::complete(
    # Only geographies that already exist
    tidyr::nesting(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE,
      SUB_GEOGRAPHY_NAME
    ),
    # All years, months and CH flags
    FY,
    CH_FLAG,
    fill = list(
      TOTAL_PATIENTS = 0L,
      ITEMS_PER_PATIENT_MONTH = NA_real_,
      COST_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_UNIQUE_MEDICINES = 0L,
      UNIQUE_MEDICINES_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_TEN_OR_MORE = 0L,
      PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = NA_real_
    )
  )

### Statistical disclosure control ----------------------------------------
metrics_by_geo_and_ch_flag_df <- metrics_by_geo_and_ch_flag_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS, -1) # rounding to nearest 10; where are our rules for SDC?
    ),
    SDC_ITEMS_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(ITEMS_PER_PATIENT_MONTH, 1)
    ),
    SDC_COST_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(COST_PER_PATIENT_MONTH)
    ),
    SDC = ifelse(TOTAL_PATIENTS_UNIQUE_MEDICINES %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_UNIQUE_MEDICINES = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_UNIQUE_MEDICINES, -1) # rounding to nearest 10; where are our rules for SDC?
    ),
    SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(UNIQUE_MEDICINES_PER_PATIENT_MONTH, 1)
    ),
    SDC = ifelse(TOTAL_PATIENTS_TEN_OR_MORE %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_TEN_OR_MORE = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_TEN_OR_MORE, -1) # rounding to nearest 10; where are our rules for SDC?
    ),
    SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH, 1)
    )
  ) %>%
  select(-SDC)

### Format ----------------------------------------------------------------
metrics_by_geo_and_ch_flag_df <- metrics_by_geo_and_ch_flag_df %>%
  format_data_raw("CH_FLAG") %>% 
  suppressWarnings() # We do not have Overall and PCN in this data

### Save ------------------------------------------------------------------
usethis::use_data(metrics_by_geo_and_ch_flag_df, overwrite = TRUE)

### Cleanup ---------------------------------------------------------------
DBI::dbDisconnect(con)
