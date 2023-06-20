# Initial setup -----------------------------------------------------------

library(dplyr)
library(dbplyr)
library(tidyr)
library(stringr)
library(glue)
library(purrr)
devtools::load_all()

# Data validation ---------------------------------------------------------


## Setup ------------------------------------------------------------------

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# PCD <- con %>%
#   tbl(from = "INT646_POSTCODE_LOOKUP") %>%
#   select(ends_with("CODE"), ends_with("NAME")) %>%
#   distinct() %>%
#   collect()
# 
# transform_PCD <- function(data, geography) {
#   data %>%
#     select(starts_with(glue("PCD_{geography}"))) %>% 
#     distinct() %>%
#     rename_with(
#       \(x) str_replace(x, glue("PCD_{geography}"), "SUB_GEOGRAPHY")
#     ) %>%
#     filter(!is.na(SUB_GEOGRAPHY_NAME))
# }
# 
# PCD_list <- list(
#   REGION = PCD %>% transform_PCD("REGION"),
#   ICB    = PCD %>% transform_PCD("ICB"),
#   LAD    = PCD %>% transform_PCD("LAD")
# )
# 
# GIS_list <- geo_data_validation
# 
# # Check sub-geography codes and names match exactly between PCD and GIS; you
# # should get character(0) for in_GIS_only and NA or character(0) for in_PCD_only
# 
# ## Check sub-geography codes ----------------------------------------------
# 
# check_sub_geo_codes <- list(
#   in_GIS_only = map2(
#     GIS_list,
#     PCD_list,
#     \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
#   ),
#   in_PCD_only = map2(
#     PCD_list,
#     GIS_list,
#     \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
#   )
# ) %>% print()
# 
# 
# ## Check sub-geography names ----------------------------------------------
# 
# check_sub_geo_names <- list(
#   in_GIS_only = map2(
#     GIS_list,
#     PCD_list,
#     \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
#   ),
#   in_PCD_only = map2(
#     PCD_list,
#     GIS_list,
#     \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
#   )
# ) %>% print()

# Data prep ---------------------------------------------------------------

## Setup ------------------------------------------------------------------

# Item-level base table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))


## Process ----------------------------------------------------------------

### Collect data ----------------------------------------------------------
metrics_by_ch_type_df <- base_db

metrics_by_ch_type <- metrics_by_ch_type_df %>% 
  dplyr::transmute(
    FY,
    YEAR_MONTH,
    CH_FLAG = CH_FLAG,
    NON_CH_FLAG = 1 - CH_FLAG,
    NH_FLAG = NURSING_HOME_FLAG,
    RH_FLAG = RESIDENTIAL_HOME_FLAG,
    NHS_NO,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC,
    BNF_CHEMICAL_SUBSTANCE,
    CHEMICAL_SUBSTANCE_BNF_DESCR
  ) %>%
  pivot_longer(
    ends_with("FLAG"),
    names_to = "CH_TYPE"
  ) %>%
  filter(value == 1L) %>%
  select(-value) %>%
  mutate(
    CH_TYPE = case_match(
      CH_TYPE,
      "CH_FLAG"     ~ "Carehome",
      "NH_FLAG"     ~ "Nursing Home",
      "RH_FLAG"     ~ "Residential Home",
      "NON_CH_FLAG" ~ "Non-carehome"
    )
  ) %>%  
  group_by(
    FY, YEAR_MONTH, CH_TYPE, NHS_NO
  ) %>%
  summarise(
    TOTAL_ITEMS = sum(ITEM_COUNT),
    TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
    UNIQUE_MEDICINES = n_distinct(
      ifelse(
        as.integer(substr(BNF_CHEMICAL_SUBSTANCE, 1, 2)) %in% c(1:4, 6:10),
        CHEMICAL_SUBSTANCE_BNF_DESCR,
        NA_character_
      )
    )
  ) %>%
  group_by(FY, CH_TYPE) %>%
  summarise(
    # Items and cost
    TOTAL_PATIENTS = n_distinct(NHS_NO), # For SDC
    ITEMS_PER_PATIENT_MONTH = mean(TOTAL_ITEMS),
    COST_PER_PATIENT_MONTH = mean(TOTAL_COST),
    # Unique medicines
    TOTAL_PATIENTS_UNIQUE_MEDICINES = n_distinct(
      ifelse(
        UNIQUE_MEDICINES > 1,
        NHS_NO,
        NA_integer_
      )
    ),
    UNIQUE_MEDICINES_PER_PATIENT_MONTH = mean(UNIQUE_MEDICINES),
    TOTAL_PATIENTS_TEN_OR_MORE = n_distinct( # For SDC
      ifelse(
        UNIQUE_MEDICINES >= 10,
        NHS_NO,
        NA_integer_
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = ifelse(
      TOTAL_PATIENTS_UNIQUE_MEDICINES == 0,
      NA_real_,
      TOTAL_PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_UNIQUE_MEDICINES * 100
    )
  ) %>%
  nhsbsaR::collect_with_parallelism(8)
  
# metrics_by_ch_type <- bind_rows(
#   metrics_by_ch_type,
#   metrics_by_ch_type %>% mutate(FY = "2021/22"),
#   metrics_by_ch_type %>% mutate(FY = "2022/23")
# )

### Complete --------------------------------------------------------------
metrics_by_ch_type_complete <- metrics_by_ch_type %>% 
  tidyr::complete(
    FY,
    CH_TYPE,
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
metrics_by_ch_type <- metrics_by_ch_type %>%
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
# metrics_by_ch_type <- metrics_by_ch_type %>%
#   mutate(CH_FLAG = as.logical(CH_FLAG)) %>% 
#   filter(!is.na(SUB_GEOGRAPHY_NAME)) %>% 
#   format_data_raw("CH_FLAG") %>% 
#   suppressWarnings() # We do not have Overall and PCN in this data


### Keep only relevant columns / rename -----------------------------------
metrics_by_ch_type <- metrics_by_ch_type %>% 
  dplyr::transmute(
    FY,
    CH_TYPE,
    TOTAL_PATIENTS = SDC_TOTAL_PATIENTS,
    ITEMS_PPM = SDC_ITEMS_PER_PATIENT_MONTH,
    COST_PPM = SDC_COST_PER_PATIENT_MONTH,
    UNIQ_MEDS_PPM = SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH,
    TOTAL_PATIENTS_GTE_TEN = SDC_TOTAL_PATIENTS_TEN_OR_MORE,
    PCT_PX_GTE_TEN_PPM = SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH
  )
  
## Save -------------------------------------------------------------------
usethis::use_data(metrics_by_ch_type, overwrite = TRUE)

## Cleanup ----------------------------------------------------------------
DBI::dbDisconnect(con)
