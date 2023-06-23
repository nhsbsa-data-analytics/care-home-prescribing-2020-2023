# Initial setup -----------------------------------------------------------

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
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))


## Process ----------------------------------------------------------------

### Collect data ----------------------------------------------------------
metrics_by_ch_type_df <- base_db

metrics_by_ch_type <- metrics_by_ch_type_df %>% 
  transmute(
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
    CHEMICAL_SUBSTANCE_BNF_DESCR,
    ACB_CAT = case_when(
      BNF_CHEMICAL_SUBSTANCE %in% acb1_drugs ~ 1,
      BNF_CHEMICAL_SUBSTANCE %in% acb2_drugs ~ 2,
      BNF_CHEMICAL_SUBSTANCE %in% acb3_drugs ~ 3,
      TRUE ~ 0
    ),
    DAMN_FLAG = case_when(
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^100101') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205051') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205052') > 0 ~ 1,
      BNF_CHEMICAL_SUBSTANCE %in% other_drug_vec ~ 1,
      TRUE ~ 0
    )
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
    ),
    ACB_6 = case_when( 
      (1 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1])) + 
        (2 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 2])) + 
        (3 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 3])) >= 6 ~ 1,
      TRUE ~ 0
    ),
    DAMN = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE[DAMN_FLAG == 1]) >= 2 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>% 
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
    TOTAL_PATIENTS_SIX_OR_MORE = n_distinct( # For SDC
      ifelse(
        UNIQUE_MEDICINES >= 6,
        NHS_NO,
        NA_integer_
      )
    ),
    TOTAL_PATIENTS_TEN_OR_MORE = n_distinct( # For SDC
      ifelse(
        UNIQUE_MEDICINES >= 10,
        NHS_NO,
        NA_integer_
      )
    ),
    # ACB
    TOTAL_PATIENTS_ACB_6 = n_distinct(
      ifelse(
        ACB_6 == 1,
        NHS_NO,
        NA_integer_
      )
    ),
    # DAMN
    TOTAL_PATIENTS_DAMN = n_distinct(
      ifelse(
        DAMN == 1,
        NHS_NO,
        NA_integer_
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH = ifelse(
      TOTAL_PATIENTS_UNIQUE_MEDICINES == 0,
      NA_real_,
      TOTAL_PATIENTS_SIX_OR_MORE / TOTAL_PATIENTS_UNIQUE_MEDICINES * 100
    ),
    PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = ifelse(
      TOTAL_PATIENTS_UNIQUE_MEDICINES == 0,
      NA_real_,
      TOTAL_PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_UNIQUE_MEDICINES * 100
    ),
    PCT_PATIENTS_ACB_6_PER_PATIENT_MONTH = ifelse(
      TOTAL_PATIENTS_ACB_6 == 0,
      NA_real_,
      TOTAL_PATIENTS_ACB_6 / TOTAL_PATIENTS * 100
    ),
    PCT_PATIENTS_DAMN_PER_PATIENT_MONTH = ifelse(
      TOTAL_PATIENTS_DAMN == 0,
      NA_real_,
      TOTAL_PATIENTS_DAMN / TOTAL_PATIENTS * 100
    )
  ) %>%
  nhsbsaR::collect_with_parallelism(8)
  
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
      TOTAL_PATIENTS_SIX_OR_MORE = 0L,
      PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_TEN_OR_MORE = 0L,
      PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_ACB_6 = 0L,
      PCT_PATIENTS_ACB_6_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_DAMN = 0L,
      PCT_PATIENTS_DAMN_PER_PATIENT_MONTH = NA_real_
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
    SDC = ifelse(TOTAL_PATIENTS_SIX_OR_MORE %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_SIX_OR_MORE = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_SIX_OR_MORE, -1) # rounding to nearest 10; where are our rules for SDC?
    ),
    SDC_PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH, 1)
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
    ),
    SDC = ifelse(TOTAL_PATIENTS_ACB_6 %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_ACB_6 = ifelse(
      SDC,
      NA_integer_,
      round(SDC_TOTAL_PATIENTS_ACB_6, -1) # rounding to nearest 10; where are our rules for SDC?
    ),
    SDC_PCT_PATIENTS_ACB_6_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_ACB_6_PER_PATIENT_MONTH, 1)
    ),
    SDC = ifelse(TOTAL_PATIENTS_DAMN %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_DAMN = ifelse(
      SDC,
      NA_integer_,
      round(SDC_TOTAL_PATIENTS_DAMN, -1) # rounding to nearest 10; where are our rules for SDC?
    ),
    SDC_PCT_PATIENTS_DAMN_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_DAMN_PER_PATIENT_MONTH, 1)
    )
  ) %>%
  select(-SDC)


### Keep only relevant columns / rename -----------------------------------
metrics_by_ch_type <- metrics_by_ch_type %>% 
  transmute(
    FY,
    CH_TYPE,
    TOTAL_PATIENTS = SDC_TOTAL_PATIENTS,
    ITEMS_PPM = SDC_ITEMS_PER_PATIENT_MONTH,
    COST_PPM = SDC_COST_PER_PATIENT_MONTH,
    TOTAL_PATIENTS_UNIQ_MED = SDC_TOTAL_PATIENTS_UNIQUE_MEDICINES,
    UNIQ_MEDS_PPM = SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH,
    TOTAL_PATIENTS_GTE_SIX = SDC_TOTAL_PATIENTS_SIX_OR_MORE,
    PCT_PX_GTE_SIX_PPM = SDC_PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH,
    TOTAL_PATIENTS_GTE_TEN = SDC_TOTAL_PATIENTS_TEN_OR_MORE,
    PCT_PX_GTE_TEN_PPM = SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH,
    TOTAL_PATIENTS_ACB_6 = SDC_TOTAL_PATIENTS_ACB_6,
    PCT_PX_ACB_6_PPM = SDC_PCT_PATIENTS_ACB_6_PER_PATIENT_MONTH,
    TOTAL_PATIENTS_DAMN = SDC_TOTAL_PATIENTS_DAMN,
    PCT_PX_DAMN_PPM = SDC_PCT_PATIENTS_DAMN_PER_PATIENT_MONTH
  )
  
## Save -------------------------------------------------------------------
usethis::use_data(metrics_by_ch_type, overwrite = TRUE)

## Cleanup ----------------------------------------------------------------
DBI::dbDisconnect(con)
