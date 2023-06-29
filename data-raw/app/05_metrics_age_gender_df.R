tictoc::tic()
# Running time ~1.5 hrs

library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |>
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# Add ACB_CAT and DAMN_FLAG
base_db <- base_db |> mutate(
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
)


metrics_by_age_gender_and_ch_flag_df <- base_db |>
  group_by(
    FY,
    GENDER,
    AGE_BAND,
    CH_FLAG,
    YEAR_MONTH,
    NHS_NO
  ) |>
  # Sums per patient month
  summarise(
    TOTAL_ITEMS = sum(ITEM_COUNT),
    TOTAL_COST = sum(ITEM_PAY_DR_NIC / 100),
    UNIQUE_MEDICINES = n_distinct(
      ifelse(substr(BNF_CHEMICAL_SUBSTANCE, 1, 2) %in% c(01, 02, 03, 04, 06, 07, 08, 09, 10),
             CHEMICAL_SUBSTANCE_BNF_DESCR,
             NA_character_)
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
    
  ) |>
  # Calculate the metrics per patient month
  ungroup(YEAR_MONTH, NHS_NO) |>
  summarise(
    TOTAL_PATIENTS = n_distinct(NHS_NO), # For SDC
    # Items and cost
    ITEMS_PER_PATIENT_MONTH = mean(TOTAL_ITEMS),
    COST_PER_PATIENT_MONTH = mean(TOTAL_COST),
    # Unique medicines
    TOTAL_PATIENTS_TWO_OR_MORE = n_distinct(
      ifelse(UNIQUE_MEDICINES > 1, NHS_NO, NA)
      ),
    UNIQUE_MEDICINES_PER_PATIENT_MONTH = mean(UNIQUE_MEDICINES),
    TOTAL_PATIENTS_SIX_OR_MORE = n_distinct(
      ifelse(UNIQUE_MEDICINES >= 6, NHS_NO, NA)
    ),
    TOTAL_PATIENTS_TEN_OR_MORE = n_distinct(
      ifelse(UNIQUE_MEDICINES >= 10, NHS_NO, NA)
    ),
    # ACB
    TOTAL_PATIENTS_ACB_6 = n_distinct(
      ifelse(ACB_6 == 1, NHS_NO, NA_integer_)
    ),
    # DAMN
    TOTAL_PATIENTS_DAMN = n_distinct(
      ifelse(DAMN == 1, NHS_NO, NA_integer_)
    ),
    .groups = "drop") |>
  mutate(
    PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH = ifelse(
      TOTAL_PATIENTS_TWO_OR_MORE == 0,
      NA_real_,
      TOTAL_PATIENTS_SIX_OR_MORE / TOTAL_PATIENTS_TWO_OR_MORE * 100
    ),
    PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = ifelse(
      TOTAL_PATIENTS_TWO_OR_MORE == 0,
      NA_real_,
      TOTAL_PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_TWO_OR_MORE * 100
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
  ) |>
  nhsbsaR::collect_with_parallelism(10)

# Get all the possible combinations
metrics_by_age_gender_and_ch_flag_df <- metrics_by_age_gender_and_ch_flag_df |>
  tidyr::complete(
    tidyr::nesting(FY, GENDER, AGE_BAND),
    CH_FLAG,
    fill = list(
      TOTAL_PATIENTS = 0L,
      ITEMS_PER_PATIENT_MONTH = NA_real_,
      COST_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_TWO_OR_MORE = 0L,
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

# Apply SDC to the metrics based on the total patients
metrics_by_age_gender_and_ch_flag_df <- metrics_by_age_gender_and_ch_flag_df |>
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), TRUE, FALSE),
    SDC_TOTAL_PATIENTS = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS, -1)
    ),
    SDC_ITEMS_PER_PATIENT_MONTH =
      ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(ITEMS_PER_PATIENT_MONTH, 1)
      ),
    SDC_COST_PER_PATIENT_MONTH =
      ifelse(
        SDC == 1L,
        NA_integer_,
        janitor::round_half_up(COST_PER_PATIENT_MONTH)
      ),
    # SDC based on patients on patients on 2+ medicines
    SDC = ifelse(TOTAL_PATIENTS_TWO_OR_MORE %in% c(1, 2, 3, 4), TRUE, FALSE),
    SDC_TOTAL_PATIENTS_TWO_OR_MORE = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_TWO_OR_MORE, -1)
    ),
    SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH =
      ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(UNIQUE_MEDICINES_PER_PATIENT_MONTH, 1)
      ),
    # SDC based on patients on patients on 6+ medicines
    SDC = ifelse(TOTAL_PATIENTS_SIX_OR_MORE %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_SIX_OR_MORE = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_SIX_OR_MORE, -1)
    ),
    SDC_PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_SIX_OR_MORE_PER_PATIENT_MONTH, 1)
    ),
    # SDC based on patients on patients on 10+ medicines
    SDC = ifelse(TOTAL_PATIENTS_TEN_OR_MORE %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_TEN_OR_MORE = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_TEN_OR_MORE, -1)
    ),
    SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH, 1)
    ),
    # SDC based on patients on patients with ACB_6 flag
    SDC = ifelse(TOTAL_PATIENTS_ACB_6 %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_ACB_6 = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_ACB_6, -1)
    ),
    SDC_PCT_PATIENTS_ACB_6_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_ACB_6_PER_PATIENT_MONTH, 1)
    ),
    # SDC based on patients on patients with DAMN flag
    SDC = ifelse(TOTAL_PATIENTS_DAMN %in% 1:4, TRUE, FALSE),
    SDC_TOTAL_PATIENTS_DAMN = ifelse(
      SDC,
      NA_integer_,
      round(TOTAL_PATIENTS_DAMN, -1)
    ),
    SDC_PCT_PATIENTS_DAMN_PER_PATIENT_MONTH = ifelse(
      SDC,
      NA_integer_,
      janitor::round_half_up(PCT_PATIENTS_DAMN_PER_PATIENT_MONTH, 1)
    )
  ) |>
  select(-SDC)

# Format for highcharter
metrics_by_age_gender_and_ch_flag_df <- metrics_by_age_gender_and_ch_flag_df |>
  format_data_raw(c("GENDER", "AGE_BAND", "CH_FLAG"))

# Add to data/
usethis::use_data(
  metrics_by_age_gender_and_ch_flag_df,
  overwrite = T
)

# Disconnect from database
DBI::dbDisconnect(con)

tictoc::toc()
