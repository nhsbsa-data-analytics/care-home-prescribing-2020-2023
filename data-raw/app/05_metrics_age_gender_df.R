tictoc::tic()
# Running time ~x min

library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |>
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))


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
             yes = CHEMICAL_SUBSTANCE_BNF_DESCR,
             no = NA_character_
             )
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
    TOTAL_PATIENTS_ON_2_PLUS_MEDICINES = n_distinct(
      ifelse(UNIQUE_MEDICINES > 1, NHS_NO, NA)
      ),
    UNIQUE_MEDICINES_PER_PATIENT_MONTH = mean(UNIQUE_MEDICINES),
    .groups = "drop") |>
  nhsbsaR::collect_with_parallelism(10)

tictoc::toc()

# Get all the possible combinations
metrics_by_age_gender_and_ch_flag_df <- metrics_by_age_gender_and_ch_flag_df |>
  tidyr::complete(
    tidyr::nesting(FY, GENDER, AGE_BAND),
    CH_FLAG,
    fill = list(
      TOTAL_PATIENTS = 0L,
      ITEMS_PER_PATIENT_MONTH = NA_real_,
      COST_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_ON_2_PLUS_MEDICINES = 0L,
      UNIQUE_MEDICINES_PER_PATIENT_MONTH = NA_real_
    )
  )

# Apply SDC to the metrics based on the total patients
metrics_by_age_gender_and_ch_flag_df <- metrics_by_age_gender_and_ch_flag_df |>
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS = ifelse(
      test = SDC == 1,
      yes = NA_integer_,
      no = round(TOTAL_PATIENTS, -1)
    ),
    SDC_ITEMS_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1,
        yes = NA_integer_,
        no = janitor::round_half_up(ITEMS_PER_PATIENT_MONTH, 1)
      ),
    SDC_COST_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1L,
        yes = NA_integer_,
        no = janitor::round_half_up(COST_PER_PATIENT_MONTH)
      ),
    SDC = ifelse(TOTAL_PATIENTS_ON_2_PLUS_MEDICINES %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS_UNIQUE_MEDICINES = ifelse(
      test = SDC == 1,
      yes = NA_integer_,
      no = round(TOTAL_PATIENTS_ON_2_PLUS_MEDICINES, -1)
    ),
    SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1,
        yes = NA_integer_,
        no = janitor::round_half_up(UNIQUE_MEDICINES_PER_PATIENT_MONTH, 1)
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
