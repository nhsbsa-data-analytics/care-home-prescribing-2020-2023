# Running time ~x min

library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |>
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))


metrics_by_breakdown_and_ch_flag_df <- base_db |>
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
    UNIQUE_MEDICINES_PER_PATIENT_MONTH = mean(UNIQUE_MEDICINES),
    .groups = "drop") |>
  nhsbsaR::collect_with_parallelism(32)


# Add to data/
# usethis::use_data(
#   patients_by_fy_geo_age_gender_df,
#   overwrite = T
# )

# Disconnect from database
DBI::dbDisconnect(con)
