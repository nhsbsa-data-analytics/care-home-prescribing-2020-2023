library(dplyr)
library(dbplyr)
#library(highcharter)
library(ggplot2)
#devtools::install_github('nhsbsa-data-analytics/nhsbsaR')
#source("data-raw/app/data_raw_helpers.R")

con <- nhsbsaR::con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("DALL_REF", "INT646_BASE_20200401_20230331")) |>
  filter(FY == "2022/23" &
           UPRN_FLAG==1)


get_metrics <- function(init_db,
                        first_grouping,
                        second_grouping,
                        num_parallel = 32) {
  # Collect data and calculate raw metrics
  init_db |> 
    group_by(across(all_of(first_grouping))) |>
    summarise(
      ANY_ACB = any(
        case_when(
          ACB_CAT > 0 ~ 1L,
          TRUE ~ 0L
        ),
        na.rm = TRUE
      ),
      ACB = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1]) >= 2 ~ 1L,
        TRUE ~ 0L
      ),
      ANY_DAMN = any(
        case_when(
          DAMN_CAT > 0 ~ 1L,
          TRUE ~ 0L
        ),
        na.rm = TRUE
      ),
      DAMN = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[DAMN_CAT == 1]) >= 2 ~ 1L,
        TRUE ~ 0L
      ),
      # FALLS = case_when(
      #   n_distinct(BNF_CHEMICAL_SUBSTANCE[FALLS_CAT == 1]) >= 3 ~ 1L,
      #   TRUE ~ 0L
      # ),
      UNIQUE_MEDICINES_FALLS = n_distinct(
        case_when(
          FALLS_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      )
    ) |>
    ungroup() |> 
    group_by(across(all_of(second_grouping))) |>
    summarise(
      # Total patient months - use for denominators
      TOTAL_PM            = n(),
      TOTAL_PATIENTS      = n_distinct(NHS_NO),
      TOTAL_PM_ACB        = sum(ANY_ACB, na.rm = TRUE),
      TOTAL_PM_DAMN       = sum(ANY_DAMN, na.rm = TRUE),
      # Items, cost and unique meds count
      ITEMS_PPM           = mean(TOTAL_ITEMS, na.rm = TRUE),
      COST_PPM            = mean(TOTAL_COST, na.rm = TRUE),
      UNIQ_MEDS_PPM       = mean(UNIQUE_MEDICINES, na.rm = TRUE),
      # Unique medicines numerators
      RISK_PM_GTE_SIX     = sum(GTE_SIX, na.rm = TRUE),
      RISK_PM_GTE_TEN     = sum(GTE_TEN, na.rm = TRUE),
      # ACB numerator
      RISK_PM_ACB         = sum(ACB, na.rm = TRUE),
      # DAMN numerator
      RISK_PM_DAMN        = sum(DAMN, na.rm = TRUE),
      # Falls unique medicines
      UNIQ_MEDS_FALLS_PPM = mean(UNIQUE_MEDICINES_FALLS, na.rm = TRUE),
      # Falls numerator
      # RISK_PM_FALLS       = sum(FALLS, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      # Calculate % metrics - each denominator is restricted to patients on any
      # med that is also a condition to be included in numerator
      PCT_PM_ACB = 100 * case_when(
        TOTAL_PM_ACB == 0 ~ NA,
        TRUE ~ RISK_PM_ACB / TOTAL_PM_ACB
      ),
      PCT_PM_DAMN = 100 * case_when(
        TOTAL_PM_DAMN == 0 ~ NA,
        TRUE ~ RISK_PM_DAMN / TOTAL_PM_DAMN
      )#,
      # PCT_PM_FALLS = 100 * case_when(
      #   TOTAL_PM == 0 ~ NA,
      #   TRUE ~ RISK_PM_FALLS / TOTAL_PM
      # )
    ) |>
    nhsbsaR::collect_with_parallelism(num_parallel)
  
}

# Running time <= 1 min
tictoc::tic()
df <- get_metrics(
  DB,
  first_grouping = c(
    "YEAR_MONTH",
    "NHS_NO",
    "UPRN",
    "MATCH_SLA_STD"
  ),
  second_grouping = c(
    "UPRN",
    "MATCH_SLA_STD"
  )
)
tictoc::toc()



DBI::dbDisconnect(con)