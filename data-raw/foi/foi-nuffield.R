# Initial setup -----------------------------------------------------------

# Expected run time ~35 minutes @parallel 36
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


## Query 1: Yearly --------------------------------------------------------

# REQUEST:
# For financial years from 2020/21 to 2022/23, provide totals of
#     item count
#     cost
#     patients
# aggregated by
#     financial year
#     age group
#     sex
#     care home setting (care-home or non-care home)
#     deprivation
#     BNF chapter

foi_yearly_df <- base_db %>% 
  summarise(
    TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    TOTAL_COST = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100,
    TOTAL_PATS = n_distinct(NHS_NO),
    .by = c(
      FY,
      AGE_BAND,
      GENDER,
      CH_FLAG,
      IMD_DECILE,
      CHAPTER_DESCR
    )
  ) %>% 
  collect()

saveRDS(foi_df, "foi-nuffield-yearly.rds")

foi_yearly_final_df <- foi_yearly_df %>% 
  filter(!is.na(IMD_DECILE)) %>% 
  mutate(
    TOTAL_PATS = case_when(
      TOTAL_PATS < 5 ~ NA_integer_,
      TRUE ~ bespoke_round(TOTAL_PATS)
    ),
    TOTAL_ITEMS = case_when(
      is.na(TOTAL_PATS) ~ NA_integer_,
      TRUE ~ bespoke_round(TOTAL_ITEMS)
    ),
    TOTAL_COST = case_when(
      is.na(TOTAL_PATS) ~ NA_real_,
      TRUE ~ bespoke_round(TOTAL_COST)
    )
  )

saveRDS(foi_yearly_final_df, "foi-final-nuffield-yearly.rds")

perc_redacted_yearly <- round(
  100 * nrow(filter(foi_yearly_final_df, is.na(TOTAL_PATS))) / nrow(foi_yearly_final_df)
)

## Query 2: Monthly --------------------------------------------------------

# REQUEST:
# For financial years from 2020/21 to 2022/23, provide totals of
#     item count
#     cost
#     patients
# aggregated by
#     year_month
#     age group
#     sex
#     care home setting (care-home or non-care home)
#     deprivation
#     BNF chapter

foi_monthly_df <- base_db %>% 
  summarise(
    TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    TOTAL_COST = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100,
    TOTAL_PATS = n_distinct(NHS_NO),
    .by = c(
      YEAR_MONTH,
      AGE_BAND,
      GENDER,
      CH_FLAG,
      IMD_DECILE,
      CHAPTER_DESCR
    )
  ) %>% 
  collect()

saveRDS(foi_df, "foi-nuffield-monthly.rds")

foi_monthly_final_df <- foi_monthly_df %>% 
  filter(!is.na(IMD_DECILE)) %>% 
  mutate(
    TOTAL_PATS = case_when(
      TOTAL_PATS < 5 ~ NA_integer_,
      TRUE ~ bespoke_round(TOTAL_PATS)
    ),
    TOTAL_ITEMS = case_when(
      is.na(TOTAL_PATS) ~ NA_integer_,
      TRUE ~ bespoke_round(TOTAL_ITEMS)
    ),
    TOTAL_COST = case_when(
      is.na(TOTAL_PATS) ~ NA_real_,
      TRUE ~ bespoke_round(TOTAL_COST)
    )
  )

saveRDS(foi_monthly_final_df, "foi-final-nuffield-monthly.rds")

perc_redacted_monthly <- round(
  100 * nrow(filter(foi_monthly_final_df, is.na(TOTAL_PATS))) / nrow(foi_monthly_final_df)
)
