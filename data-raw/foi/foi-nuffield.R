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

saveRDS(foi_yearly_df, "foi-nuffield-yearly.rds")

foi_yearly_final_df <- foi_yearly_df %>% 
  filter(!is.na(IMD_DECILE)) %>% 
  mutate(
    TOTAL_PATS = bespoke_round(TOTAL_PATS),
    TOTAL_ITEMS = bespoke_round(TOTAL_ITEMS),
    TOTAL_COST = bespoke_round(TOTAL_COST)
  )

saveRDS(foi_yearly_final_df, "foi-final-nuffield-yearly.rds")


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

saveRDS(foi_monthly_df, "foi-nuffield-monthly.rds")

foi_monthly_final_df <- foi_monthly_df %>% 
  filter(!is.na(IMD_DECILE)) %>% 
  mutate(
    TOTAL_PATS = bespoke_round(TOTAL_PATS),
    TOTAL_ITEMS = bespoke_round(TOTAL_ITEMS),
    TOTAL_COST = bespoke_round(TOTAL_COST)
  )

saveRDS(foi_monthly_final_df, "foi-final-nuffield-monthly.rds")

foi_final_df <- bind_rows(
  foi_yearly_final_df %>% mutate(PERIOD_TYPE = "ANNUALLY"),
  foi_monthly_final_df %>% mutate(PERIOD_TYPE = "MONTHLY")
) %>% 
  mutate(
    "Period type" = PERIOD_TYPE,
    "Financial year" = FY,
    "Year Month (YYYYMM)" = YEAR_MONTH,
    "Age band" = AGE_BAND,
    "Gender" = GENDER,
    "Care-home" = CH_FLAG,
    "IMD decile" = IMD_DECILE,
    "Chapter description" = CHAPTER_DESCR,
    "Total items" = TOTAL_ITEMS,  
    "Total cost" = TOTAL_COST,
    "Total distinct patients" = TOTAL_PATS,
    .keep = "none"
  )

saveRDS(foi_final_df, "foi-final-nuffield.rds")

write.csv(foi_final_df, "foi-nuffield.csv", row.names = FALSE)
