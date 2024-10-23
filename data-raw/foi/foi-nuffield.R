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
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20240331"))

foi_df <- base_db %>% 
  summarise(
    TOTAL_PATIENTS = n_distinct(NHS_NO),
    TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    TOTAL_COST = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100,
    .by = c(CH_FLAG, AGE_BAND, GENDER, IMD_DECILE, CHAPTER_DESCR)
  ) %>% 
  collect()

saveRDS(foi_df, "foi-nuffield.rds")

foi_final_df <- foi_df %>% 
  filter(!is.na(IMD_DECILE)) %>% 
  mutate(
    TOTAL_PATIENTS = case_when(
      TOTAL_PATIENTS < 5 ~ NA_integer_,
      TRUE ~ TOTAL_PATIENTS
    ),
    TOTAL_ITEMS = case_when(
      is.na(TOTAL_PATIENTS) ~ NA_integer_,
      TRUE ~ TOTAL_ITEMS
    ),
    TOTAL_COST = case_when(
      is.na(TOTAL_PATIENTS) ~ NA_real_,
      TRUE ~ TOTAL_COST
    )
  )

saveRDS(foi_final_df, "foi-final-nuffield.rds")

perc_redacted <- round(
  100 * nrow(filter(foi_final_df, is.na(TOTAL_PATIENTS))) / nrow(foi_final_df)
)

foi_mf_only_df <- foi_final_df %>% 
  filter(
    GENDER %in% c("Male", "Female"),
    !is.na(IMD_DECILE)
  )
