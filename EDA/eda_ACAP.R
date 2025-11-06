
library(dbplyr)
library(dplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level FACT table
base_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20250331"))



a = base_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(FY, YEAR_MONTH, NHS_NO) %>% 
  summarise(
    ANY_ACAP = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE[(PARAGRAPH_DESCR == 'Oral anticoagulants' | PARAGRAPH_DESCR == 'Antiplatelet drugs') & CHEMICAL_SUBSTANCE_BNF_DESCR != 'INR blood testing reagents']) >= 1 ~ 1L,
      TRUE ~ 0L
    ),
    ACAP_TWO = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE[(PARAGRAPH_DESCR == 'Oral anticoagulants' | PARAGRAPH_DESCR == 'Antiplatelet drugs') & CHEMICAL_SUBSTANCE_BNF_DESCR != 'INR blood testing reagents']) >= 2 ~ 1L,
      TRUE ~ 0L
    ),
    ACAP_THREE = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE[(PARAGRAPH_DESCR == 'Oral anticoagulants' | PARAGRAPH_DESCR == 'Antiplatelet drugs') & CHEMICAL_SUBSTANCE_BNF_DESCR != 'INR blood testing reagents']) >= 3 ~ 1L,
      TRUE ~ 0L
    )
  ) %>% 
  ungroup() %>% 
  collect()

a %>% 
  group_by(FY) %>% 
  summarise(
    ANY_ACAP = sum(ANY_ACAP),
    ACAP_TWO = sum(ACAP_TWO),
    ACAP_THREE = sum(ACAP_THREE)
  ) %>% 
  ungroup() %>% 
  mutate(
    PCT_ACAP_TWO = 100 * case_when(
      ANY_ACAP == 0 ~ NA,
      TRUE ~ ACAP_TWO / ANY_ACAP
    ),
    PCT_ACAP_THREE = 100 * case_when(
      ANY_ACAP == 0 ~ NA,
      TRUE ~ ACAP_THREE / ANY_ACAP
    )
  )

b = base_db %>% 
  filter(CH_FLAG == 1) %>% 
  group_by(FY, YEAR_MONTH, NHS_NO) %>% 
  summarise(
    UNIQUE_ACAP = n_distinct(
      case_when(
        (PARAGRAPH_DESCR == 'Oral anticoagulants' | PARAGRAPH_DESCR == 'Antiplatelet drugs') & CHEMICAL_SUBSTANCE_BNF_DESCR != 'INR blood testing reagents' ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
        ),
      na.rm = TRUE
      )
    ) %>% 
  ungroup() %>% 
  collect()

b %>% 
  group_by(FY) %>% 
  summarise(
    ANY_ACAP = sum(ifelse(UNIQUE_ACAP >= 1, 1, 0)),
    ACAP_TWO = sum(ifelse(UNIQUE_ACAP >= 2, 1, 0)),
    ACAP_THREE = sum(ifelse(UNIQUE_ACAP >= 3, 1, 0)),
  ) %>% 
  ungroup() %>% 
  ungroup() %>% 
  mutate(
    PCT_ACAP_TWO = 100 * case_when(
      ANY_ACAP == 0 ~ NA,
      TRUE ~ ACAP_TWO / ANY_ACAP
    ),
    PCT_ACAP_THREE = 100 * case_when(
      ANY_ACAP == 0 ~ NA,
      TRUE ~ ACAP_THREE / ANY_ACAP
    )
  )
    
DBI::dbDisconnect(con); rm(list = ls()); gc()
