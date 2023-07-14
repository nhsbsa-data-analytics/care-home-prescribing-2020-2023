
# Libraries
library(dplyr)
library(dbplyr)

# Falls risk drug sections
section_vec = c(
  'Antidepressant drugs',
  'Antiepileptic drugs',
  'Diuretics',
  'Hypertension and heart failure',
  'Hypnotics and anxiolytics'
)

# Falls risk paragraphs
paragraph_vec = c(
  'Antipsychotic depot injections',
  'Antipsychotic drugs',
  'Opioid analgesics',
  'Opioid dependence',
  'Alpha-adrenoceptor blocking drugs',
  'Antihistamines',
  'Vasodilator antihypertensive drugs',
  'Drugs for urinary frequency enuresis and incontinence',
  'Nitrates'
)

# Falls risk drug chem
chem_sub_vec = c('Midazolam hydrochloride')

# Falls risk chem sub exlcusions
exclude_chem_sub_vec = c(
  'Paraldehyde',
  'Mirabegron',
  'Mannitol',
  'Loratadine',
  'Desloratadine',
  'Bilastine',
  'Minoxidil'
)

# Part One: collect chem sub-level falls risk med list -------------------------

# Set up connection to DALP 
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
drug_db <- con %>%
  tbl(from = in_schema("DIM", "CDR_DY_DRUG_BNF_DIM"))

# Join drug list
drug_list_db = drug_db %>% 
  filter(
    YEAR_MONTH == 202303,
    SECTION_DESCR %in% section_vec | PARAGRAPH_DESCR %in% paragraph_vec | CHEMICAL_SUBSTANCE_BNF_DESCR %in% chem_sub_vec,
    !CHEMICAL_SUBSTANCE_BNF_DESCR %in% exclude_chem_sub_vec
  ) %>% 
  select(PARAGRAPH_DESCR, CHEMICAL_SUBSTANCE_BNF_DESCR) %>% 
  distinct()

# Inspect drug list
drug_list_df = drug_list_db %>% collect()

# Part Two: Mean unique falls risk meds ---------------------------------------- 

# Create lazy table
data <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))
  
# Distinct patient-month combinations for left join
pat_months = data %>% 
  filter(
    FY == "2022/23",
    CH_FLAG == 1
    ) %>% 
  select(NHS_NO, YEAR_MONTH, PCD_ICB_NAME) %>% 
  distinct()

# Number of unique falls rish chem subs per month (in which they received presc)
pat_presc = data %>% 
  inner_join(drug_list_db) %>% 
  group_by(YEAR_MONTH, NHS_NO, PCD_ICB_NAME) %>% 
  summarise(N_DRUG =  n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>% 
  ungroup()

# Collect df output
df = pat_months %>% 
  left_join(pat_presc) %>% 
  mutate(N_DRUG = ifelse(is.na(N_DRUG), 0, N_DRUG)) %>% 
  group_by(PCD_ICB_NAME) %>% 
  summarise(N_DRUG = mean(N_DRUG)) %>% 
  ungroup() %>% 
  collect()

# Part Three: Proportion 3+ falls risk meds ------------------------------------

# Proportion 3+ falls risk meds
# df_two = data %>% 
#   filter(
#     FY == "2022/23",
#     CH_FLAG == 1
#   ) %>% 
#   inner_join(drug_list_db) %>% 
#   group_by(YEAR_MONTH, NHS_NO, PCD_ICB_NAME) %>% 
#   summarise(
#     PROP_3_PLUS = case_when(
#       n_distinct(BNF_CHEMICAL_SUBSTANCE) >= 3 ~ 1,
#       TRUE ~ 0
#     )
#   ) %>% 
#   ungroup() %>% 
#   group_by(PCD_ICB_NAME) %>% 
#   summarise(PROP_3_PLUS = sum(PROP_3_PLUS) / n()) %>% 
#   ungroup() %>% 
#   collect()

# Part four: falls 5+ check ----------------------------------------------------

falls_5_df = data %>% 
  filter(FY == "2020/21") %>% 
  mutate(
    FALLS_CAT = case_when(
      (SECTION_DESCR %in% section_vec |
         PARAGRAPH_DESCR %in% paragraph_vec |
         CHEMICAL_SUBSTANCE_BNF_DESCR %in% chem_sub_vec) &
        !CHEMICAL_SUBSTANCE_BNF_DESCR %in% exclude_chem_sub_vec ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(YEAR_MONTH, NHS_NO, PCD_LAD_NAME, CH_FLAG) %>% 
  summarise(
    FALLS = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE[FALLS_CAT == 1]) >= 5 ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  ungroup() %>% 
  group_by(PCD_LAD_NAME, CH_FLAG) %>% 
  summarise(PCT_PM_FALLS = 100 * (sum(FALLS) / n())) %>% 
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16)


a = falls_5_df %>% 
  filter(CH_FLAG == 0) %>% 
  select(PCT_PM_FALLS, CH_FLAG) %>% 
  arrange(PCT_PM_FALLS) %>%
  mutate(ROW = row_number())

b = falls_5_df %>% 
  filter(CH_FLAG == 1) %>% 
  select(PCT_PM_FALLS, CH_FLAG) %>% 
  arrange(PCT_PM_FALLS) %>% 
  mutate(ROW = row_number())

hchart(rbind(a,b), "spline", hcaes(ROW, PCT_PM_FALLS, group = CH_FLAG))

# Part five: data export -------------------------------------------------------

# join outputs
output = inner_join(df, df_two)

# Export 
write.csv(output, "C:/Users/ADNSH/OneDrive - NHS Business Services Authority/Desktop/FALLS_RISK_METRICS_SAMPLE.csv")

# Clean
DBI::dbDisconnect(con); rm(list = ls()); gc()
