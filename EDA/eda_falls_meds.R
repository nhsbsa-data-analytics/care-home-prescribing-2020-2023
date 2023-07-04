
# Section level drug groups
falls_section_vec = c(
  'Antidepressant drugs',
  'Antiepileptic drugs',
  'Diuretics',
  'Hypertension and heart failure'
  )
 
# Paragraph level drug groups
falls_paragraph_vec = c(
  'Antipsychotic depot injections',
  'Antipsychotic drugs',
  'Opioid analgesics',
  'Opioid dependence',
  'Alpha-adrenoceptor blocking drugs',
  'Antihistamines',
  'Vasoconstrictor sympathomimetics',
  'Vasodilator antihypertensive drugs',
  'Drugs for urinary frequency enuresis and incontinence'
)

# Chem sub leevel groups
falls_chem_vec = c('Midazolam hydrochloride')

# Libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP (sub paragraph benzodiazapenes)
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create lazy table
data <- con %>%
  tbl(from = in_schema("MAMCP", "INT646_BASE_20220401_20230331"))

# Distinct patient-month combinations for left join
pat_month = data %>% 
  filter(CH_FLAG == 1) %>% 
  select(NHS_NO, YEAR_MONTH, PCD_ICB_NAME) %>% 
  distinct()

# Number of unique falls rish chem subs per month (in which they received presc)
pat_presc = data %>% 
  filter(
    CH_FLAG == 1,
    CHAPTER_DESCR %in% falls_section_vec | PARAGRAPH_DESCR %in% falls_paragraph_vec | CHEMICAL_SUBSTANCE_BNF_DESCR %in% falls_chem_vec
  ) %>% 
  group_by(YEAR_MONTH, NHS_NO, PCD_ICB_NAME) %>% 
  summarise(N_DRUG =  n_distinct(CHEMICAL_SUBSTANCE_BNF_DESCR)) %>% 
  ungroup()

# Collect for multiple local aggregations
df = pat_month %>% 
  left_join(pat_presc) %>% 
  mutate(N_DRUG = ifelse(is.na(N_DRUG), 0, N_DRUG)) %>% 
  collect()

# Sample output
output = df %>% 
  group_by(NHS_NO, ICB = PCD_ICB_NAME) %>% 
  summarise(
    MEAN = mean(N_DRUG),
    `2+` = 100 * (sum(N_DRUG >= 2) / n()),
    `3+` = 100 * (sum(N_DRUG >= 3) / n()),
    `4+` = 100 * (sum(N_DRUG >= 4) / n()),
    `5+` = 100 * (sum(N_DRUG >= 5) / n())
    ) %>% 
  ungroup() %>% 
  select(-NHS_NO) %>% 
  group_by(ICB) %>% 
  summarise_all(.funs = function(x) round(mean(x),2)) %>% 
  ungroup()

# Export 
write.csv(output, "C:/Users/ADNSH/OneDrive - NHS Business Services Authority/Desktop/FALLS_RISK_METRICS_SAMPLE.csv")

# Clean
DBI::dbDisconnect(con); rm(list = ls()); gc()
