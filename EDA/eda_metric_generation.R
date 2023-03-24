# Libraries
library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
data <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

# Create parent uprn groups
data = data %>% 
  filter(UPRN_FLAG == 1) %>% 
  mutate(
    MATCH_SLA_STD = coalesce(MATCH_SLA_PARENT, MATCH_SLA_STD),
    UPRN = coalesce(PARENT_UPRN, UPRN)
  )

# Get chapter info
chapters = con %>% 
  tbl(from = in_schema("DIM", "CDR_EP_DRUG_BNF_DIM")) %>% 
  filter(YEAR_MONTH == 202201) %>% 
  select(BNF_CHAPTER, CHAPTER_DESCR) %>% 
  distinct() %>% 
  collect() %>% 
  filter(BNF_CHAPTER %in% c('01','02','03','04','06','07','08','09','10')) %>% 
  select(CHAPTER_DESCR) %>% 
  pull()

# Get column names
cols = data.frame(COL = names(data)) %>% arrange(COL)

# 1. Define metric groups ------------------------------------------------------

# 1. General high-level and Patient-level features
# 2. The 'standard' CH metrics (minus polypharmayc overlap)
# 3. Polypharmacy metrics
# 4. Specific drug-level metrics

# 2. Metrics: high-level and patient demographics ------------------------------

# Metrics pat
df_pat = data %>% 
  group_by(
    MATCH_SLA_STD,
    UPRN
    ) %>% 
  summarise(
    PARENT_UPRN = max(PARENT_UPRN),
    NURSING_HOME_FLAG = max(NURSING_HOME_FLAG),
    RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG),
    MAX_MONTHLY_PATIENTS = max(MAX_MONTHLY_PATIENTS),
    NUMBER_OF_BEDS = max(NUMBER_OF_BEDS),
    MONTHS = n_distinct(YEAR_MONTH),
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    NIC = sum(ITEM_PAY_DR_NIC),
    MALE = n_distinct(NHS_NO[GENDER == "Male"]),
    FEMALE = n_distinct(NHS_NO[GENDER == "Female"]),
    UNKNOWN = n_distinct(NHS_NO[is.na(GENDER)]),
    AGE_65_69 = n_distinct(NHS_NO[AGE_BAND == "65-69"]),
    AGE_70_74 = n_distinct(NHS_NO[AGE_BAND == "70-74"]),
    AGE_75_79 = n_distinct(NHS_NO[AGE_BAND == "75-79"]),
    AGE_80_84 = n_distinct(NHS_NO[AGE_BAND == "80-84"]),
    AGE_85_89 = n_distinct(NHS_NO[AGE_BAND == "85-89"]),
    AGE_90_PLUS = n_distinct(NHS_NO[AGE_BAND == "90+"])
  ) %>% 
  ungroup() %>% 
  filter(PATS >= 5) %>% 
  collect() %>% 
  mutate(
    MALE_PROP = MALE / PATS,
    FEMALE_PROP = FEMALE / PATS,
    UNKNOWN_PROP = UNKNOWN / PATS,
    AGE_65_69_PROP = AGE_65_69 / PATS,
    AGE_70_74_PROP = AGE_70_74 / PATS,
    AGE_75_79_PROP = AGE_75_79 / PATS,
    AGE_80_84_PROP = AGE_80_84 / PATS,
    AGE_85_89_PROP = AGE_85_89 / PATS,
    AGE_90_PLUS_PROP = AGE_90_PLUS / PATS,
    PARENT_UPRN = ifelse(is.na(PARENT_UPRN), 0, 1)
  )

# 3. Metrics: existing CH metrics ----------------------------------------------

# NOTE: mean unique meds & >10 unique meds covered in poly metrics

# Cost PPM
df_cost_items = data %>% 
  group_by(MATCH_SLA_STD, NHS_NO) %>% 
  summarise(
    MEAN_PATIENT_MONTHLY_COST = sum(ITEM_PAY_DR_NIC * 0.01, na.rm = TRUE) / n_distinct(YEAR_MONTH),
    MEAN_PATIENT_MONTHLY_ITEMS = sum(ITEM_COUNT, na.rm = TRUE) / n_distinct(YEAR_MONTH)
  ) %>%
  ungroup() %>% 
  group_by(MATCH_SLA_STD) %>% 
  summarise(
    COST_PPM = mean(MEAN_PATIENT_MONTHLY_COST),
    ITEMS_PPM = mean(MEAN_PATIENT_MONTHLY_ITEMS)
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  inner_join(df_pat %>% select(MATCH_SLA_STD))

# Check cost
df_cost_items %>% 
  ggplot(., aes(COST_PPM, fill = "a")) + 
  geom_density() +
  xlim(0,500)

# Check items
df_cost_items %>% 
  ggplot(., aes(ITEMS_PPM, fill = "a")) + 
  geom_density() +
  xlim(0,20)

# 4. Polypharmacy metrics ------------------------------------------------------

# Check metric distributions 
plot_vars = function(df_name){
  
  # Get vars
  vars = setdiff(names({{df_name}}), c("MATCH_SLA_STD"))
  
  # Distributions
  {{df_name}} %>% 
    tidyr::pivot_longer(cols = vars) %>% 
    ggplot(., aes(value, fill = "a"))+
    geom_density()+
    facet_wrap(~name, scales = "free")
}

# NSAID metric other drugs
other_drug_vec = c(
  '0601022B0', 
  '0601023AD',
  '0601023AF',
  '0601023AH',
  '0601023AJ',
  '0601023AL',
  '0601023AP',
  '0601023AR',
  '0601023V0',
  '0601023W0',
  '0601023Z0'
)

# ACB score of 1
acb1_drugs <- c(
  '0102000A0','0102000C0','0103010D0','0103010E0','0103010S0','0103010T0','0104020D0',
  '0104020L0','0104020P0','0105020B0','0105020C0','0105020D0','0105020E0','0105020F0',
  '0107010AB','0107010L0','0107020J0','0107020P0','0107040B0','0201010F0','0202010F0',
  '0202020L0','0202030W0','0202040U0','0202040V0','0202080E0','0202080K0','0203020F0',
  '0203020G0','0203020T0','0203020U0','0204000A0','0204000E0','0204000F0','0204000J0',
  '0204000K0','0204000U0','0204000W0','0205010J0','0205051F0','0206010I0','0206010K0',
  '0206010W0','0206020R0','0208020V0','0209000L0','0209000V0','0301030S0','0304010AB',
  '0304010AC','0304010D0','0304010I0','0304010Y0','0309010C0','0401020A0','0401020K0',
  '0401020V0','040201030','0402010AD','0402010AE','0402010J0','0402020AA','0402020AB',
  '0402020AD','0402020T0','0402030R0','0403010X0','0403030L0','0403040B0','0403040W0',
  '040702010','040702020','0407020A0','0407020C0','0407020N0','0407020P0','0407020Q0',
  '0410020A0','0603020J0','0603020L0','0603020M0','0603020N0','0603020Q0','0603020T0',
  '0603020V0','0603020W0','0603020X0','1001022G0','1001022N0','1001040G0'
)

# ACB score of 2
acb2_drugs <- c(
  '0102000H0',
  '0304010K0',
  '0309020C0',
  '0402010M0',
  '0402010R0',
  '0407010P0',
  '0408010C0', 
  '0408010D0',
  '0409010B0',
  '1002020H0',
  '1003020D0'
)

# ACB score of 3
acb3_drugs <- c(
  '010200030','0102000AB','0102000AC','0102000F0','0102000J0','0102000K0','0102000Q0',
  '0102000Y0','0301020A0','0301020B0','030401020','0304010F0','0304010G0','0304010H0',
  '0304010J0','0304010N0','0304010W0','030902040','030902050','030902060','0309020AB',
  '0309020U0','040201060','0402010AB','0402010C0','0402010D0','0402010Q0','0402010W0',
  '0402010X0','0402020AC','0403010B0','0403010C0','0403010E0','0403010F0','0403010H0',
  '0403010L0','0403010N0','0403010V0','0403010Y0','0403030P0','0406000AA','0406000AC',
  '0406000H0','0406000V0','0409020C0','0409020N0','0704010W0','0704020AB','0704020AC',
  '0704020AD','0704020G0','0704020J0','0704020N0','0704020P0','0704020Z0','0704040G0',
  '1002020S0','1002020V0'
)

# Poly metrics 1-3
df_poly_one = data %>% 
  filter(CHAPTER_DESCR %in% chapters) %>% 
  mutate(
    ACB_CAT = case_when(
      BNF_CHEMICAL_SUBSTANCE %in% acb1_drugs ~ 1,
      BNF_CHEMICAL_SUBSTANCE %in% acb2_drugs ~ 2,
      BNF_CHEMICAL_SUBSTANCE %in% acb3_drugs ~ 3,
      TRUE ~ 0
    )
  ) %>% 
  group_by(YEAR_MONTH, NHS_NO, MATCH_SLA_STD) %>% 
  summarise(
    # All Drugs
    DRUG_TOTAL = n_distinct(BNF_CHEMICAL_SUBSTANCE),
    
    # 8 Distinct drugs
    DRUG_8 = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE) >= 8 ~ 1,
      TRUE ~ 0
    ),
    
    # 10 distinct drugs
    DRUG_10 = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE) >= 10 ~ 1,
      TRUE ~ 0
    ),
    
    # 15 distinct drugs
    DRUG_15 = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE) >= 15 ~ 1,
      TRUE ~ 0
    ),
    
    # 20 distinct drugs
    DRUG_20 = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE) >= 20 ~ 1,
      TRUE ~ 0
    ),
    
    # ACB SCORE 6
    ACB_6 = case_when( 
      (1 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1])) + 
        (2 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 2])) + 
        (3 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 3])) >= 6 ~ 1,
      TRUE ~ 0
    ),
    
    # ACB SCORE 9
    ACB_9 = case_when( 
      (1 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1])) + 
        (2 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 2])) + 
        (3 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 3])) >= 9 ~ 1,
      TRUE ~ 0
    ),
    
    # ACB SCORE 12
    ACB_12 = case_when( 
      (1 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1])) + 
        (2 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 2])) + 
        (3 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 3])) >= 12 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  ungroup() %>% 
  group_by(NHS_NO, MATCH_SLA_STD) %>% 
  summarise(
    DRUG_TOTAL = mean(DRUG_TOTAL),
    DRUG_8 = sum(DRUG_8) / n(),
    DRUG_10 = sum(DRUG_10) / n(),
    DRUG_15 = sum(DRUG_15) / n(),
    DRUG_20 = sum(DRUG_20) / n(),
    ACB_6 = sum(ACB_6) / n(),
    ACB_9 = sum(ACB_9) / n(),
    ACB_12 = sum(ACB_12) / n(),
  ) %>% 
  ungroup() %>% 
  select(-NHS_NO) %>% 
  group_by(MATCH_SLA_STD) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  collect() %>% 
  inner_join(df_pat %>% select(MATCH_SLA_STD))

# Plot distributions
plot_vars(df_poly_one)

# Poly metrics 4-6
df_poly_two = data %>% 
  mutate(
    # NSAID flag
    NSAID_FLAG = case_when(
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^100101') > 0 ~ 1,
      TRUE ~ 0
    ),
    # NSAID other flag
    OTHER_FLAG = case_when(
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205051') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205052') > 0 ~ 1,
      BNF_CHEMICAL_SUBSTANCE %in% other_drug_vec ~ 1,
      TRUE ~ 0
    ),
    
    # Coag flag
    COAG_FLAG = case_when(
      BNF_CHEMICAL_SUBSTANCE == '0208020W0' ~ 0,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0208020') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0209000') > 0 ~ 1,
      TRUE ~ 0
    ),
    
    # Damn flag
    DAMN_FLAG = case_when(
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^100101') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205051') > 0 ~ 1,
      REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205052') > 0 ~ 1,
      BNF_CHEMICAL_SUBSTANCE %in% other_drug_vec ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(YEAR_MONTH, NHS_NO, MATCH_SLA_STD) %>% 
  summarise(
    # Metric: NSAID
    NSAID = case_when(
      max(NSAID_FLAG) + max(OTHER_FLAG) == 2 ~ 1,
      TRUE ~ 0
    ),
    
    # Metric: DAMN
    DAMN = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE[DAMN_FLAG == 1]) >= 2 ~ 1,
      TRUE ~ 0
    ),
    
    # Metric: COAG
    COAG = case_when(
      n_distinct(BNF_CHEMICAL_SUBSTANCE[COAG_FLAG == 1]) >= 3 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(NHS_NO, MATCH_SLA_STD) %>% 
  summarise(
    NSAID = 100 * sum(NSAID) / n(),
    DAMN = 100 * sum(DAMN) / n(),
    COAG = 100 * sum(COAG) / n()
  ) %>% 
  ungroup() %>% 
  select(-NHS_NO) %>% 
  group_by(MATCH_SLA_STD) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup() %>% 
  collect() %>% 
  inner_join(df_pat %>% select(MATCH_SLA_STD))

# Plot distributions
plot_vars(df_poly_two)

# 5. Drug-level metrics --------------------------------------------------------

# Replace NA for zero
replace_na = function(x) ifelse(is.na(x), 0, x)

# Chapter-level proportions
df_chapter = data %>% 
  group_by(MATCH_SLA_STD, CHAPTER_DESCR) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  group_by(MATCH_SLA_STD) %>% 
  mutate(
    TOTAL = sum(ITEMS),
    PROP = ITEMS / TOTAL
    ) %>%
  ungroup() %>% 
  collect() %>% 
  inner_join(df_pat %>% select(MATCH_SLA_STD)) %>% 
  select(-c(ITEMS, TOTAL)) %>% 
  tidyr::pivot_wider(
    names_from = "CHAPTER_DESCR",
    values_from = "PROP",
    names_glue = "CHAPTER_{CHAPTER_DESCR}"
    ) %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  mutate_all(.funs = replace_na)

# Plot distributions
plot_vars(df_chapter)

# BNF sections of interest
sections = c(
  'Antifungal drugs',
  'Laxatives',
  'Antidepressant drugs',
  'Drugs used in psychoses and related disorders',
  'Analgesics'
)

# Section-level metrics
df_section = data %>% 
  group_by(MATCH_SLA_STD) %>% 
  mutate(TOTAL = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  filter(SECTION_DESCR %in% sections) %>% 
  group_by(MATCH_SLA_STD, TOTAL, SECTION_DESCR) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  mutate(PROP = ITEMS / TOTAL) %>%
  collect() %>% 
  inner_join(df_pat %>% select(MATCH_SLA_STD)) %>% 
  select(-c(ITEMS, TOTAL)) %>% 
  tidyr::pivot_wider(
    names_from = "SECTION_DESCR",
    values_from = "PROP",
    names_glue = "SECTION_{SECTION_DESCR}"
  ) %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  mutate_all(.funs = replace_na)

#Plot distributions
plot_vars(df_section)

# BNF paragraphs of interest
paragraphs = c(
  'Opioid analgesics',
  'Urinary-tract infections',
  'Antibacterials'
)

# Section-level metrics
df_paragraph = data %>% 
  group_by(MATCH_SLA_STD) %>% 
  mutate(TOTAL = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  filter(PARAGRAPH_DESCR %in% paragraphs) %>% 
  group_by(MATCH_SLA_STD, TOTAL, PARAGRAPH_DESCR) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  mutate(PROP = ITEMS / TOTAL) %>%
  collect() %>% 
  inner_join(df_pat %>% select(MATCH_SLA_STD)) %>% 
  select(-c(ITEMS, TOTAL)) %>% 
  tidyr::pivot_wider(
    names_from = "PARAGRAPH_DESCR",
    values_from = "PROP",
    names_glue = "PARAGRAPH_{PARAGRAPH_DESCR}"
  ) %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  mutate_all(.funs = replace_na)

#Plot distributions
plot_vars(df_paragraph)

# BNF Chemical Substances of Interest
chem_subs = c(
  'Tramadol hydrochloride',
  'Diazepam',
  'Donepezil hydrochloride',
  'Fentanyl'
)

# Section-level metrics
df_bnf = data %>% 
  group_by(MATCH_SLA_STD) %>% 
  mutate(TOTAL = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  filter(CHEMICAL_SUBSTANCE_BNF_DESCR %in% chem_subs) %>% 
  group_by(MATCH_SLA_STD, TOTAL, CHEMICAL_SUBSTANCE_BNF_DESCR) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  mutate(PROP = ITEMS / TOTAL) %>%
  collect() %>% 
  inner_join(df_pat %>% select(MATCH_SLA_STD)) %>% 
  select(-c(ITEMS, TOTAL)) %>% 
  tidyr::pivot_wider(
    names_from = "CHEMICAL_SUBSTANCE_BNF_DESCR",
    values_from = "PROP",
    names_glue = "BNF_{CHEMICAL_SUBSTANCE_BNF_DESCR}"
  ) %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  mutate_all(.funs = replace_na)

#Plot distributions
plot_vars(df_bnf)

# 6. Merge all data and clean and close ----------------------------------------

# Merge all data
df_total = df_pat %>% 
  left_join(df_cost_items) %>% 
  left_join(df_poly_one) %>% 
  left_join(df_poly_two) %>% 
  left_join(df_chapter) %>% 
  left_join(df_section) %>% 
  left_join(df_paragraph) %>% 
  left_join(df_bnf) %>% 
  mutate_all(.funs = replace_na)

# Close, remove and clean
DBI::dbDisconnect(con)

# All vars except total data
remove = setdiff(ls(), c("df_total"))
rm(list = c(remove, "remove")); gc()
