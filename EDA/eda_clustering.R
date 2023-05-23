
# Get data
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("EDA/eda_metric_generation.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Columns to remove
remove_vars = c(
  'CURRENT_RATING',
  'MATCH_SLA_STD',
  'PARENT_UPRN',
  'NURSING_HOME_FLAG',
  'RESIDENTIAL_HOME_FLAG',
  'MAX_MONTHLY_PATIENTS',
  'NUMBER_OF_BEDS',
  'MONTHS',
  'ITEMS',
  'NIC',
  'MALE',
  'FEMALE',
  'UNKNOWN',
  'AGE_65_69',
  'AGE_70_74',
  'AGE_75_79',
  'AGE_80_84',
  'AGE_85_89',
  'AGE_90_PLUS',
  'UNKNOWN_PROP',
  'AGE_65_69_PROP',
  'AGE_70_74_PROP',
  'AGE_75_79_PROP',
  'AGE_80_84_PROP',
  'AGE_85_89_PROP',
  'AGE_90_PLUS_PROP',
  'MALE_PROP',
  'FEMALE_PROP',
  'PATS'
)

# 0. High-level figures --------------------------------------------------------

# Create lazy table
data <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_MATCH_SLA"))

# Create lazy table
base <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

# Any care home figures
df_high = data %>% 
  filter(AGE_BAND != "<65") %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    COST = round(sum(ITEM_PAY_DR_NIC) / 100)
  ) %>% 
  collect_with_parallelism(., 8) %>% 
  mutate(TYPE = "Exact care home matching")

# Any care home figures
df_high_ch = base %>% 
  filter(
    AGE_BAND != "<65",
    CH_FLAG == 1
    ) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    COST = round(sum(ITEM_PAY_DR_NIC) / 100)
  ) %>% 
  collect_with_parallelism(., 8) %>% 
  mutate(TYPE = "Any care home matching")

# Both above
df_high_all = rbind(df_high, df_high_ch)

# Exact care home figures
df_exact = data %>% 
  filter(AGE_BAND != "<65") %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    COST = round(sum(ITEM_PAY_DR_NIC) / 100)
  ) %>% 
  ungroup() %>% 
  collect_with_parallelism(., 8) %>%  
  arrange(YEAR_MONTH) %>% 
  mutate(
    YEAR_MONTH = factor(YEAR_MONTH),
    TYPE = "Exact care home matching"
    )

# High level figures df
df_any = base %>% 
  filter(
    CH_FLAG == 1,
    AGE_BAND != "<65"
    ) %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT),
    COST = round(sum(ITEM_PAY_DR_NIC) / 100)
  ) %>% 
  ungroup() %>% 
  collect_with_parallelism(., 8) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(
    YEAR_MONTH = factor(YEAR_MONTH),
    TYPE = "Any care home matching"
  )

# combing above 2 df
df_comb = rbind(df_any, df_exact)

# Mean patients per month
mean(df_high$PATS)

# Patients per month
df_comb %>% 
  highcharter::hchart(., "line", hcaes(YEAR_MONTH, PATS, group = TYPE)) %>% 
  hc_yAxis(min = 0) %>% 
  hc_xAxis(title = list(text = "Month")) %>% 
  hc_yAxis(title = list(text = "Number of Distinct Patients")) %>% 
  hc_title(text = "Number of Distinct Patients per Month by Match Type for FY21/22")

# Cost per month 
df_comb %>% 
  highcharter::hchart(., "line", hcaes(YEAR_MONTH, COST, group = TYPE)) %>% 
  hc_yAxis(min = 0)

# Items per month
df_comb %>% 
  highcharter::hchart(., "line", hcaes(YEAR_MONTH, ITEMS, group = TYPE)) %>% 
  hc_yAxis(min = 0)

# 1. PCA -----------------------------------------------------------------------

# DF for pca
df_pca = df_total %>% 
  select(-remove_vars) %>% 
  tibble::column_to_rownames(var = "UPRN")

# PCA
pca = FactoMineR::PCA(df_pca, graph = FALSE)

# PCA Summary
summary(pca)

# Screeplot
factoextra::fviz_screeplot(pca)

# Cos2 of dims 1-5
factoextra::fviz_cos2(pca, choice = "var", axes = 1:5)+
  coord_flip()

# Plot PC1 and PC2
factoextra::fviz_pca_var(
  pca,
  col.var="contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  #repel = TRUE,
  select.ind = list(contrib = 30)
  )

# Contributions of variables to PC1
factoextra::fviz_contrib(pca, choice = "var", axes = 1, top = 10)+
  coord_flip()

# Contributions of variables to PC2
factoextra::fviz_contrib(pca, choice = "var", axes = 2, top = 10)+
  coord_flip()

# Contributions of variables to PC3
factoextra::fviz_contrib(pca, choice = "var", axes = 3, top = 10)+
  coord_flip()

# 2. Clustering ----------------------------------------------------------------

# DF for clustering
mat_clust = df_total %>% 
  select(-remove_vars) %>% 
  tibble::column_to_rownames(var = "UPRN") %>% 
  mutate_all(.funs = scale) %>% 
  as.matrix()

# Cluster
km = kmeans(mat_clust, 3, nstart = 25)

# Plot
factoextra::fviz_cluster(
  km, 
  data = mat_clust, 
  ellipse.type = "norm",
  geom = "point"
)

# Result
output = factoextra::fviz_cluster(
  km, 
  data = mat_clust
)

# As data frame
df_output = output$data %>% 
  tibble::rownames_to_column(var = "UPRN") %>% 
  mutate(UPRN = as.double(UPRN)) %>% 
  select(x, y, UPRN) %>% 
  inner_join(df_total)

# Plot by rating
df_output %>% 
  ggplot(aes(x, y, color = DRUG_TOTAL, size = PARAGRAPH_OPIOID_ANALGESICS))+
  geom_point()+
  scale_color_continuous(type = "viridis")

# Plot by rating
df_output %>% 
  ggplot(aes(x, y, color = DRUG_TOTAL, size = CHAPTER_CENTRAL_NERVOUS_SYSTEM^3))+
  geom_point()+
  scale_color_continuous(type = "viridis")

# 3. Chapter proportions by mean DRUG_TOTAL metric -----------------------------

# Create chapter names vector
drug_chapter_vars = c(names(df_total)[grepl("CHAPTER", names(df_total))], "DRUG_TOTAL")

# Process data and simplify DRUG_TOTAL categories
df_bar = df_total %>% 
  select(drug_chapter_vars) %>% 
  mutate(
    DRUG_TOTAL = round(DRUG_TOTAL),
    DRUG_TOTAL = case_when(
      DRUG_TOTAL <= 4 ~ "1-4",
      DRUG_TOTAL >= 9 ~ "9+",
      T ~ as.character(DRUG_TOTAL)
    ),
    DRUG_TOTAL = factor(DRUG_TOTAL, levels = c("1-4", "5", "6", "7", "8", "9+"))
  ) %>% 
  group_by(DRUG_TOTAL) %>% 
  summarise_all(.funs = mean) %>% 
  pivot_longer(!DRUG_TOTAL, names_to = "CHAPTER", values_to = "MEAN")

# Function to plot
ggplot_bar_chapter = function(vars){
  p = df_bar %>% 
    filter(CHAPTER == vars) %>% 
    ggplot(aes(DRUG_TOTAL, MEAN, fill = CHAPTER))+
    geom_bar(stat = "identity")
  print(p)
}

# All plots
#lapply(names(df_total)[grepl("CHAPTER", names(df_total))], ggplot_bar_chapter)

# Identify upward trend chapters
down_chapters = c(
  "CHAPTER_ANAESTHESIA",
  "CHAPTER_INFECTIONS",
  "CHAPTER_EYE",
  "CHAPTER_CARDIOVASCULAR_SYSTEM",
  "CHAPTER_ENDOCRINE_SYSTEM"
)

# Identify downward trend chapters
up_chapters = c(
  "CHAPTER_RESPIRATORY_SYSTEM",
  "CHAPTER_NUTRITION_AND_BLOOD",
  "CHAPTER_MUSCULOSKELETAL_AND_JOINT_DISEASES",
  "CHAPTER_GASTRO_INTESTINAL_SYSTEM"
)

# All plots
lapply(down_chapters, ggplot_bar_chapter)
lapply(up_chapters, ggplot_bar_chapter)

# Grouped plot
df_bar %>% 
  filter(CHAPTER %in% up_chapters) %>% 
  ggplot(aes(DRUG_TOTAL, MEAN, fill = CHAPTER))+
  geom_bar(stat = "identity")

# Grouped plot
df_bar %>% 
  filter(CHAPTER %in% down_chapters) %>% 
  ggplot(aes(DRUG_TOTAL, MEAN, fill = CHAPTER))+
  geom_bar(stat = "identity")

# Hchart
df_bar %>% 
  filter(CHAPTER == "CHAPTER_ANAESTHESIA") %>% 
  hchart(., "column", hcaes(DRUG_TOTAL, MEAN)) %>% 
  hc_xAxis(title = list(text = "Mean number of Monthly Unique Medicines per Patient")) %>% 
  hc_yAxis(title = list(text = "Proportion of prescribing")) %>% 
  hc_title(text = "Proportion of drugs from the <b>Aneasthesia Chapter</b> against the Mean Number of Monthly Unqiue Medicines per Patient")

# Hchart
df_bar %>% 
  filter(CHAPTER == "CHAPTER_GASTRO_INTESTINAL_SYSTEM") %>% 
  hchart(., "column", hcaes(DRUG_TOTAL, MEAN)) %>% 
  hc_xAxis(title = list(text = "Mean number of Monthly Unique Medicines per Patient")) %>% 
  hc_yAxis(title = list(text = "Proportion of prescribing")) %>% 
  hc_title(text = "Proportion of drugs from the <b>Gastro Intestinal Chapter</b> against the Mean Number of Monthly Unqiue Medicines per Patient")

# 4. Rating by metric ----------------------------------------------------------

# Process rating information
df_rating = df_total %>% 
  filter(!CURRENT_RATING %in% c("Inspected but not rated", "Unknown")) %>% 
  mutate(CURRENT_RATING = factor(
    CURRENT_RATING, 
    levels = c("Inadequate", "Requires improvement", "Good", "Outstanding")
    )) %>% 
  select(UPRN, CURRENT_RATING) %>% 
  inner_join(
    df_total %>% 
      select(-remove_vars),
    by = "UPRN"
  ) %>% 
  select(-UPRN) %>% 
  group_by(CURRENT_RATING) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup()

# All column names
cols = names(df_rating)[names(df_rating) != c("CURRENT_RATING")]

# Function to plot groups of variables
ggplot_bar_rating = function(vars){
  p = df_rating %>% 
    rename_at(vars, ~"METRIC") %>%
    ggplot(aes(CURRENT_RATING, METRIC, fill = "lightblue"))+
    geom_col()+
    ggtitle(vars)
  print(p)
}

# All plots
#lapply(cols, ggplot_bar_rating)

# Trend columns
descending_vars = c(
  "BNF_DIAZEPAM",
  "BNF_TRAMADOL_HYDROCHLORIDE",
  "SECTION_DRUGS_USED_IN_PSYCHOSES_AND_RELATED_DISORDERS",
  "CHAPTER_RESPIRATORY_SYSTEM",
  "CHAPTER_NUTRITION_AND_BLOOD",
  "DAMN",
  "NSAID",
  "ACB_9"
)

# Trend columns
ascending_vars = c(
  "BNF_FENTANYL",
  "BNF_DONEPEZIL_HYDROCHLORIDE",
  "PARAGRAPH_URINARY_TRACT_INFECTIONS",
  "PARAGRAPH_OPIOID_ANALGESICS",
  "SECTION_ANALGESICS",
  "CHAPTER_ANAESTHESIA",
  "CHAPTER_MALIGNANT_DISEASE_AND_IMMUNOSUPPRESSION",
  "CHAPTER_INCONTINENCE_APPLIANCES",
  "CHAPTER_EAR_NOSE_AND_OROPHARYNX",
  "CHAPTER_MUSCULOSKELETAL_AND_JOINT_DISEASES",
  "CHAPTER_INFECTIONS",
  "CHAPTER_INCONTINENCE_APPLIANCES"
)

# Other columns of interest
other_vars = c(
  "DRUG_TOTAL",
  "ITEMS_PPM"
)

# Plots
lapply(descending_vars, ggplot_bar_rating)
lapply(ascending_vars, ggplot_bar_rating)
lapply(other_vars, ggplot_bar_rating)

# Hchart
df_rating %>% 
  hchart(., "column", hcaes(CURRENT_RATING, NSAID)) %>% 
  hc_xAxis(title = list(text = "Current Care Home Rating")) %>% 
  hc_yAxis(title = list(text = "Polypharmacy Metric Value")) %>% 
  hc_title(text = "Proportion of <b>Patients Prescribed a Non-Steroidal Anti-Inflammmatory Drug and one or more other medicine likely to cause kidney injury</b> by Care Home Rating")

# Hchart
df_rating %>% 
  hchart(., "column", hcaes(CURRENT_RATING, CHAPTER_INCONTINENCE_APPLIANCES)) %>% 
  hc_xAxis(title = list(text = "Current Care Home Rating")) %>% 
  hc_yAxis(title = list(text = "Proportion of Prescribing")) %>% 
  hc_title(text = "Proportion of Prescribing from the <b>Incontinence Appliances Chapter</b> by Care Home Rating")

# 5. Correlated vars scatterplots ----------------------------------------------

# Columns to remove
remove_vars = c(
  "UPRN",
  'CURRENT_RATING',
  'MATCH_SLA_STD',
  'PARENT_UPRN',
  'NURSING_HOME_FLAG',
  'RESIDENTIAL_HOME_FLAG',
  'MAX_MONTHLY_PATIENTS',
  'NUMBER_OF_BEDS',
  'MONTHS',
  'ITEMS',
  'NIC',
  'MALE',
  'FEMALE',
  'UNKNOWN',
  'AGE_65_69',
  'AGE_70_74',
  'AGE_75_79',
  'AGE_80_84',
  'AGE_85_89',
  'AGE_90_PLUS',
  'PATS'
)

# Columns to correlate
df_scatter = df_total %>% 
  select(-remove_vars) %>% 
  cor() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("METRIC_ONE") %>% 
  pivot_longer(!METRIC_ONE, names_to = "METRIC_TWO", values_to = "CORR") %>% 
  filter(
    CORR >= 0.3 | CORR <= -0.3,
    CORR != 1,
    !grepl("^DRUG_", METRIC_TWO),
    !grepl("ITEMS", METRIC_TWO),
    !grepl("COST_", METRIC_TWO)
  )

# Plot function
ggplot_scatter = function(var_ind){
  p = ggplot(df_total, aes_string(
    x = df_scatter$METRIC_ONE[var_ind],
    y = df_scatter$METRIC_TWO[var_ind]
    ))+
    geom_point()
  print(p)
}

# All plots
#lapply(1:nrow(df_scatter), ggplot_scatter)

# Select plots 
ggplot_select_scatter = function(var_one, var_two){
  p = ggplot(df_total, aes_string(
    x = var_one,
    y = var_two,
    color = 'MALE_PROP'
    ))+
    geom_point()+
    scale_color_viridis_b()
    #scale_color_gradient(low = "lightblue", high = "red")
  print(p)
}

# Select plots, now coloured by gender proportion (yellow and green more male)
ggplot_select_scatter('CHAPTER_STOMA_APPLIANCES', "CHAPTER_CARDIOVASCULAR_SYSTEM")
ggplot_select_scatter("CHAPTER_CENTRAL_NERVOUS_SYSTEM", "CHAPTER_CARDIOVASCULAR_SYSTEM")
ggplot_select_scatter("CHAPTER_APPLIANCES", "CHAPTER_CARDIOVASCULAR_SYSTEM")
ggplot_select_scatter("SECTION_LAXATIVES", "CHAPTER_CARDIOVASCULAR_SYSTEM")
ggplot_select_scatter("COST_PPM", "CHAPTER_CARDIOVASCULAR_SYSTEM")
ggplot_select_scatter("SECTION_ANALGESICS", "SECTION_DRUGS_USED_IN_PSYCHOSES_AND_RELATED_DISORDERS")

# Hchart
df_total %>% 
  filter(
    SECTION_LAXATIVES > 0,
    CHAPTER_CARDIOVASCULAR_SYSTEM > 0,
    row_number() <= 2000
  ) %>% 
  hchart(., "scatter", hcaes(
    SECTION_LAXATIVES, 
    CHAPTER_CARDIOVASCULAR_SYSTEM
    )) %>% 
  hc_xAxis(title = list(text = "Proportion of Prescribing from the Laxatives Section")) %>% 
  hc_yAxis(title = list(text = "Proportion of Prescribing from the Cardiovascular System Chapter")) %>% 
  hc_title(text = "Sample of <b>Laxatives Section</b> against <b>Cardiovascular Chapter</b> Prescribing Proportions per Care Home in FY21/22")

# 6. Age by gender and section/chapter heatmaps --------------------------------

# Create lazy table
data <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_MATCH_SLA"))

# BNF sections of interest
sections = c(
  'Antifungal drugs',
  'Laxatives',
  'Antidepressant drugs',
  'Drugs used in psychoses and related disorders',
  'Analgesics'
)

# Section-level metrics
df_gen = data %>% 
  group_by(AGE_BAND, GENDER) %>% 
  mutate(TOTAL = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  filter(SECTION_DESCR %in% sections) %>% 
  group_by(AGE_BAND, GENDER, SECTION_DESCR, TOTAL) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  mutate(PROP = round(ITEMS / TOTAL, 5)) %>% 
  collect() %>% 
  arrange(SECTION_DESCR, AGE_BAND, GENDER) %>% 
  filter(!is.na(GENDER))

# Plot heatmaps
for(i in sections){
  p = df_gen %>% 
    filter(SECTION_DESCR == i) %>% 
    hchart(., 
           "heatmap", 
           hcaes(GENDER, AGE_BAND, value = PROP),
           dataLabels = list(enabled = TRUE)
    ) %>% 
    hc_title(text = i)
  print(p)
}

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

# Section-level metrics
df_gen_ch = data %>% 
  group_by(AGE_BAND, GENDER) %>% 
  mutate(TOTAL = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  group_by(AGE_BAND, GENDER, CHAPTER_DESCR, TOTAL) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  mutate(PROP = round(ITEMS / TOTAL, 5)) %>% 
  collect() %>% 
  arrange(CHAPTER_DESCR, AGE_BAND, GENDER) %>% 
  filter(
    !is.na(GENDER),
    CHAPTER_DESCR %in% chapters
    )

# Plot heatmaps
for(i in chapters){
  p = df_gen_ch %>% 
    filter(CHAPTER_DESCR == i) %>% 
    hchart(., 
           "heatmap", 
           hcaes(GENDER, AGE_BAND, value = PROP),
           dataLabels = list(enabled = TRUE)
    ) %>% 
    hc_title(text = i)
  print(p)
}

# Hchart
df_gen %>% 
  filter(SECTION_DESCR == "Antidepressant drugs") %>% 
  hchart(., 
         "heatmap", 
         hcaes(GENDER, AGE_BAND, value = PROP),
         dataLabels = list(enabled = TRUE)
  ) %>% 
  hc_xAxis(title = list(text = "Gender")) %>% 
  hc_yAxis(title = list(text = "Age Band")) %>% 
  hc_title(text = "Proportion of Prescribing of the <b>Antidepressant Drug Section</b> by Gender and Age Band") %>% 
  hc_legend(enabled = FALSE)

# 7. Quick check of distinct pat count vs ch number of beds --------------------

# Chart
df_total %>% 
  ggplot(aes(NUMBER_OF_BEDS, PATS))+
  geom_jitter(size = 0.2)+
  geom_abline(color = "red", size = 1.5)+
  xlim(0, 200)+
  ylim(0,500)

# Chart
df_total %>% 
  filter(
    NUMBER_OF_BEDS > 0,
    row_number() <= 2000
    ) %>% 
  hchart(., "scatter", hcaes(NUMBER_OF_BEDS, PATS)) %>% 
  hc_xAxis(title = list(text = "Number of Beds")) %>% 
  hc_yAxis(title = list(text = "Number of Distinct Patients")) %>% 
  hc_title(text = "Sample of the Number of Beds against the Number of Distinct Patients per Care Home in FY21/22")

# Distribution of rounded number of beds per patient
df_total %>% 
  mutate(BEDS_PER_PAT = ceiling(PATS / NUMBER_OF_BEDS)) %>% 
  count(BEDS_PER_PAT) %>% 
  filter(BEDS_PER_PAT != Inf) %>% 
  mutate(
    BEDS_PER_PAT = case_when(
      BEDS_PER_PAT >= 10 ~ "Ten or more",
      T ~ as.character(BEDS_PER_PAT)
    ),
    BEDS_PER_PAT = factor(BEDS_PER_PAT)
  ) %>% 
  ggplot(aes(BEDS_PER_PAT, n))+
  geom_col()+
  coord_flip()

# 8. Composite rating feature view ---------------------------------------------

# Trend columns
descending_vars = c(
  "BNF_DIAZEPAM",
  "SECTION_DRUGS_USED_IN_PSYCHOSES_AND_RELATED_DISORDERS",
  "CHAPTER_RESPIRATORY_SYSTEM",
  "CHAPTER_NUTRITION_AND_BLOOD",
  "DAMN",
  "NSAID",
  "ACB_9"
)

# Trend columns
ascending_vars = c(
  "BNF_FENTANYL",
  "BNF_DONEPEZIL_HYDROCHLORIDE",
  "PARAGRAPH_URINARY_TRACT_INFECTIONS",
  "PARAGRAPH_OPIOID_ANALGESICS",
  "SECTION_ANALGESICS",
  "CHAPTER_ANAESTHESIA",
  "CHAPTER_MALIGNANT_DISEASE_AND_IMMUNOSUPPRESSION",
  "CHAPTER_INCONTINENCE_APPLIANCES",
  "CHAPTER_EAR_NOSE_AND_OROPHARYNX",
  "CHAPTER_MUSCULOSKELETAL_AND_JOINT_DISEASES",
  "CHAPTER_INFECTIONS",
  "CHAPTER_INCONTINENCE_APPLIANCES"
)

# Create 
asc_decile_rank = function(x) abs(ntile(x, 4) - 11)
desc_decile_rank = function(x) ntile(x, 4)

# Rank df
df_rank = df_total %>% 
  select(
    MATCH_SLA_STD, 
    descending_vars,
    ascending_vars
    ) %>% 
  mutate_at(.vars = ascending_vars, .funs = asc_decile_rank) %>% 
  mutate_at(.vars = descending_vars, .funs = desc_decile_rank) %>% 
  mutate(SCORE = rowSums(.[2:ncol(.)])) %>% 
  inner_join(
    df_total %>% 
      select(MATCH_SLA_STD, CURRENT_RATING)
  ) %>% 
  filter(
    CURRENT_RATING != "Inspected but not rated",
    CURRENT_RATING != "Unknown"
  ) %>% 
  mutate(
    CURRENT_RATING = factor(
      CURRENT_RATING, 
      levels = c("Outstanding", "Good", "Inadequate", "Requires improvement")
    ))

# Chart 1
df_rank %>% 
  ggplot(aes(SCORE, color = CURRENT_RATING)) + 
  geom_density(size = 1)

# Chart 2
df_rank %>% 
  filter(
    CURRENT_RATING != "Requires improvement",
    CURRENT_RATING != "Good"
  ) %>% 
  ggplot(aes(SCORE, color = CURRENT_RATING)) + 
  geom_density(size = 1)

# Classifying scores into groups using SCORE of 112 as boundary
df_rank %>% 
  filter(
    CURRENT_RATING != "Requires improvement",
    CURRENT_RATING != "Good"
  ) %>% 
  mutate(
    GROUP = as.numeric(SCORE >= 112),
    GROUP = ifelse(GROUP == 0, "Outstanding", "Inadequate")
    ) %>% 
  count(GROUP, CURRENT_RATING) %>% 
  mutate(CLASS = ifelse(GROUP == CURRENT_RATING, "Correct", "Incorrect")) %>% 
  group_by(CLASS) %>%
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(
    TOTAL = sum(n),
    PROP = round(n / TOTAL, 2)
  )

# 9. Provider Size -------------------------------------------------------------

# Create lazy table
cqc <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_CQC_20230502"))

# Dates for filter
start_date = "2021-04-01"
end_date =   "2022-03-31"

# With provider features
df_prov = cqc %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    CH_FLAG = 1L
  ) %>% 
  filter(
    !is.na(UPRN),
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  mutate(UPRN = as.numeric(UPRN)) %>% 
  collect() %>% 
  group_by(PROVIDER_ID) %>% 
  mutate(PROVIDER_SIZE = n_distinct(UPRN)) %>% 
  ungroup() %>% 
  inner_join(
    df_total,
    by = "UPRN"
  ) %>% 
  group_by(PROVIDER_ID) %>% 
  mutate(PROVIDER_SIZE = n_distinct(UPRN)) %>% 
  ungroup() %>% 
  select(UPRN, PROVIDER_ID, PROVIDER_SLA, PROVIDER_SIZE)

# Columns to remove
remove_vars = c(
  "UPRN",
  'CURRENT_RATING',
  'MATCH_SLA_STD',
  'PARENT_UPRN',
  'NURSING_HOME_FLAG',
  'RESIDENTIAL_HOME_FLAG',
  'MAX_MONTHLY_PATIENTS',
  'NUMBER_OF_BEDS',
  'MONTHS',
  'ITEMS',
  'NIC',
  'MALE',
  'FEMALE',
  'UNKNOWN',
  'AGE_65_69',
  'AGE_70_74',
  'AGE_75_79',
  'AGE_80_84',
  'AGE_85_89',
  'AGE_90_PLUS',
  'PATS'
)

# Columns to correlate
df_scatter = df_prov %>% 
  inner_join(
    df_total,
    by = "UPRN"
    ) %>% 
  mutate(
    PROVIDER_SIZE = case_when(
      PROVIDER_SIZE >= 11 & PROVIDER_SIZE <= 20 ~ "11-20",
      PROVIDER_SIZE >= 21 & PROVIDER_SIZE <= 50 ~ "21-50",
      PROVIDER_SIZE >= 51 & PROVIDER_SIZE <= 100 ~ "51-100",
      PROVIDER_SIZE >= 101 ~ "100+",
      T ~ as.character(PROVIDER_SIZE)
    ),
    PROVIDER_SIZE = factor(
      PROVIDER_SIZE,
      levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11-20", "21-50", "51-100", "100+")
    )
  ) %>% 
  select(-remove_vars) %>% 
  select(-c(PROVIDER_SLA, PROVIDER_ID)) %>% 
  group_by(PROVIDER_SIZE) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup()

# Columns for loop
cols = names(df_scatter)[!names(df_scatter) == "PROVIDER_SIZE"]

# Create Charts
for(i in cols){
  p = ggplot(df_scatter, aes_string('PROVIDER_SIZE', i))+
    geom_col()
  print(p)
}

# Get top 100 providers
top_prov = df_prov %>% 
  select(PROVIDER_ID, PROVIDER_SIZE) %>% 
  distinct() %>% 
  arrange(desc(PROVIDER_SIZE)) %>% 
  top_n(100) %>% 
  select(PROVIDER_ID) %>% 
  pull()

# Filter provider data
top_prov_df = df_prov %>% 
  filter(PROVIDER_ID %in% top_prov) %>% 
  inner_join(
    df_total,
    by = "UPRN"
  ) %>% 
  select(-remove_vars) %>% 
  group_by(PROVIDER_ID, PROVIDER_SLA, PROVIDER_SIZE) %>% 
  summarise_all(.funs = mean) %>% 
  ungroup()

# Columns for loop
cols = names(df_scatter)[!names(df_scatter) %in% c("PROVIDER_SLA", "PROVIDER_SIZE", "PROVIDER_ID")]

# Plot function
plot_prov_bar = function(vars){
  p = top_prov_df %>% 
    rename_at(vars, ~"METRIC") %>% 
    select(PROVIDER_SLA, METRIC) %>% 
    arrange(METRIC) %>% 
    hchart(., "area", hcaes(PROVIDER_SLA, METRIC)) %>% 
    hc_title(text = vars) %>% 
    hc_xAxis(categories = FALSE)
  print(p)
}

# Check plots
lapply(cols[12:22], plot_prov_bar)

# Single plot
plot_prov_bar("DRUG")

top_prov_df %>% 
  ggplot(., hcaes(DRUG_20))+
  geom_density()

df_total %>% 
  ggplot(., hcaes(DRUG_20))+
  geom_density()
  
  

df_prov = df_total %>% 
  left_join(prov) %>% 
  mutate(PROVIDER_SIZE = ifelse(is.na(PROVIDER_SIZE), 1, PROVIDER_SIZE))

# Disconnect and clean ---------------------------------------------------------

DBI::dbDisconnect(con); rm(list = ls()); gc()

#-------------------------------------------------------------------------------