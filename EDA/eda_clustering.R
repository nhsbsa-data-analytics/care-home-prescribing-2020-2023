
# Libraries
library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)
library(tidyr)

# Get data
tictoc::tic()
source("EDA/eda_metric_generation.R")
tictoc::toc()

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

# PCA --------------------------------------------------------------------------

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

# Clustering -------------------------------------------------------------------

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

# Chapter proportions by mean DRUG_TOTAL metric --------------------------------

drug_chapter_vars = c(names(df)[grepl("CHAPTER", names(df))], "DRUG_TOTAL")

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

down_chapters = c(
  "CHAPTER_ANAESTHESIA",
  "CHAPTER_INFECTIONS",
  "CHAPTER_EYE",
  "CHAPTER_CARDIOVASCULAR_SYSTEM",
  "CHAPTER_ENDOCRINE_SYSTEM"
)

up_chapters = c(
  "CHAPTER_RESPIRATORY_SYSTEM",
  "CHAPTER_NUTRITION_AND_BLOOD",
  "CHAPTER_MUSCULOSKELETAL_AND_JOINT_DISEASES",
  "CHAPTER_GASTRO_INTESTINAL_SYSTEM"
)

for(i in up_chapters){
  p = df_bar %>% 
    filter(CHAPTER == i) %>% 
    ggplot(aes(DRUG_TOTAL, MEAN, fill = CHAPTER))+
    geom_bar(stat = "identity")
  print(p)
}

for(i in down_chapters){
  p = df_bar %>% 
    filter(CHAPTER == i) %>% 
    ggplot(aes(DRUG_TOTAL, MEAN, fill = CHAPTER))+
    geom_bar(stat = "identity")
  print(p)
}

df_bar %>% 
  filter(CHAPTER %in% up_chapters) %>% 
  ggplot(aes(DRUG_TOTAL, MEAN, fill = CHAPTER))+
  geom_bar(stat = "identity")

df_bar %>% 
  filter(CHAPTER %in% down_chapters) %>% 
  ggplot(aes(DRUG_TOTAL, MEAN, fill = CHAPTER))+
  geom_bar(stat = "identity")

# Rating by metric -------------------------------------------------------------


# Disconnect from database
DBI::dbDisconnect(con)