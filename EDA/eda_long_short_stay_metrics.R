
# library
library(dplyr)
library(dbplyr)
library(highcharter)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create lazy table
base <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20250331"))

# Distinct ch patients: 1.2m (for 1+ month across entire time period)
ch_pats = base %>% 
  filter(
    CH_FLAG == 1,
    !is.na(NHS_NO)
    ) %>% 
  select(NHS_NO) %>% 
  distinct()

# Base data for chart
df_national = base %>% 
  #filter(PCD_LAD_NAME == "Newcastle upon Tyne") %>% 
  # Only pats with a ch item
  inner_join(ch_pats) %>%  
  group_by(NHS_NO, CH_FLAG, YEAR_MONTH) %>% 
  summarise(
    UNIQUE_MEDICINES = n_distinct(
      case_when(
        CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
      )
    ),
    ITEMS = sum(ITEM_COUNT),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  # CALC PART 1: Add a flag each time the ch_flag 'flips' (ordered by YEAR_MONTH)
  mutate(
    FLIP = sql("CASE WHEN CH_FLAG != LAG(CH_FLAG, 1, CH_FLAG) OVER (PARTITION BY NHS_NO ORDER BY YEAR_MONTH) THEN 1 ELSE 0 END")
    ) %>% 
  # CALC PART 2: Use cumsum to generate a 'run label' (i.e. each block of prescribing per NHS_No and CH_FLAG)
  group_by(NHS_NO) %>% 
  arrange(NHS_NO, YEAR_MONTH) %>% 
  mutate(RUN_LABEL = cumsum(FLIP)) %>% 
  ungroup() %>% 
  # CALC PART 3: Calculate row count per NHS_NO and RUN_LABEL
  group_by(NHS_NO, RUN_LABEL) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(SEQ = row_number()) %>% 
  ungroup() %>% 
  filter(CH_FLAG == 1) %>% 
  # Group data for bar charts
  mutate(
    TEN_PLUS = ifelse(UNIQUE_MEDICINES >= 10, 1, 0),
    SEQ_GROUP = case_when(
      SEQ >= 2 & SEQ <= 4 ~ "1-3 Months",
      SEQ >= 5 & SEQ <= 7 ~ "4-6 Months",
      SEQ >= 8 & SEQ <= 10 ~ "7-9 Months",
      SEQ >= 11 & SEQ <= 13 ~ "10-12 Months",
      SEQ >= 14 ~ "13+ Months"
      )
    ) %>% 
  group_by(SEQ_GROUP) %>% 
  summarise(
    ITEMS = sum(ITEMS),
    MEAN_UNIQUE_MEDS = round(mean(UNIQUE_MEDICINES), 2),
    TEN_PLUS_SUM = sum(TEN_PLUS),
    TOTAL_PM = n()
    ) %>% 
  ungroup() %>% 
  collect() %>% 
  # Plotting data
  mutate(
    SEQ_GROUP = factor(
      SEQ_GROUP,
      levels = c(
        "1-3 Months", 
        "4-6 Months", 
        "7-9 Months", 
        "10-12 Months", 
        "13+ Months"
      )),
    TEN_PLUS_PROP = round(100 * TEN_PLUS_SUM / TOTAL_PM, 2),
    MEAN_ITEMS_PPM = round(ITEMS / TOTAL_PM, 2)
    ) %>% 
  arrange(SEQ_GROUP) %>% 
  filter(!is.na(SEQ_GROUP))

# Bar charts -------------------------------------------------------------------

df_national %>% 
  hchart("column", hcaes(SEQ_GROUP, MEAN_ITEMS_PPM)) %>% 
  hc_title(text = "National - Mean Unique Meds")

df_national %>% 
  hchart("column", hcaes(SEQ_GROUP, TEN_PLUS_PROP)) %>% 
  hc_title(text = "National - Prop 10+ Unique Meds")

df_national %>% 
  hchart("column", hcaes(SEQ_GROUP, MEAN_UNIQUE_MEDS)) %>% 
  hc_title(text = "National - Mean Unique Meds")

# Local Authority option -------------------------------------------------------

df_lad = base %>% 
  # Only pats with a ch item
  inner_join(ch_pats) %>%  
  group_by(NHS_NO, CH_FLAG, YEAR_MONTH, PCD_LAD_NAME) %>% 
  summarise(
    UNIQUE_MEDICINES = n_distinct(
      case_when(
        CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
      )
    ),
    ITEMS = sum(ITEM_COUNT),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  # CALC PART 1: Add a flag each time the ch_flag 'flips' (ordered by YEAR_MONTH)
  mutate(
    FLIP = sql("CASE WHEN CH_FLAG != LAG(CH_FLAG, 1, CH_FLAG) OVER (PARTITION BY NHS_NO ORDER BY YEAR_MONTH) THEN 1 ELSE 0 END")
  ) %>% 
  # CALC PART 2: Use cumsum to generate a 'run label' (i.e. each block of prescribing per NHS_No and CH_FLAG)
  group_by(NHS_NO) %>% 
  arrange(NHS_NO, YEAR_MONTH) %>% 
  mutate(RUN_LABEL = cumsum(FLIP)) %>% 
  ungroup() %>% 
  # CALC PART 3: Calculate row count per NHS_NO and RUN_LABEL
  group_by(NHS_NO, RUN_LABEL) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(SEQ = row_number()) %>% 
  ungroup() %>% 
  filter(CH_FLAG == 1) %>% 
  # Group data for bar charts
  mutate(
    TEN_PLUS = ifelse(UNIQUE_MEDICINES >= 10, 1, 0),
    SEQ_GROUP = case_when(
      SEQ >= 2 & SEQ <= 4 ~ "1-3 Months",
      SEQ >= 5 & SEQ <= 7 ~ "4-6 Months",
      SEQ >= 8 & SEQ <= 10 ~ "7-9 Months",
      SEQ >= 11 & SEQ <= 13 ~ "10-12 Months",
      SEQ >= 14 ~ "13+ Months"
    )
  ) %>% 
  group_by(SEQ_GROUP, PCD_LAD_NAME) %>% 
  summarise(
    ITEMS = sum(ITEMS),
    MEAN_UNIQUE_MEDS = round(mean(UNIQUE_MEDICINES), 2),
    TEN_PLUS_SUM = sum(TEN_PLUS),
    TOTAL_PM = n()
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  # Plotting data
  mutate(
    SEQ_GROUP = factor(
      SEQ_GROUP,
      levels = c(
        "1-3 Months", 
        "4-6 Months", 
        "7-9 Months", 
        "10-12 Months", 
        "13+ Months"
      )),
    TEN_PLUS_PROP = round(100 * TEN_PLUS_SUM / TOTAL_PM, 2),
    MEAN_ITEMS_PPM = round(ITEMS / TOTAL_PM, 2)
  ) %>% 
  arrange(SEQ_GROUP) %>% 
  filter(!is.na(SEQ_GROUP))

# Function for all charts
lad_plots = function(lad_name){
  
  p1 = df_lad %>% 
    filter(PCD_LAD_NAME == lad_name) %>% 
    hchart("column", hcaes(SEQ_GROUP, MEAN_ITEMS_PPM)) %>% 
    hc_title(text = paste0(lad_name, " - Mean Items PPM"))
  
  p2 = df_lad %>% 
    filter(PCD_LAD_NAME == lad_name) %>% 
    hchart("column", hcaes(SEQ_GROUP, TEN_PLUS_PROP)) %>% 
    hc_title(text = paste0(lad_name, " - Prop 10+ Unique Meds"))
  
  p3 = df_lad %>% 
    filter(PCD_LAD_NAME == lad_name) %>% 
    hchart("column", hcaes(SEQ_GROUP, MEAN_UNIQUE_MEDS)) %>% 
    hc_title(text = paste0(lad_name, " - Mean Unique Meds"))
  
  print(p1)
  print(p2)
  print(p3)
}

# Examples
lad_plots("South Tyneside")
lad_plots("Hammersmith and Fulham")
lad_plots("South Staffordshire")
lad_plots("Hackney")

# Diff between 1st and Last SEQ_GROUP
df_lad_range = df_lad %>% 
  filter(SEQ_GROUP %in% c("1-3 Months", "13+ Months")) %>% 
  arrange(PCD_LAD_NAME, SEQ_GROUP) %>% 
  select(PCD_LAD_NAME, MEAN_UNIQUE_MEDS, TEN_PLUS_PROP, MEAN_ITEMS_PPM) %>% 
  group_by(PCD_LAD_NAME) %>%
  summarise(across(where(is.numeric), ~ diff(.x))) %>% 
  ungroup()

# ICB option -------------------------------------------------------------------

# Local Authority option -------------------------------------------------------

df_icb = base %>% 
  # Only pats with a ch item
  inner_join(ch_pats) %>%  
  group_by(NHS_NO, CH_FLAG, YEAR_MONTH, PCD_ICB_NAME) %>% 
  summarise(
    UNIQUE_MEDICINES = n_distinct(
      case_when(
        CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
      )
    ),
    ITEMS = sum(ITEM_COUNT),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  # CALC PART 1: Add a flag each time the ch_flag 'flips' (ordered by YEAR_MONTH)
  mutate(
    FLIP = sql("CASE WHEN CH_FLAG != LAG(CH_FLAG, 1, CH_FLAG) OVER (PARTITION BY NHS_NO ORDER BY YEAR_MONTH) THEN 1 ELSE 0 END")
  ) %>% 
  # CALC PART 2: Use cumsum to generate a 'run label' (i.e. each block of prescribing per NHS_No and CH_FLAG)
  group_by(NHS_NO) %>% 
  arrange(NHS_NO, YEAR_MONTH) %>% 
  mutate(RUN_LABEL = cumsum(FLIP)) %>% 
  ungroup() %>% 
  # CALC PART 3: Calculate row count per NHS_NO and RUN_LABEL
  group_by(NHS_NO, RUN_LABEL) %>% 
  arrange(YEAR_MONTH) %>% 
  mutate(SEQ = row_number()) %>% 
  ungroup() %>% 
  filter(CH_FLAG == 1) %>% 
  # Group data for bar charts
  mutate(
    TEN_PLUS = ifelse(UNIQUE_MEDICINES >= 10, 1, 0),
    SEQ_GROUP = case_when(
      SEQ >= 2 & SEQ <= 4 ~ "1-3 Months",
      SEQ >= 5 & SEQ <= 7 ~ "4-6 Months",
      SEQ >= 8 & SEQ <= 10 ~ "7-9 Months",
      SEQ >= 11 & SEQ <= 13 ~ "10-12 Months",
      SEQ >= 14 ~ "13+ Months"
    )
  ) %>% 
  group_by(SEQ_GROUP, PCD_ICB_NAME) %>% 
  summarise(
    ITEMS = sum(ITEMS),
    MEAN_UNIQUE_MEDS = round(mean(UNIQUE_MEDICINES), 2),
    TEN_PLUS_SUM = sum(TEN_PLUS),
    TOTAL_PM = n()
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  # Plotting data
  mutate(
    SEQ_GROUP = factor(
      SEQ_GROUP,
      levels = c(
        "1-3 Months",
        "4-6 Months", 
        "7-9 Months", 
        "10-12 Months", 
        "13+ Months"
      )),
    TEN_PLUS_PROP = round(100 * TEN_PLUS_SUM / TOTAL_PM, 2),
    MEAN_ITEMS_PPM = round(ITEMS / TOTAL_PM, 2)
  ) %>% 
  arrange(SEQ_GROUP) %>% 
  filter(!is.na(SEQ_GROUP))

# Function for all charts
icb_plots = function(icb_name){
  
  p1 = df_icb %>% 
    filter(PCD_ICB_NAME == icb_name) %>% 
    hchart("column", hcaes(SEQ_GROUP, MEAN_ITEMS_PPM)) %>% 
    hc_title(text = paste0(icb_name, " - Mean Items PPM"))
  
  p2 = df_icb %>% 
    filter(PCD_ICB_NAME == icb_name) %>% 
    hchart("column", hcaes(SEQ_GROUP, TEN_PLUS_PROP)) %>% 
    hc_title(text = paste0(icb_name, " - Prop 10+ Unique Meds"))
  
  p3 = df_icb %>% 
    filter(PCD_ICB_NAME == icb_name) %>% 
    hchart("column", hcaes(SEQ_GROUP, MEAN_UNIQUE_MEDS)) %>% 
    hc_title(text = paste0(icb_name, " - Mean Unique Meds"))
  
  print(p1)
  print(p2)
  print(p3)
}

# Examples
icb_plots("NHS Frimley ICB")
icb_plots("NHS Cambridgeshire and Peterborough ICB")
icb_plots("NHS Black Country ICB")

# Diff between 1st and Last SEQ_GROUP
df_icb_range = df_icb %>% 
  filter(SEQ_GROUP %in% c("1-3 Months", "13+ Months")) %>% 
  arrange(PCD_ICB_NAME, SEQ_GROUP) %>% 
  select(PCD_ICB_NAME, MEAN_UNIQUE_MEDS, TEN_PLUS_PROP, MEAN_ITEMS_PPM) %>% 
  group_by(PCD_ICB_NAME) %>%
  summarise(across(where(is.numeric), ~ diff(.x))) %>% 
  ungroup()

# Disconnect and clean
DBI::dbDisconnect(con); rm(list = ls()); gc()

# Archive ----------------------------------------------------------------------

# Function to generate seq groups
# create_seq_groups = function(df){
#   df %>% 
#     mutate(
#       SEQ_GROUP = case_when(
#         SEQ >= 2 & SEQ <= 4 ~ "1-3 Months",
#         SEQ >= 5 & SEQ <= 7 ~ "4-6 Months",
#         SEQ >= 8 & SEQ <= 10 ~ "7-9 Months",
#         SEQ >= 11 & SEQ <= 13 ~ "10-12 Months",
#         SEQ >= 14 ~ "13+ Months"
#       ),
#       SEQ_GROUP = factor(
#         SEQ_GROUP,
#         levels = c(
#           "1-3 Months", 
#           "4-6 Months", 
#           "7-9 Months", 
#           "10-12 Months", 
#           "13+ Months"
#         ))
#     )
# }

# Chart one: mean unique meds per month (marginal increase)
# df_plot %>% 
#   filter(
#     SEQ <= 13,
#     SEQ >= 2
#   ) %>% 
#   hchart("line", hcaes(SEQ, MEAN_UNIQUE_MEDS)) %>% 
#   hc_yAxis(min = 5, max = 8)

# Chart two: prop 10+ unique meds monthly (decrease then plateau)
# df_plot %>% 
#   filter(
#     SEQ <= 13,
#     SEQ >= 2
#   ) %>% 
#   hchart("line", hcaes(SEQ, PROP)) %>% 
#   hc_yAxis(min = 18, max = 24)

# Chart three: mean items PPM
# df_plot %>% 
#   arrange(SEQ) %>% 
#   mutate(ITEMS_PPM = ITEMS / TOTAL_PM) %>% 
#   filter(
#     SEQ <= 25,
#     SEQ >= 2
#   ) %>% 
#   hchart("line", hcaes(SEQ, ITEMS_PPM)) %>% 
#   hc_yAxis(min = 9, max = 12)