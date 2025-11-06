
# library
library(dplyr)
library(dbplyr)
source("data-raw/workflow/workflow_helpers.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create lazy table
base <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20250331"))

# RUN ONCE: SPEED UP CODE * 4 RUN ----------------------------------------------

# Distinct ch patients: 1.2m (for 1+ month across entire time period)
ch_pats = base %>%
  filter(
    CH_FLAG == 1,
    !is.na(NHS_NO)
  ) %>%
  select(NHS_NO) %>%
  distinct()

# Define table name
table_name = "INT646_DISTINCT_CH_PATS"

# Remove temp table (DROP TABLE FUN NOT WORKING ...)
if(DBI::dbExistsTable(con, table_name)){DBI::dbRemoveTable(con, table_name)}

# Just format postcode
ch_pats %>%
  compute_with_parallelism(table_name, 32)

# Generate output---------------------------------------------------------------

# Create lazy table
pats <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_DISTINCT_CH_PATS"))

# Define bnf strings
p1 = "Oral anticoagulants"
p2 = "Antiplatelet drugs"
c1 = "INR blood testing reagents"

# Function for all geography types
geo_long_short_stay_metrics = function(GEO=NULL){

  # Group by a geography if present
  if(is.null(GEO)){
    group_one = c('NHS_NO', 'CH_FLAG', 'YEAR_MONTH')
    group_two = c('SEQ_GROUP')
  }else{
    group_one = c('NHS_NO', 'CH_FLAG', 'YEAR_MONTH', GEO)
    group_two = c('SEQ_GROUP', GEO)
  }
  
  # Only pats with a ch item
  df = base %>% 
    inner_join(pats) %>% 
    group_by(!!!rlang::syms(group_one)) %>% 
    summarise(
      UNIQUE_MEDICINES = n_distinct(
        case_when(
          CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      FALLS = n_distinct(
        case_when(
          FALLS_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      ACB = n_distinct(
        case_when(
          ACB_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      DAMN = n_distinct(
        case_when(
          DAMN_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      ACAP = n_distinct(
        case_when(
          (PARAGRAPH_DESCR == p1 | PARAGRAPH_DESCR == p2) & CHEMICAL_SUBSTANCE_BNF_DESCR != c1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      ITEMS = sum(ITEM_COUNT),
      COST = sum(ITEM_PAY_DR_NIC),
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
      ACAP_ONE = ifelse(ACAP >= 1, 1, 0),
      ACAP_TWO = ifelse(ACAP >= 2, 1, 0),
      DAMN_ONE = ifelse(DAMN >= 1, 1, 0),
      DAMN_TWO = ifelse(DAMN >= 2, 1, 0),
      ACB_ONE = ifelse(ACB >= 1, 1, 0),
      ACB_TWO = ifelse(ACB >= 2, 1, 0),
      FALLS_THREE = ifelse(FALLS >= 3, 1, 0),
      SIX_PLUS = ifelse(UNIQUE_MEDICINES >= 6, 1, 0),
      TEN_PLUS = ifelse(UNIQUE_MEDICINES >= 10, 1, 0),
      SEQ_GROUP = case_when(
        SEQ >= 2 & SEQ <= 4 ~ "1-3 Months",
        SEQ >= 5 & SEQ <= 7 ~ "4-6 Months",
        SEQ >= 8 & SEQ <= 10 ~ "7-9 Months",
        SEQ >= 11 & SEQ <= 13 ~ "10-12 Months",
        SEQ >= 14 ~ "13+ Months"
      )
    ) %>% 
    # Second group-grouping
    group_by(!!!rlang::syms(group_two)) %>% 
    summarise(
      ITEMS = sum(ITEMS),
      COST = sum(COST / 100),
      UNIQ_MEDS_PPM = round(mean(UNIQUE_MEDICINES), 2),
      SIX_PLUS_SUM = sum(SIX_PLUS),
      TEN_PLUS_SUM = sum(TEN_PLUS),
      TOTAL_PM = n(),
      DAMN_ONE_SUM = sum(DAMN_ONE),
      DAMN_TWO_SUM = sum(DAMN_TWO),
      ACB_ONE_SUM = sum(ACB_ONE),
      ACB_TWO_SUM = sum(ACB_TWO),
      ACAP_ONE_SUM = sum(ACAP_ONE),
      ACAP_TWO_SUM = sum(ACAP_TWO),
      FALLS_THREE_SUM = sum(FALLS_THREE),
      UNIQ_MEDS_FALLS_PPM = round(mean(FALLS), 2)
    ) %>% 
    ungroup() %>% 
    collect() %>% 
    transmute(
      # GROUPING INFO
      SEQ_GROUP = factor(
        SEQ_GROUP,
        levels = c(
          "1-3 Months", 
          "4-6 Months", 
          "7-9 Months", 
          "10-12 Months", 
          "13+ Months"
        )),
      GEO_TYPE = if (is.null(GEO)) "Overall" else GEO,
      GEO      = if (is.null(GEO)) "Overall" else .[[GEO]],
      # METRIC CALC INFO
      ITEMS,
      COST,
      SIX_PLUS_SUM,
      TEN_PLUS_SUM,
      TOTAL_PM,
      DAMN_ONE_SUM,
      DAMN_TWO_SUM,
      ACB_ONE_SUM,
      ACB_TWO_SUM,
      ACAP_ONE_SUM,
      ACAP_TWO_SUM,
      FALLS_THREE_SUM,
      # METRIC OUTPUT
      ITEMS_PPM = round(ITEMS / TOTAL_PM, 2),
      COST_PPM = round(COST / TOTAL_PM, 2),
      PCT_PM_GTE_SIX = round(100 * SIX_PLUS_SUM / TOTAL_PM, 2),
      PCT_PM_GTE_TEN = round(100 * TEN_PLUS_SUM / TOTAL_PM, 2),
      PCT_PM_DAMN = round(100 * DAMN_TWO_SUM / DAMN_ONE_SUM, 2),
      PCT_PM_ACB = round(100 * ACB_TWO_SUM / ACB_ONE_SUM, 2),
      PCT_PM_ACAP = round(100 * ACAP_TWO_SUM / ACAP_ONE_SUM, 2),
      PCT_PM_FALLS = round(100 * FALLS_THREE_SUM / TOTAL_PM, 2),
      UNIQ_MEDS_FALLS_PPM
    ) %>% 
    arrange(SEQ_GROUP) %>% 
    filter(
      !is.na(SEQ_GROUP),
      !is.na(GEO)
      )
  
  # Return
  return(df)
}

# Generate data: 25 mins
Sys.time()
national = geo_long_short_stay_metrics()
region = geo_long_short_stay_metrics('PCD_REGION_NAME')
ics = geo_long_short_stay_metrics('PCD_ICB_NAME')
lad = geo_long_short_stay_metrics('PCD_LAD_NAME')
Sys.time()

# Total data
mod_short_longstay_df = rbind(national, region, ics, lad)

# Add to data-raw/
usethis::use_data(mod_short_longstay_df, overwrite = TRUE)

# Remove temp table (DROP TABLE FUN NOT WORKING ...)
if(DBI::dbExistsTable(con, table_name)){DBI::dbRemoveTable(con, table_name)}

# Disconnect and clean
DBI::dbDisconnect(con); rm(list = ls()); gc()

# Single output ----------------------------------------------------------------

# Only pats with a ch item
df = base %>% 
  inner_join(pats) %>% 
  filter(PCD_LAD_NAME == "Doncaster") %>% 
  group_by(NHS_NO, CH_FLAG, YEAR_MONTH) %>% 
  summarise(
    UNIQUE_MEDICINES = n_distinct(
      case_when(
        CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
      )
    ),
    ITEMS = sum(ITEM_COUNT),
    COST = sum(ITEM_PAY_DR_NIC),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  # CALC PART 1: Add a flag each time the ch_flag 'flips' (ordered by YEAR_MONTH)
  mutate(
    FLIP = sql("CASE WHEN CH_FLAG != LAG(CH_FLAG, 1, CH_FLAG) OVER (PARTITION BY NHS_NO ORDER BY YEAR_MONTH) THEN 1 ELSE 0 END")
  ) %>% 
  collect()

df %>% 
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
    ACAP_ONE = ifelse(ACAP >= 1, 1, 0),
    ACAP_TWO = ifelse(ACAP >= 2, 1, 0),
    DAMN_ONE = ifelse(DAMN >= 1, 1, 0),
    DAMN_TWO = ifelse(DAMN >= 2, 1, 0),
    ACB_ONE = ifelse(ACB >= 1, 1, 0),
    ACB_TWO = ifelse(ACB >= 2, 1, 0),
    FALLS_THREE = ifelse(FALLS >= 3, 1, 0),
    SIX_PLUS = ifelse(UNIQUE_MEDICINES >= 6, 1, 0),
    TEN_PLUS = ifelse(UNIQUE_MEDICINES >= 10, 1, 0),
    SEQ_GROUP = case_when(
      SEQ >= 2 & SEQ <= 4 ~ "1-3 Months",
      SEQ >= 5 & SEQ <= 7 ~ "4-6 Months",
      SEQ >= 8 & SEQ <= 10 ~ "7-9 Months",
      SEQ >= 11 & SEQ <= 13 ~ "10-12 Months",
      SEQ >= 14 ~ "13+ Months"
    )
  ) %>% 
  # Second group-grouping
  group_by(!!!rlang::syms(group_two)) %>% 
  summarise(
    ITEMS = sum(ITEMS),
    COST = sum(COST / 100),
    UNIQ_MEDS_PPM = round(mean(UNIQUE_MEDICINES), 2),
    SIX_PLUS_SUM = sum(SIX_PLUS),
    TEN_PLUS_SUM = sum(TEN_PLUS),
    TOTAL_PM = n(),
    DAMN_ONE_SUM = sum(DAMN_ONE),
    DAMN_TWO_SUM = sum(DAMN_TWO),
    ACB_ONE_SUM = sum(ACB_ONE),
    ACB_TWO_SUM = sum(ACB_TWO),
    ACAP_ONE_SUM = sum(ACAP_ONE),
    ACAP_TWO_SUM = sum(ACAP_TWO),
    FALLS_THREE_SUM = sum(FALLS_THREE),
    UNIQ_MEDS_FALLS_PPM = round(mean(FALLS), 2)
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  transmute(
    # GROUPING INFO
    SEQ_GROUP = factor(
      SEQ_GROUP,
      levels = c(
        "1-3 Months", 
        "4-6 Months", 
        "7-9 Months", 
        "10-12 Months", 
        "13+ Months"
      )),
    GEO_TYPE = if (is.null(GEO)) "Overall" else GEO,
    GEO      = if (is.null(GEO)) "Overall" else .[[GEO]],
    # METRIC CALC INFO
    ITEMS,
    COST,
    SIX_PLUS_SUM,
    TEN_PLUS_SUM,
    TOTAL_PM,
    DAMN_ONE_SUM,
    DAMN_TWO_SUM,
    ACB_ONE_SUM,
    ACB_TWO_SUM,
    ACAP_ONE_SUM,
    ACAP_TWO_SUM,
    FALLS_THREE_SUM,
    # METRIC OUTPUT
    ITEMS_PPM = round(ITEMS / TOTAL_PM, 2),
    COST_PPM = round(COST / TOTAL_PM, 2),
    PCT_PM_GTE_SIX = round(100 * SIX_PLUS_SUM / TOTAL_PM, 2),
    PCT_PM_GTE_TEN = round(100 * TEN_PLUS_SUM / TOTAL_PM, 2),
    PCT_PM_DAMN = round(100 * DAMN_TWO_SUM / DAMN_ONE_SUM, 2),
    PCT_PM_ACB = round(100 * ACB_TWO_SUM / ACB_ONE_SUM, 2),
    PCT_PM_ACAP = round(100 * ACAP_TWO_SUM / ACAP_ONE_SUM, 2),
    PCT_PM_FALLS = round(100 * FALLS_THREE_SUM / TOTAL_PM, 2),
    UNIQ_MEDS_FALLS_PPM
  ) %>% 
  arrange(SEQ_GROUP) %>% 
  filter(
    !is.na(SEQ_GROUP),
    !is.na(GEO)
  )