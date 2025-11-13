
# library
library(dplyr)
library(dbplyr)
library(rlang)
library(tidyr)
library(assertr)
library(assertr.alt)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create lazy table
base <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20250331"))

# RUN ONCE: SPEED UP CODE * 12 RUN ---------------------------------------------

# Functions
source("data-raw/workflow/workflow_helpers.R")

# Distinct ch patients: 1.2m (for 1+ month across entire time period)
ch_pats = base %>%
  filter(
    CH_FLAG == 1,
    !is.na(NHS_NO)
  ) %>%
  group_by(NHS_NO) %>% 
  summarise(
    CH_FLAG = max(CH_FLAG),
    RES_FLAG = ifelse(is.na(max(RESIDENTIAL_HOME_FLAG)), 0, max(RESIDENTIAL_HOME_FLAG)),
    NURS_FLAG = ifelse(is.na(max(NURSING_HOME_FLAG)), 0, max(NURSING_HOME_FLAG))
  ) %>% 
  ungroup()

# Define table name
table_name = "INT646_DISTINCT_CH_PATS"

# Remove temp table (DROP TABLE FUN NOT WORKING ...)
if(DBI::dbExistsTable(con, table_name)){DBI::dbRemoveTable(con, table_name)}

# Temp table save (deleted at end of script)
ch_pats %>% compute_with_parallelism(table_name, 32)

# Generate output---------------------------------------------------------------

# Create lazy table
pats <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_DISTINCT_CH_PATS"))

# Define bnf strings
p1 = "Oral anticoagulants"
p2 = "Antiplatelet drugs"
c1 = "INR blood testing reagents"

# Function for all geography types
geo_long_short_stay_metrics = function(GEO=NULL, ch_type){

  # Group by a geography if present
  if(is.null(GEO)){
    group_one = c('NHS_NO', 'CH_FLAG', 'YEAR_MONTH')
    group_two = c('SEQ_GROUP')
  }else{
    group_one = c('NHS_NO', 'CH_FLAG', 'YEAR_MONTH', GEO)
    group_two = c('SEQ_GROUP', GEO)
  }
  
  # Determine patient ch type list
  pats = pats %>% 
    transmute(
      NHS_NO,
      CH_FLAG := {{ ch_type }}
    ) %>% 
    filter(CH_FLAG == 1) %>% 
    select(NHS_NO)
  
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
    # CALC PART 3: Remove months with 'mixed' (CH and non-CH) prescribing from count
    group_by(NHS_NO, YEAR_MONTH) %>%
    filter(n_distinct(CH_FLAG) == 1) %>%
    ungroup() %>%
    # CALC PART 4: Calculate row count per NHS_NO and RUN_LABEL
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
        SEQ >= 1 & SEQ <= 3 ~ "1-3 Months",
        SEQ >= 4 & SEQ <= 6 ~ "4-6 Months",
        SEQ >= 7 & SEQ <= 9 ~ "7-9 Months",
        SEQ >= 10 & SEQ <= 12 ~ "10-12 Months",
        SEQ >= 13 ~ "13+ Months"
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
      # CH Type
      CH_TYPE = as_string(ensym(ch_type)),
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
      # METRIC CALC INFO (keep for potential future reference)
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
      UNIQ_MEDS_PPM,
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

# Start time: ~1 hr total
Sys.time()

# National data
national_ch = geo_long_short_stay_metrics(NULL, CH_FLAG)
national_res = geo_long_short_stay_metrics(NULL, RES_FLAG)
national_nurs = geo_long_short_stay_metrics(NULL, NURS_FLAG)

# Region data
region_ch = geo_long_short_stay_metrics('PCD_REGION_NAME', CH_FLAG)
region_res = geo_long_short_stay_metrics('PCD_REGION_NAME', RES_FLAG)
region_nurs = geo_long_short_stay_metrics('PCD_REGION_NAME', NURS_FLAG)

# Ics data
ics_ch = geo_long_short_stay_metrics('PCD_ICB_NAME', CH_FLAG)
ics_res = geo_long_short_stay_metrics('PCD_ICB_NAME', RES_FLAG)
ics_nurs = geo_long_short_stay_metrics('PCD_ICB_NAME', NURS_FLAG)

# Lad data
lad_ch = geo_long_short_stay_metrics('PCD_LAD_NAME', CH_FLAG)
lad_res = geo_long_short_stay_metrics('PCD_LAD_NAME', RES_FLAG)
lad_nurs = geo_long_short_stay_metrics('PCD_LAD_NAME', NURS_FLAG)

# End time
Sys.time()

# Final process and quick validation -------------------------------------------

# Region (7) + ICB (42) + LAD (295) + national (1) count
geo_count = 345
metric_count = 10
seq_count = 5
ch_type = 3
expected_rows = geo_count * metric_count * seq_count * ch_type

# Bind and process
mod_short_longstay_df = rbind(
  national_ch,
  national_res,
  national_nurs,
  region_ch,
  region_res,
  region_nurs,
  ics_ch,
  ics_res,
  ics_nurs,
  lad_ch,
  lad_res,
  lad_nurs
  ) %>% 
  transmute(
    CH_TYPE = case_when(
      CH_TYPE == "CH_FLAG" ~ "Care home",
      CH_TYPE == "RES_FLAG" ~ "Residential home",
      CH_TYPE == "NURS_FLAG" ~ "Nursing home"
    ),
    SEQ_GROUP,
    GEO_TYPE = case_when(
      GEO_TYPE == "PCD_REGION_NAME" ~ "Region",
      GEO_TYPE == "PCD_ICB_NAME" ~ "ICS",
      GEO_TYPE == "PCD_LAD_NAME" ~ "Local Authority",
      TRUE ~ GEO_TYPE
    ),
    GEO,
    ITEMS_PPM,
    COST_PPM,
    UNIQ_MEDS_PPM,
    PCT_PM_GTE_SIX,
    PCT_PM_GTE_TEN,
    PCT_PM_DAMN,
    PCT_PM_ACB,
    PCT_PM_ACAP,
    PCT_PM_FALLS,
    UNIQ_MEDS_FALLS_PPM
  ) %>% 
  tidyr::pivot_longer(
    cols = where(is.numeric),
    names_to = "METRIC",
    values_to = "VALUE"
  ) %>% 
  tidyr::complete(
    CH_TYPE,
    SEQ_GROUP,
    tidyr::nesting(GEO_TYPE, GEO),
    METRIC
  ) %>% 
  mutate(
    VALUE = ifelse(is.na(VALUE), 0, VALUE),
    GEO_TYPE = factor(
      GEO_TYPE,
      levels = c(
        "Overall",
        "Region",
        "ICS",
        "Local Authority"
        )
      )
    ) %>% 
  verify(nrow.alt(.) == expected_rows) %>%
  assert.alt(not_na.alt, SEQ_GROUP, GEO_TYPE, GEO, METRIC, VALUE)

# Quick validation -------------------------------------------------------------

# Add to data-raw/
usethis::use_data(mod_short_longstay_df, overwrite = TRUE)

# Remove temp table (DROP TABLE FUN NOT WORKING ...)
if(DBI::dbExistsTable(con, table_name)){DBI::dbRemoveTable(con, table_name)}

# Disconnect and clean
DBI::dbDisconnect(con); rm(list = ls()); gc()

# Archive - manual data validation check (metric value range sense check) ------

# # Helper to check range diff across timeframe per df geography
# get_geo_range = function(df, geo){
#   df %>%
#     filter(GEO_TYPE == geo) %>% 
#     tidyr::pivot_wider(names_from = METRIC, values_from = VALUE) %>% 
#     filter(SEQ_GROUP %in% c("1-3 Months", "13+ Months")) %>%
#     arrange(GEO, SEQ_GROUP) %>% 
#     group_by(GEO) %>%
#     summarise(across(where(is.numeric), ~ diff(.x))) %>%
#     ungroup() 
# }
# 
# # Diff between 1st and Last SEQ_GROUP
# region_range = get_geo_range(mod_short_longstay_df, "Region")
# icb_range = get_geo_range(mod_short_longstay_df, "ICS")
# lad_range = get_geo_range(mod_short_longstay_df, "Local Authority")
