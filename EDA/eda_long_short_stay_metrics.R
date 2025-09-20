
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
  filter(CH_FLAG == 1) %>% 
  select(NHS_NO) %>% 
  distinct()

# CHECK: Records across entire time period associated with ch_pats: 441m (diff to CH_FLAG == 1 record count)
base %>% 
  select(PF_ID, NHS_NO) %>% 
  inner_join(ch_pats) %>% 
  tally()

# Base data for chart
df = base %>% 
  #filter(PCD_LAD_NAME == "Newcastle upon Tyne") %>% 
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
  filter(CH_FLAG == 1)

# Plotting data
df_plot = df %>% 
  mutate(TEN_PLUS = ifelse(UNIQUE_MEDICINES >= 10, 1, 0)) %>% 
  group_by(SEQ) %>% 
  summarise(
    ITEMS = sum(ITEMS),
    MEAN_UNIQUE_MEDS = mean(UNIQUE_MEDICINES),
    TEN_PLUS_SUM = sum(TEN_PLUS),
    TOTAL_PM = n()
    ) %>% 
  ungroup() %>% 
  collect() %>% 
  mutate(PROP = 100 * TEN_PLUS_SUM / TOTAL_PM) %>% 
  arrange(SEQ)



# Line charts ------------------------------------------------------------------

# Chart one: mean unique meds per month (marginal increase)
df_plot %>% 
  filter(
    SEQ <= 13,
    SEQ >= 2
  ) %>% 
  hchart("line", hcaes(SEQ, MEAN_UNIQUE_MEDS)) %>% 
  hc_yAxis(min = 5, max = 8)

# Chart two: prop 10+ unique meds monthly (decrease then plateau)
df_plot %>% 
  filter(
    SEQ <= 13,
    SEQ >= 2
  ) %>% 
  hchart("line", hcaes(SEQ, PROP)) %>% 
  hc_yAxis(min = 18, max = 24)

# Chart three: mean items PPM
df_plot %>% 
  arrange(SEQ) %>% 
  mutate(ITEMS_PPM = ITEMS / TOTAL_PM) %>% 
  filter(
    SEQ <= 25,
    SEQ >= 2
  ) %>% 
  hchart("line", hcaes(SEQ, ITEMS_PPM)) %>% 
  hc_yAxis(min = 9, max = 12)

# Bar charts -------------------------------------------------------------------

# Function to generate seq groups
create_seq_groups = function(df){
  df %>% 
    mutate(
      SEQ_GROUP = case_when(
        SEQ >= 2 & SEQ <= 4 ~ "1-3 Months",
        SEQ >= 5 & SEQ <= 7 ~ "4-6 Months",
        SEQ >= 8 & SEQ <= 10 ~ "7-9 Months",
        SEQ >= 11 & SEQ <= 13 ~ "10-12 Months",
        SEQ >= 14 ~ "13+ Months"
      ),
      SEQ_GROUP = factor(
        SEQ_GROUP,
        levels = c(
          "1-3 Months", 
          "4-6 Months", 
          "7-9 Months", 
          "10-12 Months", 
          "13+ Months"
          ))
    )
}

# Chart four: mean items PPM (grouped)
df_plot %>% 
  create_seq_groups() %>% 
  filter(!is.na(SEQ_GROUP)) %>% 
  group_by(SEQ_GROUP) %>% 
  summarise(
    ITEMS = sum(ITEMS),
    TOTAL_PM = sum(TOTAL_PM),
    MEAN_ITEMS_PPM = ITEMS / TOTAL_PM
  ) %>% 
  hchart("column", hcaes(SEQ_GROUP, MEAN_ITEMS_PPM))

# Chart five: mean unique items PPM (grouped)
df_plot %>% 
  create_seq_groups() %>% 
  filter(!is.na(SEQ_GROUP)) %>% 
  group_by(SEQ_GROUP) %>% 
  summarise(
    TEN_PLUS_SUM = sum(TEN_PLUS_SUM),
    TOTAL_PM = sum(TOTAL_PM),
    PROP = 100 * TEN_PLUS_SUM / TOTAL_PM
  ) %>% 
  hchart("column", hcaes(SEQ_GROUP, PROP))

# Disconnect aned clean
DBI::dbDisconnect(con); rm(list = ls()); gc()

# ------------------------------------------------------------------------------
# CALC STEP ONE:l DBPLYR OPTION
# group_by(NHS_NO) %>%                        
#   arrange(YEAR_MONTH) %>% 
#   mutate(FLIP = ifelse(
#     row_number() == 1, 
#     0,
#     ifelse(CH_FLAG != lag(CH_FLAG), 1, 0))
#   ) %>% 
#   ungroup()