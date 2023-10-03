

# Library
library(dplyr)
library(dbplyr)

# Connect to dalp
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  dplyr::tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# BNF columns
bnf_cols = c(
  "CHAPTER_DESCR",
  "SECTION_DESCR", 
  "PARAGRAPH_DESCR", 
  "CHEMICAL_SUBSTANCE_BNF_DESCR"
)

# Part One: items and nic prop -------------------------------------------------

# Function to generate data
get_geo_bnf_prop = function(index){
  
  # Get vars names
  bnf = rlang::sym(bnf_cols[index])
  
  # Limit to care homes
  ch_db = fact_db %>% 
    filter(CH_FLAG == 1)
  
  # Filter when section name equals paragraph name
  if(bnf_cols[index] == "PARAGRAPH_DESCR"){
    ch_db = ch_db %>% filter(PARAGRAPH_DESCR != SECTION_DESCR)
  }
  
  # Filter when chem_sub name equals paragraph name
  if(bnf_cols[index] == "CHEMICAL_SUBSTANCE_BNF_DESCR"){
    ch_db = ch_db %>% filter(CHEMICAL_SUBSTANCE_BNF_DESCR != PARAGRAPH_DESCR)
  }
  
  # Limit to top 20 bnf_child ch items across all years, for *each metric*
  join_fact = ch_db %>% 
    group_by({{ bnf }}) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    ungroup() %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 20
    ) %>% 
    select({{ bnf }}) %>% 
    nhsbsaR::collect_with_parallelism(., 16)
  
  # Total metric generation plus join
  df = fact_db %>% 
    group_by(FY, CH_FLAG, {{ bnf }}) %>%
    summarise(
      ITEMS = sum(ITEM_COUNT),
      NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>%
    mutate(
      TOTAL_ITEMS = sum(ITEMS),
      TOTAL_NIC = sum(NIC),
      PROP_ITEMS = (ITEMS / TOTAL_ITEMS) * 100,
      PROP_NIC = (NIC / TOTAL_NIC) * 100
    ) %>%
    ungroup() %>%
    nhsbsaR::collect_with_parallelism(., 16) %>% 
    inner_join(join_fact) %>% 
    transmute(
      FY,
      CH_FLAG,
      BNF_PARENT = rlang::as_string(bnf),
      BNF_CHILD := {{ bnf }},
      PROP_ITEMS = janitor::round_half_up(PROP_ITEMS, 3),
      PROP_NIC = janitor::round_half_up(PROP_NIC, 3)
    ) %>%
    tidyr::pivot_longer(
      c('PROP_ITEMS', 'PROP_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
    )
  
  # Expand grid an left_join for all permutations
  expand.grid(
    FY = unique(df$FY),
    CH_FLAG = unique(df$CH_FLAG),
    BNF_PARENT = unique(df$BNF_PARENT),
    BNF_CHILD = unique(df$BNF_CHILD),
    METRIC = unique(df$METRIC)
  ) %>%
    left_join(df) %>%
    mutate(VALUE = ifelse(is.na(VALUE), 0, VALUE))
}

# Get prop results: ~1 minute
prop_results = lapply(1:length(bnf_cols), get_geo_bnf_prop) %>% bind_rows()

# Part two: items ppm and nic ppm ----------------------------------------------

# Function to generate data
get_geo_bnf_ppm = function(index){
  
  # Get vars names
  bnf = rlang::sym(bnf_cols[index])
  
  # Limit to care homes
  ch_db = fact_db %>% 
    filter(CH_FLAG == 1)
  
  # Filter when section name equals paragraph name
  if(bnf_cols[index] == "PARAGRAPH_DESCR"){
    ch_db = ch_db %>% filter(PARAGRAPH_DESCR != SECTION_DESCR)
  }
  
  # Filter when chem_sub name equals paragraph name
  if(bnf_cols[index] == "CHEMICAL_SUBSTANCE_BNF_DESCR"){
    ch_db = ch_db %>% filter(CHEMICAL_SUBSTANCE_BNF_DESCR != PARAGRAPH_DESCR)
  }
  
  # Limit to top 20 bnf_child across all years & geographies, for *each metric*
  join_fact = ch_db %>%
    group_by({{ bnf }}) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>%
    slice_max(
      TOTAL_ITEMS,
      n = 20
    ) %>%
    ungroup() %>%
    select({{ bnf }})
  
  # Distinct months of presc per nhs_no and FY
  pat_months = fact_db %>%
    group_by(FY, CH_FLAG, YEAR_MONTH, NHS_NO) %>% 
    summarise() %>% 
    ungroup() %>% 
    group_by(FY, CH_FLAG) %>% 
    tally(n = "PAT_MONTHS") %>% 
    ungroup() %>% 
    nhsbsaR::collect_with_parallelism(., 16) 
  
  # Generate output
  pat_info = fact_db %>% 
    inner_join(join_fact) %>% 
    # There are 17 records without a postcode and no geographic information
    group_by(FY, CH_FLAG, {{ bnf }}) %>% 
    summarise(
      ITEMS = sum(ITEM_COUNT),
      NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>%
    ungroup() %>% 
    nhsbsaR::collect_with_parallelism(., 16)
  
  # Output
  pat_months %>% 
    left_join(pat_info) %>%
    # Data now annual patient-month level
    mutate(
      PPM_ITEMS = ITEMS / PAT_MONTHS,
      PPM_NIC = NIC / PAT_MONTHS
    ) %>% 
    transmute(
      FY,
      CH_FLAG,
      BNF_PARENT = rlang::as_string(bnf),
      BNF_CHILD := {{ bnf }},
      PPM_ITEMS = janitor::round_half_up(PPM_ITEMS, 3),
      PPM_NIC = janitor::round_half_up(PPM_NIC, 3)
    ) %>% 
    tidyr::pivot_longer(
      c('PPM_ITEMS', 'PPM_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
    )
}

# Get ppm results: ~40 mins
ppm_results = lapply(1:length(bnf_cols), get_geo_bnf_ppm) %>% bind_rows()

# Part Three: bind results and save --------------------------------------------

# Bind both both outputs
mod_ch_flag_drug_df = rbind(prop_results, ppm_results) %>% 
  mutate(
    BNF_PARENT = case_when(
      BNF_PARENT == "CHAPTER_DESCR" ~ "Chapter",
      BNF_PARENT == "PARAGRAPH_DESCR" ~ "Paragraph",
      BNF_PARENT == "SECTION_DESCR" ~ "Section",
      BNF_PARENT == "CHEMICAL_SUBSTANCE_BNF_DESCR" ~ "Chemical substance"
    ),
    METRIC = case_when(
      METRIC == "PPM_ITEMS" ~ "Mean prescription items PPM",
      METRIC == "PPM_NIC" ~ "Mean drug cost PPM",
      METRIC == "PROP_ITEMS" ~ "% of total annual number of prescription items",
      METRIC == "PROP_NIC" ~ "% of total annual drug cost"
    )
  )

# Check Output
colSums(is.na(mod_ch_flag_drug_df))

# Check categories
mod_ch_flag_drug_df %>% count(FY, CH_FLAG, BNF_PARENT)

# Use this
usethis::use_data(mod_ch_flag_drug_df, overwrite = TRUE)

# Disconnect
DBI::dbDisconnect(con); rm(list = ls()); gc()

#-------------------------------------------------------------------------------
