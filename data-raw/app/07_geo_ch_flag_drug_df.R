
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

# Geography columns
geo_cols = c(
  "PCD_REGION_NAME", 
  "PCD_LAD_NAME", 
  "PCD_ICB_NAME"
)

# All level permutations
all_levels = cross_join(
  data.frame(BNF = bnf_cols),
  data.frame(GEO = geo_cols)
)

# Part One: items and nic prop -------------------------------------------------

# Function to generate data
get_geo_bnf_prop = function(index){
  
  # Get vars names
  bnf = rlang::sym(all_levels[index,1])
  geo = rlang::sym(all_levels[index,2])
  
  # Limit to top 50 bnf_child across all years & geographies, for *each metric*
  join_fact = fact_db %>% 
    filter(CH_FLAG == 1) %>% 
    group_by({{ bnf }}) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    ungroup() %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 50
    ) %>% 
    select({{ bnf }}) %>% 
    nhsbsaR::collect_with_parallelism(., 16)
  
  # Total metric generation plus join
  df = fact_db %>% 
    filter(
      CH_FLAG == 1,
      !is.na({{ geo }})
      ) %>% 
    group_by(FY, {{ geo }}, {{ bnf }}) %>%
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
      GEOGRAPHY_PARENT = rlang::as_string(geo),
      GEOGRAPHY_CHILD := {{ geo }},
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
    GEOGRAPHY_PARENT = unique(df$GEOGRAPHY_PARENT),
    GEOGRAPHY_CHILD = unique(df$GEOGRAPHY_CHILD),
    BNF_PARENT = unique(df$BNF_PARENT),
    BNF_CHILD = unique(df$BNF_CHILD),
    METRIC = unique(df$METRIC)
    ) %>% 
    left_join(df) %>% 
    mutate(VALUE = ifelse(is.na(VALUE), 0, VALUE))
}

# Get prop results: < 5 mins
prop_results = lapply(1:nrow(all_levels), get_geo_bnf_prop) %>% bind_rows()

# Part two: items ppm and nic ppm ----------------------------------------------

# Function to generate data
get_geo_bnf_ppm = function(index){
  
  # Get vars names
  bnf = rlang::sym(all_levels[index,1])
  geo = rlang::sym(all_levels[index,2])
  
  # Limit to top20 bnf_child across all years & geographies, for *each metric*
  join_fact = fact_db %>% 
    filter(CH_FLAG == 1) %>% 
    group_by({{ bnf }}) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 50
    ) %>% 
    ungroup() %>% 
    select({{ bnf }})
  
  # Distinct months of presc per nhs_no and FY
  pat_months = fact_db %>% 
    filter(
      CH_FLAG == 1,
      !is.na({{ geo }})
    ) %>% 
    group_by(FY, YEAR_MONTH, NHS_NO, {{ geo }}) %>% 
    summarise() %>% 
    ungroup() %>% 
    cross_join(join_fact)
  
  # Generate output
  pat_info = fact_db %>% 
    # There are 17 records without a postcode and no geographic information
    filter(
      CH_FLAG == 1,
      !is.na(IMD_DECILE)
      ) %>% 
    group_by(FY, YEAR_MONTH, NHS_NO, {{ geo }}, {{ bnf }}) %>% 
    summarise(
      ITEMS = sum(ITEM_COUNT),
      NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>%
    ungroup()
  
  # Output
  pat_months %>% 
    left_join(pat_info) %>%
    # Data now annual patient-month level
    mutate(
      ITEMS = ifelse(is.na(ITEMS), 0, ITEMS),
      NIC = ifelse(is.na(NIC), 0, NIC)
    ) %>% 
    # Annual-level aggregation
    group_by(FY, {{ geo }}, {{ bnf }}) %>% 
    summarise(
      PPM_ITEMS = mean(ITEMS),
      PPM_NIC = mean(NIC)
    ) %>% 
    ungroup() %>% 
    nhsbsaR::collect_with_parallelism(., 16) %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = rlang::as_string(geo),
      GEOGRAPHY_CHILD := {{ geo }},
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
ppm_results = lapply(1:nrow(all_levels), get_geo_bnf_ppm) %>% bind_rows()

# Part Three: bind results and save --------------------------------------------

# Bind both both outputs
mod_geo_ch_flag_drug_df = rbind(prop_results, ppm_results) %>% 
  mutate(
    GEOGRAPHY_CHILD = case_when(
      GEOGRAPHY_PARENT == "PCD_ICB_NAME" ~ gsub("\\bNHS\\b ", "", GEOGRAPHY_CHILD),
      TRUE ~ GEOGRAPHY_CHILD
    ),
    GEOGRAPHY_CHILD = case_when(
      GEOGRAPHY_PARENT == "PCD_ICB_NAME" ~ gsub("\\bICB\\b", "", GEOGRAPHY_CHILD),
      TRUE ~ GEOGRAPHY_CHILD
    ),
    GEOGRAPHY_PARENT = case_when(
      GEOGRAPHY_PARENT == "PCD_REGION_NAME" ~ "Region",
      GEOGRAPHY_PARENT == "PCD_ICB_NAME" ~ "ICB",
      GEOGRAPHY_PARENT == "PCD_LAD_NAME" ~ "Local Authority"
    ),
    BNF_PARENT = case_when(
      BNF_PARENT == "CHAPTER_DESCR" ~ "Chapter",
      BNF_PARENT == "PARAGRAPH_DESCR" ~ "Paragraph",
      BNF_PARENT == "SECTION_DESCR" ~ "Section",
      BNF_PARENT == "CHEMICAL_SUBSTANCE_BNF_DESCR" ~ "Chemical Substance"
    ),
    METRIC = case_when(
      METRIC == "PPM_ITEMS" ~ "Number of Prescription Items (PPM)",
      METRIC == "PPM_NIC" ~ "Drug Cost (PPM)",
      METRIC == "PROP_ITEMS" ~ "% of Total Annual Number of Prescription Items",
      METRIC == "PROP_NIC" ~ "% of Total Annual Drug Cost"
    )
  )

# Check Output
colSums(is.na(mod_geo_ch_flag_drug_df))

# Check categories
mod_geo_ch_flag_drug_df %>% count(GEOGRAPHY_PARENT, BNF_PARENT)

# Use this
usethis::use_data(mod_geo_ch_flag_drug_df, overwrite = TRUE)

# Disconnect
DBI::dbDisconnect(con); rm(list = ls()); gc()

#-------------------------------------------------------------------------------