
# Library
library(dplyr)
library(dbplyr)

# Connect to dalp
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  dplyr::tbl(from = in_schema("ADNSH", "INT646_CH_BASE_20200401_20230331"))

# All level permutations
all_levels = cross_join(
  data.frame(BNF = c(
    "CHAPTER_DESCR",
    "SECTION_DESCR", 
    "PARAGRAPH_DESCR", 
    "CHEMICAL_SUBSTANCE_BNF_DESCR"
    )),
  data.frame(GEO = c(
    "PCD_REGION_NAME", 
    "PCD_LAD_NAME", 
    "PCD_ICB_NAME"
  ))
)

# Part One: items and nic prop -------------------------------------------------

# Function to generate data
get_geo_bnf_prop = function(index){
  
  # Get vars names
  bnf = rlang::sym(all_levels[index,1])
  geo = rlang::sym(all_levels[index,2])
  
  # Limit to top 50 bnf_child across all years & geographies, for *each metric*
  join_fact = fact_db %>% 
    rename(BNF_CHILD := {{ bnf }}) %>% 
    group_by(BNF_CHILD) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    ungroup() %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 50
    ) %>% 
    select(BNF_CHILD) %>% 
    collect()
  
  # Total metric generation plus join
  df = fact_db %>% 
    rename(
      GEOGRAPHY_CHILD := {{ geo }},
      BNF_CHILD := {{ bnf }}
    ) %>% 
    filter(!is.na(GEOGRAPHY_CHILD)) %>% 
    group_by(
      FY,
      GEOGRAPHY_CHILD,
      BNF_CHILD
    ) %>%
    summarise(
      ITEMS = sum(ITEM_COUNT),
      NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>%
    mutate(
      TOTAL_ITEMS = sum(ITEMS),
      TOTAL_NIC = sum(NIC),
      PROP_ITEMS = round((ITEMS / TOTAL_ITEMS) * 100, 2),
      PROP_NIC = round((NIC / TOTAL_NIC) * 100, 2)
    ) %>%
    ungroup() %>%
    collect() %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = rlang::as_string(geo),
      GEOGRAPHY_CHILD,
      BNF_PARENT = rlang::as_string(bnf),
      BNF_CHILD,
      PROP_ITEMS,
      PROP_NIC
    ) %>% 
    tidyr::pivot_longer(
      c('PROP_ITEMS', 'PROP_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
    ) %>% 
    inner_join(
      join_fact,
      by = "BNF_CHILD"
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

# Get prop results
prop_results = lapply(1:nrow(all_levels), get_geo_bnf_prop) %>% bind_rows()

# Part two: items ppm and nic ppm ----------------------------------------------

# Function to generate data
get_geo_bnf_ppm = function(index){
  
  # Get vars names
  bnf = rlang::sym(all_levels[index,1])
  geo = rlang::sym(all_levels[index,2])
  
  # Limit to top20 bnf_child across all years & geographies, for *each metric*
  join_fact = fact_db %>% 
    rename(BNF_CHILD := {{ bnf }}) %>% 
    group_by(BNF_CHILD) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 50
    ) %>% 
    select(BNF_CHILD) %>% 
    collect()
  
  # Distinct months of presc per nhs_no and FY
  pat_months = fact_db %>% 
    group_by(FY, NHS_NO) %>% 
    summarise(N_MONTHS = n_distinct(YEAR_MONTH)) %>% 
    ungroup()
  
  # Generate output
  df = fact_db %>% 
    rename(
      GEOGRAPHY_CHILD := {{ geo }},
      BNF_CHILD := {{ bnf }}
    ) %>% 
    filter(!is.na(GEOGRAPHY_CHILD)) %>% 
    group_by(FY, NHS_NO, GEOGRAPHY_CHILD, BNF_CHILD) %>% 
    summarise(
      ITEMS = sum(ITEM_COUNT),
      NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>%
    ungroup() %>% 
    left_join(
      pat_months,
      by = c("FY", "NHS_NO")
    ) %>% 
    mutate(
      PPM_ITEMS = round(ITEMS / N_MONTHS, 2),
      PPM_NIC = round(NIC / N_MONTHS, 2)
    ) %>% 
    group_by(FY, GEOGRAPHY_CHILD, BNF_CHILD) %>% 
    summarise(
      PPM_ITEMS = round(mean(PPM_ITEMS), 2),
      PPM_NIC = round(mean(PPM_NIC), 2)
    ) %>% 
    ungroup() %>% 
    collect() %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = rlang::as_string(geo),
      GEOGRAPHY_CHILD,
      BNF_PARENT = rlang::as_string(bnf),
      BNF_CHILD,
      PPM_ITEMS,
      PPM_NIC
    ) %>% 
    tidyr::pivot_longer(
      c('PPM_ITEMS', 'PPM_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
    ) %>% 
    inner_join(
      join_fact,
      by = "BNF_CHILD"
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

# Get ppm results
ppm_results = lapply(1:nrow(all_levels), get_geo_bnf_ppm) %>% bind_rows()

# Part Three: bind results and save --------------------------------------------

# Bind both both outputs
mod_geo_ch_flag_drug_df = rbind(prop_results, ppm_results) %>% 
  mutate(
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
