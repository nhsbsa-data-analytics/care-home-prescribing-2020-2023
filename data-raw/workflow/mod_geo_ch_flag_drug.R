
# Library
library(dplyr)
library(dbplyr)

# Connect to dalp
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  dplyr::tbl(from = in_schema("ADNSH", "INT646_CH_BASE_20200401_20230331"))

# List of bnf levels
bnf_levels = c(
  "CHAPTER_DESCR", 
  "SECTION_DESCR", 
  "PARAGRAPH_DESCR", 
  "CHEMICAL_SUBSTANCE_BNF_DESCR"
  )

# List of geo levels
geo_levels = c(
  "PCD_REGION_NAME", 
  "PCD_LAD_NAME", 
  "PCD_ICB_NAME", 
  "PRESCRIBER_PCN"
  )

# All level permutations
all_levels = cross_join(
  data.frame(BNF = bnf_levels),
  data.frame(GEO = geo_levels)
)

# Rough Final df record count
# Region = 3 years * 7 geo * 4 metrics * 20 top bnf items * 4 bnf levels = 6,720 records
# ICB = 3 years * 42 geo * 4 metrics * 20 top bnf items * 4 bnf levels = _ 40,320 records
# LAD = 3 years * 308 geo * 4 metrics * 20 top bnf items * 4 bnf levels = 295,680 records
# PCN = 3 years * 1,258 geo * 4 metrics * 20 top bnf items * 4 bnf levels = 1,207,680 records

# Total = 1615 geo * 3 years * 4 metrics * 4 bnf levels * 20 bnf items = 1,550,400

# Part One: items and nic prop -------------------------------------------------

# Function to generate data
get_geo_bnf_prop = function(bnf, geo){
  
  # Limit to top20 bnf_child across all years & geographies, for *each metric*
  join_fact = fact_db %>% 
    rename(BNF_CHILD := {{ bnf }}) %>% 
    group_by(BNF_CHILD) %>%
    summarise(
      PROP_ITEMS = sum(ITEM_COUNT),
      PROP_NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>% 
    ungroup() %>% 
    tidyr::pivot_longer(
      c('PROP_ITEMS', 'PROP_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
    ) %>% 
    group_by(METRIC) %>% 
    slice_max(
      VALUE,
      n = 20
    ) %>% 
    ungroup() %>% 
    select(BNF_CHILD, METRIC) %>% 
    collect()
  
  # Total metric generation plus join
  fact_db %>% 
    rename(
      GEOGRAPHY_CHILD := {{ geo }},
      BNF_CHILD := {{ bnf }}
    ) %>% 
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
      PROP_NIC = round((NIC / TOTAL_NIC) * 100, 2),
    ) %>%
    ungroup() %>%
    collect() %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = geo,
      GEOGRAPHY_CHILD,
      BNF_PARENT = bnf,
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
      by = c("BNF_CHILD", "METRIC")
    )
}

# List for loop output
results = list()

# Loop through all_levels df info
for(i in 1:nrow(all_levels)){
  results[[i]] = get_geo_bnf_prop(all_levels[i,1], all_levels[i,2])
}

# Bind Results
prop_results = results %>% bind_rows()

# Part two: items ppm and nic ppm ----------------------------------------------

# Function to generate data
get_geo_bnf_ppm = function(bnf, geo){
  
  # Limit to top20 bnf_child across all years & geographies, for *each metric*
  join_fact = fact_db %>% 
    rename(BNF_CHILD := {{ bnf }}) %>% 
    group_by(BNF_CHILD) %>%
    summarise(
      PPM_ITEMS = sum(ITEM_COUNT),
      PPM_NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>% 
    ungroup() %>% 
    tidyr::pivot_longer(
      c('PPM_ITEMS', 'PPM_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
    ) %>% 
    group_by(METRIC) %>% 
    slice_max(
      VALUE,
      n = 20
    ) %>% 
    ungroup() %>% 
    select(BNF_CHILD, METRIC) %>% 
    collect()
  
  # Distinct months of presc per nhs_no and FY
  pat_months = fact_db %>% 
    group_by(FY, NHS_NO) %>% 
    summarise(N_MONTHS = n_distinct(YEAR_MONTH)) %>% 
    ungroup()
  
  # Generate output
  fact_db %>% 
    rename(
      GEOGRAPHY_CHILD := {{ geo }},
      BNF_CHILD := {{ bnf }}
    ) %>% 
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
      PPM_ITEMS = ITEMS / N_MONTHS,
      PPM_NIC = NIC / N_MONTHS
    ) %>% 
    group_by(FY, GEOGRAPHY_CHILD, BNF_CHILD) %>% 
    summarise(
      PPM_ITEMS = mean(PPM_ITEMS),
      PPM_NIC = mean(PPM_NIC)
    ) %>% 
    ungroup() %>% 
    collect() %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = geo,
      GEOGRAPHY_CHILD,
      BNF_PARENT = bnf,
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
      by = c("BNF_CHILD", "METRIC")
    )
}

# List for loop output
results = list()

# Loop through all_levels df info
for(i in 1:nrow(all_levels)){
  results[[i]] = get_geo_bnf_ppm(all_levels[i,1], all_levels[i,2])
}

# Bind Results
ppm_results = results %>% bind_rows()

# Part Three: bind results and save --------------------------------------------

# Bind both both outputs
mod_geo_ch_flag_drug_df = rbind(prop_results, ppm_results) %>% 
  filter(!is.na(GEOGRAPHY_CHILD))

# Check Output
colSums(is.na(mod_geo_ch_flag_drug_df))

# Check categories
mod_geo_ch_flag_drug_df %>% count(GEOGRAPHY_PARENT, BNF_PARENT)

# Use this
usethis::use_data(mod_geo_ch_flag_drug_df, overwrite = TRUE)

# Disconnect
DBI::dbDisconnect(con); rm(list = ls()); gc()
