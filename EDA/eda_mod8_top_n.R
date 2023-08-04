
# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Libraries
library(dplyr)
library(dbplyr)

# Base table
fact_db <- con %>%
  dplyr::tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# Get single total item count per year
total = fact_db %>% 
  filter(
    FY == "2022/23",
    CH_FLAG == 1
    ) %>% 
  group_by() %>% 
  summarise(TOTAL = sum(ITEM_COUNT)) %>%
  ungroup() %>% 
  nhsbsaR::collect_with_parallelism(., 16) %>% 
  pull()

# Function to generate results
item_prop_by_top_bnf = function(bnf_level, n){
  
  fact_db %>% 
    filter(
      FY == "2022/23",
      CH_FLAG == 1
    ) %>% 
    group_by_at({{ bnf_level }}) %>% 
    summarise(ITEM_COUNT = sum(ITEM_COUNT)) %>% 
    slice_max(order_by = ITEM_COUNT, n = n) %>% 
    ungroup() %>% 
    group_by() %>% 
    summarise(ITEM_COUNT = sum(ITEM_COUNT)) %>% 
    ungroup() %>% 
    nhsbsaR::collect_with_parallelism(., 16) %>% 
    mutate(
      TOTAL = total,
      TOP_N = n,
      COVERAGE = round(100 * (ITEM_COUNT / total), 1),
      TYPE := {{ bnf_level }},
      
    )
}

# Get results: ~30 mins
results = rbind(
  item_prop_by_top_bnf('SECTION_DESCR', 50),
  item_prop_by_top_bnf('SECTION_DESCR', 75),
  item_prop_by_top_bnf('SECTION_DESCR', 100),
  item_prop_by_top_bnf('PARAGRAPH_DESCR', 50),
  item_prop_by_top_bnf('PARAGRAPH_DESCR', 75),
  item_prop_by_top_bnf('PARAGRAPH_DESCR', 100),
  item_prop_by_top_bnf("CHEMICAL_SUBSTANCE_BNF_DESCR", 50),
  item_prop_by_top_bnf("CHEMICAL_SUBSTANCE_BNF_DESCR", 75),
  item_prop_by_top_bnf("CHEMICAL_SUBSTANCE_BNF_DESCR", 100),
  item_prop_by_top_bnf("CHEMICAL_SUBSTANCE_BNF_DESCR", 125),
  item_prop_by_top_bnf("CHEMICAL_SUBSTANCE_BNF_DESCR", 150)
  )

# Inspect results
View(results)

# Disconnect
DBI::dbDisconnect(con); rm(list = ls()); gc()
