
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("data-raw/workflow/workflow_production.R")

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

patient_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# 38
no_address <- patient_db %>% 
  filter(
    FY == "2022/23", 
    EPS_FLAG == "N", 
    BSA_SLA == "NA NA" | is.null(BSA_SLA) | is.na(BSA_SLA)
  ) %>% 
  summarise(num = n_distinct(PF_ID)) %>% 
  collect_with_parallelism(32)

# 17038872
num_forms <- patient_db %>% 
  filter(
    FY == "2022/23", 
    EPS_FLAG == "N"
  ) %>% 
  summarise(num = n_distinct(PF_ID)) %>% 
  collect_with_parallelism(32)

# 0.0002%
pct_no_address <- 100 * pull(no_address) / pull(num_forms)