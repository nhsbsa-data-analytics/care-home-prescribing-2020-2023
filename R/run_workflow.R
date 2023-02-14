
# Load all packages and functions
# Load packages and global variables
source("R/analysis_packages.R")
source("R/workflow_helpers.R")
source("R/workflow_production.R")

# 1. Get latest cqc data
get_latest_cqc_data()

# 2. Get a select ab plus epoch supplemented with cqc-postcode uprn
get_ab_plus_supplemented_with_cqc(
  cqc_data = "INT646_CQC_202302", 
  start_date = "2021-04-01",
  end_date =   "2022-06-01"
  )

# 3. Merge and process cqc and ab plus
stack_and_process_ab_plus_cqc_data(
  ab_plus_data = "INT646_AB_PLUS_202206",
  cqc_data = "INT646_CQC_202302", 
  start_date = "2021-04-01",
  end_date =   "2022-06-01"
)
