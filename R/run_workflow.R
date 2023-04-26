
# Load/install all required packages and functions
source("EDA/mmc/analysis_packages.R")
source("EDA/mmc/workflow_helpers.R")
source("EDA/mmc/workflow_production.R")

# Specify variables to retain at end of each script
keep_vars = c(ls(), 'keep_vars')

# FY 21/22 ---------------------------------------------------------------------

# 1. Get latest cqc data: 0.5hr
get_latest_cqc_data()

# 2. Get a select ab plus epoch supplemented with cqc-postcode uprn: 0.5hr
get_ab_plus_supplemented_with_cqc(
  end_date =   "2022-03-31"
)

# 3. Merge and process cqc and ab plus: 1 min
create_ab_plus_cqc_data(
  ab_plus_data = "INT646_ABP_20220422",
  cqc_data = "INT646_CQC_20230305",
  start_date = "2021-04-01",
  end_date =   "2022-03-31"
)

# 4. Create form level fact for records with a ch-postcode: 8hr
create_form_level_patient_addresses(
  address_data = "INT646_ABP_CQC_20210401_20220331"
)

# 5. Match patient details against ch-postcode uprn and process: 0.5hr
create_care_home_address_match(
  patient_address_data = "INT646_FORMS_20210401_20220331",
  lookup_address_data = "INT646_ABP_CQC_20210401_20220331",
  parent_uprn_data = "INT646_ABP_20220422"
)

# 6. Join to fact table and get non ch-postcode records within time frame: 11hr
create_matched_prescription_base_table(
  match_data = "INT646_MATCH_20210401_20220331",
  form_data = "INT646_FORMS_20210401_20220331"
)
