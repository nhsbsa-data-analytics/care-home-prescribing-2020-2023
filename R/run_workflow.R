
# Load/install all required packages and functions
source("R/analysis_packages.R")
source("R/workflow_helpers.R")
source("R/workflow_production.R")

# 1. Get latest cqc data
get_latest_cqc_data()

# 2. Get a select ab plus epoch supplemented with cqc-postcode uprn
get_ab_plus_supplemented_with_cqc(
  cqc_data = "INT646_CQC_20230217", 
  start_date = "2021-04-01",
  end_date =   "2022-03-31"
  )

# 3. Merge and process cqc and ab plus
create_ab_plus_cqc_data(
  ab_plus_data = "INT646_ABP_20220422",
  cqc_data = "INT646_CQC_20230217"
  )

# 4. Create form level fact for records with a ch-postcode
create_form_level_patient_addresses(
  address_data = "INT646_ABP_CQC_20210401_20220331"
  )

# 5. Match patient details against ch-postcode uprn and process
create_care_home_address_match(
  patient_address_data = "INT646_PFID_20210401_20220331",
  lookup_address_data = "INT646_ABP_CQC_20210401_20220331"
)

# 6. Join to fact table and get non ch-postcode records within time frame
create_matched_prescription_base_table(
  match_table = "INT646_UPRN_MATCH_202203"
)