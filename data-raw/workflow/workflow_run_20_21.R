# Load/install all required packages and functions
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("data-raw/workflow/workflow_production.R")

# Specify variables to retain at end of each script
keep_vars = c(ls(), 'keep_vars')

# FY 20/21 ---------------------------------------------------------------------

# 1. Get latest cqc data: 0.5hr
get_latest_cqc_data()

# 2. Get a select ab plus epoch: 1 hr
get_abp_from_os(
  epoch_year = "2021"
)

# 3. Merge and process cqc and ab plus: 1 min
create_ab_plus_cqc_data(
  ab_plus_data = "INT646_ABP_yyyymmdd",
  cqc_data = "INT646_CQC_20230526",
  start_date = "2020-04-01",
  end_date =   "2021-03-31"
)

# 4. Create form level fact for records with a ch-postcode: 8hr
create_form_level_patient_addresses(
  address_data = "INT646_ABP_CQC_2020401_20210331"
)

# 5. Match patient details against ch-postcode uprn and process: 0.5hr
create_care_home_address_match(
  patient_address_data = "INT646_FORMS_20200401_20210331",
  lookup_address_data = "INT646_ABP_CQC_2020401_20210331",
  parent_uprn_data = "INT646_ABP_yyyymmdd"
)

# 6. Create postcode lookup table (latest available mappings) for joining in the next step: 2min
create_postcode_lookup()

# 7. Join to fact table and get non ch-postcode records within time frame: 11hr
create_matched_prescription_base_table(
  match_data = "INT646_MATCH_20200401_20210331",
  form_data = "INT646_FORMS_20200401_20210331"
)
