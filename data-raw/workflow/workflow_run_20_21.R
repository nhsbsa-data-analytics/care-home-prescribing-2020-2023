# Load/install all required packages and functions
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("data-raw/workflow/workflow_production.R")

# Specify variables to retain at end of each script
keep_vars = c(ls(), 'keep_vars')

# FY 20/21 ---------------------------------------------------------------------
# Set these manually ...
start_str <- "20200401"
end_str <- "20210331"
abp_data <- "INT646_ABP_20210324"
cqc_data <- "INT646_CQC_20230602"

# ... then these are based on manually set variables
start_date <- glue(
  "{substr(start_str, 1, 4)}-{substr(start_str, 5, 6)}-{substr(start_str, 7, 8)}"
)
end_date <- paste(
  "{substr(end_str, 1, 4)}-{substr(end_str, 5, 6)}-{substr(end_str, 7, 8)}"
)
address_data <- glue("INT646_ABP_CQC_{start_str}_{end_str}")
patient_data = glue("INT646_FORMS_{start_str}_{end_str}")
match_data = glue("INT646_MATCH_{start_str}_{end_str}")

# 1. Get latest cqc data: ~0.5hr
get_latest_cqc_data()

# 2. Get a select ab plus epoch: ~130 mins
get_abp_from_dall_ref(
  end_date = end_date
)

# 3. Merge and process cqc and ab plus: ~3 mins
create_ab_plus_cqc_data(
  ab_plus_data = abp_data,
  cqc_data = cqc_data,
  start_date = start_date,
  end_date =   end_date
)

# 4. Create form level fact for records with a ch-postcode: ~11-14hr
create_form_level_patient_addresses(
  address_data = address_data
)

# 5. Match patient details against ch-postcode uprn and process: ~30-40 mins
create_care_home_address_match(
  patient_address_data = patient_data,
  lookup_address_data = address_data,
  parent_uprn_data = abp_data
)

# 6. Create postcode lookup table (latest available mappings) for joining in the next step: ~5 min
create_postcode_lookup()

# 7. Join to fact table and get non ch-postcode records within time frame: ~9 hrs
create_matched_prescription_base_table(
  match_data = match_data,
  form_data = patient_data
)
