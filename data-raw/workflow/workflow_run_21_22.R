# Load/install all required packages and functions
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("data-raw/workflow/workflow_production.R")
b <- Sys.time()
# Specify variables to retain at end of each script
keep_vars = c(ls(), 'keep_vars')

# FY 21/22 ---------------------------------------------------------------------

# 1. Get latest cqc data: 0.5hr - Run once in first epoch script
# get_latest_cqc_data()

# 2. Get a select ab plus epoch: ~130 mins
# get_abp_from_os(
#   epoch_year = "2022"
# )

b <- Sys.time()
print(paste0(b, ": Starting step 3..."))
# 3. Merge and process cqc and ab plus: ~2 mins
create_ab_plus_cqc_data(
  ab_plus_data = "INT646_ABP_20220324",
  cqc_data = "INT646_CQC_20230602",
  start_date = "2021-04-01",
  end_date =   "2022-03-31"
)
e <- Sys.time()
print(paste0(e, ": Step 3 finished"))
print(paste0("Step 3 took ", format(as.numeric(e - b), digits = 3)))

# 4. Create form level fact for records with a ch-postcode: ~11hr (parallel 8)
# create_form_level_patient_addresses(
#   address_data = "INT646_ABP_CQC_20210401_20220331"
# )

b <- Sys.time()
print(paste0(b, ": Starting step 5..."))
# 5. Match patient details against ch-postcode uprn and process: 0.5hr
create_care_home_address_match(
  patient_address_data = "INT646_FORMS_20210401_20220331",
  lookup_address_data = "INT646_ABP_CQC_20210401_20220331",
  parent_uprn_data = "INT646_ABP_20220324"
)
e <- Sys.time()
print(paste0(e, ": Step 5 finished"))
print(paste0("Step 5 took ", format(as.numeric(e - b), digits = 3)))

b <- Sys.time()
print(paste0(b, ": Starting step 6..."))
# 6. Create postcode lookup table (latest available mappings) for joining in the next step: 2min
create_postcode_lookup() # Run once in first epoch script
e <- Sys.time()
print(paste0(e, ": Step 6 finished"))
print(paste0("Step 6 took ", format(as.numeric(e - b), digits = 3)))

b <- Sys.time()
print(paste0(b, ": Starting step 7..."))
# 7. Join to fact table and get non ch-postcode records within time frame: 11hr
create_matched_prescription_base_table(
  match_data = "INT646_MATCH_20210401_20220331",
  form_data = "INT646_FORMS_20210401_20220331"
)
e <- Sys.time()
print(paste0(e, ": Step 7 finished"))
print(paste0("Step 7 took ", format(as.numeric(e - b), digits = 3)))