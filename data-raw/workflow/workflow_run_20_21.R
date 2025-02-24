# Load/install all required packages and functions
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("data-raw/workflow/workflow_production.R")

# Specify variables to retain at end of each script
keep_vars = c(ls(), 'keep_vars')

# FY 20/21 ---------------------------------------------------------------------


# Set variables -----------------------------------------------------------

# Set these manually ...
dry_run      <- TRUE # Set to TRUE to not run scripts, but instead check variables

year_from    <- 2020
year_to      <- 2021
cqc_date     <- NULL     # Set to null to recreate in step 1, or provide date string
                         # like yyyymmdd to reuse previously created CQC table

# ... then these are set automatically
run_cqc      <- is.null(cqc_date)
if(is.null(cqc_date)) {
  cqc_date   <- as.integer(format(today(), "%Y%m%d"))
}
start_str    <- glue("{year_from}0401")
end_str      <- glue("{year_to}0331")
start_date   <- as.character(as.Date(start_str, format = '%Y%m%d'))
end_date     <- as.character(as.Date(end_str, format = '%Y%m%d'))
cqc_data     <- glue("INT646_CQC_{cqc_date}")
abp_data     <- glue("INT646_ABP_{get_abp_epoch(end_date)}")
address_data <- glue("INT646_ABP_CQC_{start_str}_{end_str}")
patient_data <- glue("INT646_FORMS_{start_str}_{end_str}")
match_data   <- glue("INT646_MATCH_{start_str}_{end_str}")


# Run workflow steps ------------------------------------------------------

# 1. Get latest cqc data: ~0.5hr
if(run_cqc) {
  get_latest_cqc_data(
    cqc_date,
    dry_run
  )
}

# 2. Get a select ab plus epoch: ~130 mins
get_abp_from_dall_ref(
  end_date = end_date,
  dry_run
)

# 3. Merge and process cqc and ab plus: ~3 mins
create_ab_plus_cqc_data(
  ab_plus_data = abp_data,
  cqc_data = cqc_data,
  start_date = start_date,
  end_date =   end_date,
  dry_run
)

# 4. Create form level fact for records with a ch-postcode: ~11-14hr
create_form_level_patient_addresses(
  address_data = address_data,
  dry_run
)

# 5. Match patient details against ch-postcode uprn and process: ~30-40 mins
create_care_home_address_match(
  patient_address_data = patient_data,
  lookup_address_data = address_data,
  parent_uprn_data = abp_data,
  dry_run
)

# 6. Create postcode lookup table (latest available mappings) for joining in the next step: ~5 min
create_postcode_lookup(
  dry_run
)

# 7. Join to fact table and get non ch-postcode records within time frame: ~9 hrs
create_matched_prescription_base_table(
  match_data = match_data,
  form_data = patient_data,
  dry_run
)
