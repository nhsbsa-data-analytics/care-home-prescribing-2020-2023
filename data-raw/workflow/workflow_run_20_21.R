# Load/install all required packages and functions
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("data-raw/workflow/workflow_production.R")

# FY 20/21 ---------------------------------------------------------------------


# Set variables -----------------------------------------------------------

# Set these manually ...
# Set to TRUE to not run scripts, but instead get printouts of variable values
# that would be used.
dry_run       <- FALSE

# Start year of run
year_from     <- 2020

# Set to NULL to create CQC table in step 1, or provide date string like
# yyyymmdd (matching existing table) to reuse previously created CQC table
cqc_date      <- NULL

# ... then these are set automatically
year_to       <- year_from + 1
if(!is.null(pc_sample)) {
  pc_sample_f <- pc_sample %>% map_vec(\(x) gsub(" ", "", x))
}
run_cqc       <- is.null(cqc_date)
if(is.null(cqc_date)) {
  cqc_date    <- as.integer(format(today(), "%Y%m%d"))
}
start_str     <- glue("{year_from}0401")
end_str       <- glue("{year_to}0331")
start_date    <- as.character(as.Date(start_str, format = '%Y%m%d'))
end_date      <- as.character(as.Date(end_str, format = '%Y%m%d'))
cqc_tbl       <- glue("INT646_CQC_{cqc_date}")
abp_tbl       <- glue("INT646_ABP_{get_abp_epoch(end_date)}")
address_tbl   <- glue("INT646_ABP_CQC_{start_str}_{end_str}")
patient_tbl   <- glue("INT646_FORMS_{start_str}_{end_str}")
match_tbl     <- glue("INT646_MATCH_{start_str}_{end_str}")

# Specify variables to retain at end of each script
keep_vars = c(ls(), 'keep_vars')

# Run workflow steps ------------------------------------------------------

# 1. Get latest cqc data: ~1hr
if(run_cqc) {
  get_latest_cqc_data(dry_run)
}

# 2. Get a select ab plus epoch: ~3 hrs
get_abp_from_dall_ref(dry_run)

# 3. Merge and process cqc and ab plus: ~3 mins
create_ab_plus_cqc_data(dry_run)

# 4. Create form level fact for records with a ch-postcode: ~8 hrs
create_form_level_patient_addresses(dry_run)

# 5. Match patient details against ch-postcode uprn and process: ~30 mins
create_care_home_address_match(dry_run)

# 6. Create postcode lookup table (latest available mappings) for joining in the next step: ~5 min
create_postcode_lookup(dry_run)

# 7. Join to fact table and get non ch-postcode records within time frame: ~11 hrs
create_matched_prescription_base_table(dry_run)
