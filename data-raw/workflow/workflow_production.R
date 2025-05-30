#' @description downloads the latest cqc data from the cqc api
#' @param none: date for data generated from Sys.date()
#' @noRd
get_latest_cqc_data = function(dry_run = FALSE){
  print("Running step 1 with:")
  print(glue("cqc_date = {cqc_date}"))
  
  if (dry_run) return(invisible())
  
  # Source single script with no date input required
  tictoc::tic()
  source("data-raw/workflow/01_upload_cqc_data_from_api.R")
  tictoc::toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_api = function(dry_run = FALSE){
  print("Running step 2 with:")
  print(glue("end_date = {end_date}"))
    
  if (dry_run) return(invisible())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/02a_upload_ab_data_from_api.R")
  tictoc::toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_os = function(dry_run = FALSE){
  print("Running step 2 with:")
  print(glue("epoch_year = {epoch_year}"))
    
  if (dry_run) return(invisible())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/02b_get_additional_ab_epoch.R")
  tictoc::toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_dall_ref = function(dry_run = FALSE){
  print("Running step 2 with:")
  print(glue("end_date = {end_date}"))
    
  if (dry_run) return(invisible())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/02c_ab_data_from_dall_ref.R")
  tictoc::toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param abp_tbl: name of the ab plus db table
#' @param cqc_tbl: the name of the cqc db table
#' @noRd
create_ab_plus_cqc_data = function(dry_run = FALSE){
  print("Running step 3 with:")
  print(glue("cqc_tbl = {cqc_tbl}"))
  print(glue("abp_tbl = {abp_tbl}"))
  print(glue("start_date = {start_date}"))
  print(glue("end_date = {end_date}"))
    
  if (dry_run) return(invisible())
  
  # Stack and process the cqc and ab plus address data
  tictoc::tic()
  source("data-raw/workflow/03_address_base_cqc_merge.R")
  tictoc::toc()
}


#' @description creates a patient address for each form within time period
#' @param address_tbl: name of the lookup address db table
#' @noRd
create_form_level_patient_addresses = function(dry_run = FALSE){
  print("Running step 4 with:")
  print(glue("address_tbl = {address_tbl}"))
    
  if (dry_run) return(invisible())
  
  # Join relevant fact tables and save as single table
  tictoc::tic()
  source("data-raw/workflow/04_form_level_fact.R")
  tictoc::toc()
}


#' @description matches two address tables then process for ch flag
#' @param patient_address_tbl: patient address data
#' @param lookup_address_tbl: address data to be matched against
#' @noRd
create_care_home_address_match = function(dry_run = FALSE){
  print("Running step 5 with:")
  print(glue("patient_tbl = {patient_tbl}"))
  print(glue("address_tbl = {address_tbl}"))
  print(glue("abp_tbl = {abp_tbl}"))
    
  if (dry_run) return(invisible())
  
  # Perform address matching and save results
  tictoc::tic()
  source("data-raw/workflow/05_address_match.R")
  tictoc::toc()
}


#' @description creates the postcode lookup table in the DB containing latest available (hard-coded) mappings
create_postcode_lookup = function(dry_run = FALSE){
  print("Running step 6")
    
  if (dry_run) return(invisible())
  
  # Write postcode lookup table to the DB for the appropriate FY
  tictoc::tic()
  source("data-raw/workflow/06_postcode_lookup.R")
  tictoc::toc()
}


#' @description gets prescription info for matched records
#' @param match_tbl: matched address data
#' @noRd
create_matched_prescription_base_table = function(dry_run = FALSE){
  print("Running step 7 with:")
  print(glue("match_tbl = {match_tbl}"))
  print(glue("patient_tbl = {patient_tbl}"))
    
  if (dry_run) return(invisible())
  
  # Create final base table for FY
  tictoc::tic()
  source("data-raw/workflow/07_item_level_base.R")
  tictoc::toc()
}
