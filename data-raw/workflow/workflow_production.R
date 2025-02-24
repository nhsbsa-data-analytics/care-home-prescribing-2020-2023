#' @description downloads the latest cqc data from the cqc api
#' @param none: date for data generated from Sys.date()
#' @noRd
get_latest_cqc_data = function(cqc_date, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("cqc_date", cqc_date, envir = globalenv())
  
  if (dry_run) {
    print("Running step 1 with:")
    print(glue("cqc_date = {cqc_date}"))
    
    return(invisible())
  }
  
  # source single script with no date input required
  tictoc::tic()
  source("data-raw/workflow/01_upload_cqc_data_from_api.R")
  tictoc::toc()
  print(Sys.time())
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_api = function(end_date, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("end_date", end_date, envir = globalenv())
  
  if (dry_run) {
    print("Running step 2 with:")
    print(glue("end_date = {end_date}"))
    
    return(invisible())
  }
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/02a_upload_ab_data_from_api.R")
  tictoc::toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_os = function(epoch_year, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("epoch_year", epoch_year, envir = globalenv())
  
  if (dry_run) {
    print("Running step 2 with:")
    print(glue("epoch_year = {epoch_year}"))
    
    return(invisible())
  }
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/02b_get_additional_ab_epoch.R")
  tictoc::toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_dall_ref = function(end_date, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("end_date", end_date, envir = globalenv())
  
  if (dry_run) {
    print("Running step 2 with:")
    print(glue("end_date = {end_date}"))
    
    return(invisible())
  }
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/02c_ab_data_from_dall_ref.R")
  tictoc::toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param ab_plus_data: name of the ab plus db table
#' @param cqc_data: the name of the cqc db table
#' @noRd
create_ab_plus_cqc_data = function(cqc_data, ab_plus_data, start_date, end_date, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("cqc_data", cqc_data, envir = globalenv())
  assign("ab_plus_data", ab_plus_data, envir = globalenv())
  assign("start_date", start_date, envir = globalenv())
  assign("end_date", end_date, envir = globalenv())
  
  if (dry_run) {
    print("Running step 3 with:")
    print(glue("cqc_data = {cqc_data}"))
    print(glue("ab_plus_data = {ab_plus_data}"))
    print(glue("start_date = {start_date}"))
    print(glue("end_date = {end_date}"))
    
    return(invisible())
  }
  
  # stack and process the cqc and ab plus address data
  tictoc::tic()
  source("data-raw/workflow/03_address_base_cqc_merge.R")
  tictoc::toc()
}


#' @description creates a patient address for each form within time period
#' @param address_data: name of the lookup address db table
#' @noRd
create_form_level_patient_addresses = function(address_data, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("address_data", address_data, envir = globalenv())
  
  if (dry_run) {
    print("Running step 4 with:")
    print(glue("address_data = {address_data}"))
    
    return(invisible())
  }
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/04_form_level_fact.R")
  tictoc::toc()
}


#' @description matches two address tables then process for ch flag
#' @param patient_address_data: patient address data
#' @param lookup_address_data: address data to be matched against
#' @noRd
create_care_home_address_match = function(patient_address_data, lookup_address_data, parent_uprn_data, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("patient_address_data", patient_address_data, envir = globalenv())
  assign("lookup_address_data", lookup_address_data, envir = globalenv())
  assign("parent_uprn_data", parent_uprn_data, envir = globalenv())
  
  if (dry_run) {
    print("Running step 5 with:")
    print(glue("patient_address_data = {patient_address_data}"))
    print(glue("lookup_address_data = {lookup_address_data}"))
    print(glue("parent_uprn_data = {parent_uprn_data}"))
    
    return(invisible())
  }
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/05_address_match.R")
  tictoc::toc()
}


#' @description creates the postcode lookup table in the DB containing latest available (hard-coded) mappings
create_postcode_lookup = function(dry_run = FALSE){
  
  if (dry_run) {
    print("Running step 6")
    
    return(invisible())
  }
  
  # Write postcode lookup table to the DB for the appropriate FY
  tictoc::tic()
  source("data-raw/workflow/06_postcode_lookup.R")
  tictoc::toc()
}


#' @description gets prescription info for matched records
#' @param match_data: matched address data
#' @noRd
create_matched_prescription_base_table = function(match_data, form_data, dry_run = FALSE){
  
  # Assign function inputs to global env
  assign("match_data", match_data, envir = globalenv())
  assign("form_data", form_data, envir = globalenv())

  if (dry_run) {
    print("Running step 7 with:")
    print(glue("match_data = {match_data}"))
    print(glue("form_data = {form_data}"))
    
    return(invisible())
  }
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic()
  source("data-raw/workflow/07_item_level_base.R")
  tictoc::toc()
}
