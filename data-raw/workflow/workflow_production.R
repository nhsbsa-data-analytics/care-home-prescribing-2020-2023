#' @description downloads the latest cqc data from the cqc api
#' @param none: date for data generated from Sys.date()
#' @noRd
get_latest_cqc_data = function(){
  
  # source single script with no date input required
  tictoc::tic(); source("data-raw/workflow/01_upload_cqc_data_from_api.R"); tictoc::toc(); print(Sys.time())
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_api = function(end_date){
  
  # Assign function inputs to global env
  assign("end_date", end_date, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic(); source("data-raw/workflow/02a_upload_ab_data_from_api.R"); tictoc::toc(); print(Sys.time())
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_abp_from_os = function(epoch_year){
  
  # Assign function inputs to global env
  assign("epoch_year", epoch_year, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic(); source("data-raw/workflow/02b_get_additional_ab_epoch.R"); tictoc::toc(); print(Sys.time())
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @param ab_plus_data: name of the ab plus db table
#' @param cqc_data: the name of the cqc db table
#' @noRd
create_ab_plus_cqc_data = function(cqc_data, ab_plus_data, start_date, end_date){
  
  # Assign function inputs to global env
  assign("cqc_data", cqc_data, envir = globalenv())
  assign("ab_plus_data", ab_plus_data, envir = globalenv())
  assign("start_date", start_date, envir = globalenv())
  assign("end_date", end_date, envir = globalenv())
  
  # stack and process the cqc and ab plus address data
  tictoc::tic(); source("data-raw/workflow/03_address_base_cqc_merge.R"); tictoc::toc(); print(Sys.time())
}


#' @description creates a patient address for each form within time period
#' @param address_data: name of the lookup address db table
#' @noRd
create_form_level_patient_addresses = function(address_data){
  
  # Assign function inputs to global env
  assign("address_data", address_data, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic(); source("data-raw/workflow/04_form_level_fact.R"); tictoc::toc(); print(Sys.time())
}


#' @description matches two address tables then process for ch flag
#' @param patient_address_data: patient address data
#' @param lookup_address_data: address data to be matched against
#' @noRd
create_care_home_address_match = function(patient_address_data, lookup_address_data, parent_uprn_data){
  
  # Assign function inputs to global env
  assign("patient_address_data", patient_address_data, envir = globalenv())
  assign("lookup_address_data", lookup_address_data, envir = globalenv())
  assign("parent_uprn_data", parent_uprn_data, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic(); source("data-raw/workflow/05_address_match.R"); tictoc::toc(); print(Sys.time())
}

#' @description creates the postcode lookup table in the DB containing latest available (hard-coded) mappings
create_postcode_lookup = function(){
  
  # Write postcode lookup table to the DB for the appropriate FY
  tic(); source("data-raw/workflow/06_postcode_lookup.R"); toc(); print(Sys.time())
}



#' @description gets prescription info for matched records
#' @param match_data: matched address data
#' @noRd
create_matched_prescription_base_table = function(match_data, form_data){
  
  # Assign function inputs to global env
  assign("match_data", match_data, envir = globalenv())
  assign("form_data", form_data, envir = globalenv())

  # Get nearest ab plus to end date with cqc postcodes within time frame
  tictoc::tic(); source("data-raw/workflow/07_item_level_base.R"); tictoc::toc(); print(Sys.time())
}
