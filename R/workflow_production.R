
#' @description downloads the latest cqc data from the cqc api
#' @param none: date for data generated from Sys.date()
get_latest_cqc_data = function(){

  # source single script with no date input required
  tic(); source("data-raw/01_upload_cqc_data_from_api.R"); toc()
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @note this will be supplemented with postcodes from specified cqc data 
#' @param  cqc_data: the name of the cqc db table
#' @param start_date: start date as a char in format 'YYYY-MM-DD'
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
get_ab_plus_supplemented_with_cqc = function(cqc_data, start_date, end_date){
  
  # Assign function inputs to global env
  assign("cqc_data", cqc_data, envir = globalenv())
  assign("start_date", start_date, envir = globalenv())
  assign("end_date", end_date, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic(); source("data-raw/02_upload_ab_data_from_api.R"); toc()
} 


#' @description downloads a single epoch of ab plus closest to end_date
#' @param ab_plus_data: name of the ab plus db table
#' @param cqc_data: the name of the cqc db table
create_ab_plus_cqc_data = function(cqc_data, ab_plus_data){
  
  # Assign function inputs to global env
  assign("cqc_data", cqc_data, envir = globalenv())
  assign("ab_plus_data", ab_plus_data, envir = globalenv())
  
  # stack and process the cqc and ab plus address data
  tic(); source("data-raw/03_address_base_cqc_merge.R"); toc()
}


#' @description creates a patient address for each form within time period
#' @param address_data: name of the lookup address db table
create_form_level_patient_addresses = function(address_data){
  
  # Assign function inputs to global env
  assign("address_data", address_data, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic(); source("data-raw/04_form_level_fact.R"); toc()
}


#' @description matches two address tables then process for 
#' @param patient_address_data: patient address data
#' @param lookup_address_data: address data to be matched against
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
create_care_home_address_match = function(patient_address_data, lookup_address_data, end_date){
  
  # Assign function inputs to global env
  assign("patient_address_data", patient_address_data, envir = globalenv())
  assign("lookup_address_data", lookup_address_data, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic(); source("data-raw/05_address_match.R"); toc()
}

create_matched_prescription_base_table = function(match_data, start_date, end_date){
    
  # Assign function inputs to global env
  assign("match_data", match_data, envir = globalenv())
  assign("start_date", start_date, envir = globalenv())
  assign("end_date", end_date, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic; source("data-raw/06_item_level_base.R"); toc()

}