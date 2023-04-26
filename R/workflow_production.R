
#' @description downloads the latest cqc data from the cqc api
#' @param none: date for data generated from Sys.date()
get_latest_cqc_data = function(){
  
  # source single script with no date input required
  tic(); source("EDA/mmc/01_upload_cqc_data_from_api.R"); toc(); print(Sys.time())
}


#' @description downloads a single epoch of ab plus closest to end_date
#' @note this will be supplemented with postcodes from specified cqc data 
#' @param  cqc_data: the name of the cqc db table
#' @param start_date: start date as a char in format 'YYYY-MM-DD'
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
get_ab_plus_supplemented_with_cqc = function(start_date, end_date){
  
  # Assign function inputs to global env
  assign("end_date", end_date, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic(); source("EDA/mmc/02_upload_ab_data_from_api.R"); toc(); print(Sys.time())
} 


#' @description downloads a single epoch of ab plus closest to end_date
#' @param ab_plus_data: name of the ab plus db table
#' @param cqc_data: the name of the cqc db table
create_ab_plus_cqc_data = function(cqc_data, ab_plus_data, start_date, end_date){
  
  # Assign function inputs to global env
  assign("cqc_data", cqc_data, envir = globalenv())
  assign("ab_plus_data", ab_plus_data, envir = globalenv())
  assign("start_date", start_date, envir = globalenv())
  assign("end_date", end_date, envir = globalenv())
  
  # stack and process the cqc and ab plus address data
  tic(); source("EDA/mmc/03_address_base_cqc_merge.R"); toc(); print(Sys.time())
}


#' @description creates a patient address for each form within time period
#' @param address_data: name of the lookup address db table
create_form_level_patient_addresses = function(address_data){
  
  # Assign function inputs to global env
  assign("address_data", address_data, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic(); source("EDA/mmc/04_form_level_fact.R"); toc(); print(Sys.time())
}


#' @description matches two address tables then process for ch flag
#' @param patient_address_data: patient address data
#' @param lookup_address_data: address data to be matched against
create_care_home_address_match = function(patient_address_data, lookup_address_data, parent_uprn_data){
  
  # Assign function inputs to global env
  assign("patient_address_data", patient_address_data, envir = globalenv())
  assign("lookup_address_data", lookup_address_data, envir = globalenv())
  assign("parent_uprn_data", parent_uprn_data, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic(); source("EDA/mmc/05_address_match.R"); toc(); print(Sys.time())
}


#' @description gets prescription info for matched records
#' @param match_data: matched address data
create_matched_prescription_base_table = function(match_data, form_data){
  
  # Assign function inputs to global env
  assign("match_data", match_data, envir = globalenv())
  assign("form_data", match_data, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  tic; source("EDA/mmc/06_item_level_base.R"); toc(); print(Sys.time())
}
