
#' @description downloads the latest cqc data from the cqc api
#' @param none: date for data generated from Sys.date()
get_latest_cqc_data = function(){
  
  # source single script with no date input required
  source("data-raw/01_upload_cqc_data_from_api.R")
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
  source("data-raw/02_upload_ab_data_from_api.R")
} 


#' @description downloads a single epoch of ab plus closest to end_date
#' @param ab_plus_data: name of the ab plus db table
#' @param cqc_data: the name of the cqc db table
#' @param start_date: start date as a char in format 'YYYY-MM-DD'
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
stack_and_process_ab_plus_cqc_data = function(cqc_data, ab_plus_data, start_date, end_date){
  
  # Assign function inputs to global env
  assign("cqc_data", cqc_data, envir = globalenv())
  assign("ab_plus_data", ab_plus_data, envir = globalenv())
  assign("start_date", start_date, envir = globalenv())
  assign("end_date", end_date, envir = globalenv())
  
  # Get nearest ab plus to end date with cqc postcodes within time frame
  source("data-raw/03_address_base_cqc_merge.R")
}

