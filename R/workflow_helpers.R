
#' @param table_name_db: name of proposed db table
#' @description deletes a db table if the name is already used
drop_table_if_exists_db = function(table_name_db){
  
  # Drop any existing table beforehand
  if(DBI::dbExistsTable(conn = con, name = "INT646_AB_PLUS_CQC") == T){
    DBI::dbRemoveTable(conn = con, name = "INT646_AB_PLUS_CQC")
  }
}

#' @param date_field: string in the form 'YYYY-MM-DD'
#' @description gets numerical year-month from string in date format
get_year_month_from_date = function(date_field){
  
  # Add a zero in fron tof single month integer
  min_two_digits = function(x) ifelse(nchar(x) == 1, paste0(0, x), x)
  
  # Paste year and month together
  output = as.integer(paste0(
    lubridate::year(date_field),
    min_two_digits(lubridate::month(date_field))
  ))
  
  # Return output
  return(output)
}

