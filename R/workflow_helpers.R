
#' @param table_name_db: name of proposed db table
#' @description deletes a db table if the name is already used
drop_table_if_exists_db = function(table_name_db){
  
  # Drop any existing table beforehand
  if(DBI::dbExistsTable(conn = con, name = "INT646_AB_PLUS_CQC") == T){
    DBI::dbRemoveTable(conn = con, name = "INT646_AB_PLUS_CQC")
  }
}

#' @param x: numerical string to coerce to 2 digits
#' @description adds a zero before a single digit numerical string
min_two_digits = function(x) ifelse(nchar(x) == 1, paste0(0, x), x)
