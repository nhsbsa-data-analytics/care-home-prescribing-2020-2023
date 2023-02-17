
#' @param table_name_db: name of proposed db table
#' @description deletes a db table if the name is already used
drop_table_if_exists_db = function(table_name_db){
  
  # Drop any existing table beforehand
  if(DBI::dbExistsTable(conn = con, name = table_name_db) == T){
    DBI::dbRemoveTable(conn = con, name = table_name_db)
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


#' @param date_field: string in the form 'YYYY-MM-DD'
#' @description gets date as 8 digit integer
get_integer_from_date = function(x) as.integer(gsub("-", "", x))


#' @description gets a list of distinct cqc postcodes within a timeframe
#' @param  cqc_data: the name of the cqc db table
#' @param start_date: start date as a char in format 'YYYY-MM-DD'
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
get_cqc_postcodes = function(cqc_data, start_date, end_date){
  
  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")
  
  # Create a lazy table from the CQC care home table
  cqc_db <- con %>%
    tbl(from = cqc_data)
  
  # Get cqc postcodes to include within later ab plus join
  cqc_postcodes = cqc_db %>% 
    mutate(
      REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
      DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD")
    ) %>% 
    filter(
      !is.na(UPRN),
      REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
      is.na(DEREGISTRATION_DATE) | 
        DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
    ) %>% 
    select(POSTCODE_LOCATOR = POSTCODE) %>% 
    distinct() %>% 
    collect()
  
  # Disconnect now, in case the function crashes due to memory restriction
  DBI::dbDisconnect(con)
  
  # Assign postcodes to globel env for ab plus script to use
  assign("cqc_postcodes", cqc_postcodes, envir = globalenv())
}


#' @description calculate the number and proportion of CQC CHs excluded due to null UPRNs for inclusion in caveats
#' @param  cqc_data: the name of the cqc db table
#' @param start_date: start date as a char in format 'YYYY-MM-DD'
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
count_cqc_chs_excluded = function(cqc_data, start_date, end_date){
  
  con <- nhsbsaR::con_nhsbsa(database = "DALP")
  
  cqc_db <- tbl(con, cqc_data)
  
  s <- cqc_db |>
    mutate(
      REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
      DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
      CH_FLAG = 1L,
      NULL_UPRN = ifelse(is.na(UPRN), 1L, 0L)
    ) |>
    filter(
      REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
      is.na(DEREGISTRATION_DATE) | 
        DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
    ) |>
    group_by(NULL_UPRN) |>
    summarise(N = n())
  
  N <- s |> filter(NULL_UPRN==1) |> pull(N)
  P <- N / (s |> summarise(TOTAL = sum(N)) |> pull(TOTAL))
  
  return(
    paste0(format(N, big.mark = ",", scientific = F),
           " (",
           round(P*100,1),"%",
           ") ",
           "excluded from CQC data for the period due to missing UPRNs")
  )
  
  # Disconnect now, in case the function crashes due to memory restriction
  DBI::dbDisconnect(con)
  
}
