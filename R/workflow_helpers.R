
#' @param none
#' @description loads/installs all required packages and functions 
load_all_packages_and_functions = function(){
  
  # Source script containing all packages and functions
  source("R/analysis_packages.R")
  source("R/workflow_helpers.R")
  source("R/workflow_production.R")
}


#' @param table_name_db: name of proposed db table
#' @description deletes a db table if the name is already used
drop_table_if_exists_db = function(table_name_db){
  
  # Drop any existing table beforehand
  if(DBI::dbExistsTable(
      conn = con,
      name = Id(schema = toupper(con@info$username), table = table_name_db)
    ) == T
  ){
    DBI::dbRemoveTable(
      conn = con,
      name = Id(schema = toupper(con@info$username), table = table_name_db)
    )
    print("Table dropped")
  } else {
    print("Table does not exist")
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

# Get single distinct value from select column
pull_date_string = function(data, string_date){
  
  data %>% 
    select({{string_date}}) %>% 
    distinct() %>% 
    pull()
}

#' format_postcode_db(df, postcode)
format_postcode_db <- function(df, postcode) {
  
  # Simple formatting of postcode
  df <- df %>%
    dplyr::mutate(POSTCODE_OLD := {{ postcode }})
  
  # Just Process distinct postcodes
  output <- df %>%
    dplyr::select(POSTCODE_OLD, {{ postcode }}) %>%
    dplyr::filter(!is.na({{ postcode }})) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      # Format and split postcode
      {{ postcode }} := ifelse(nchar({{ postcode }}) == 0, NA, {{ postcode }}),
      {{ postcode }} := toupper(REGEXP_REPLACE({{ postcode }}, "[^[:alnum:]]", "")),
      # Length vars to aid below logic
      LEN = nchar({{ postcode }}),
      # copy the postcode
      PCD_TEMP = {{ postcode }},
      # each potential transposition needs to be handled as a separate if statement
      # 7 character postcodes : 1st character (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 1, 1) == "5", paste0("S", substr(PCD_TEMP, 2, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 1, 1) == "0", paste0("O", substr(PCD_TEMP, 2, 7)), PCD_TEMP),
      # 7 character postcodes : 2nd character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 2, 2) == "5", paste0(substr(PCD_TEMP, 1, 1), "S", substr(PCD_TEMP, 3, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 2, 2) == "0", paste0(substr(PCD_TEMP, 1, 1), "O", substr(PCD_TEMP, 3, 7)), PCD_TEMP),
      # 7 character postcodes : 3rd character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "S", paste0(substr(PCD_TEMP, 1, 2), "5", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "O", paste0(substr(PCD_TEMP, 1, 2), "0", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "I", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 3, 3) == "L", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 7)), PCD_TEMP),
      # 7 character postcodes : 5th character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "S", paste0(substr(PCD_TEMP, 1, 4), "5", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "O", paste0(substr(PCD_TEMP, 1, 4), "0", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "I", paste0(substr(PCD_TEMP, 1, 4), "1", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 5, 5) == "L", paste0(substr(PCD_TEMP, 1, 4), "1", substr(PCD_TEMP, 6, 7)), PCD_TEMP),
      # 7 character postcodes : 6th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 6, 6) == "5", paste0(substr(PCD_TEMP, 1, 5), "S", substr(PCD_TEMP, 7, 7)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 6, 6) == "0", paste0(substr(PCD_TEMP, 1, 5), "O", substr(PCD_TEMP, 7, 7)), PCD_TEMP),
      # 7 character postcodes : 7th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 7, 7) == "5", paste0(substr(PCD_TEMP, 1, 6), "S"), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 7 & substr(PCD_TEMP, 7, 7) == "0", paste0(substr(PCD_TEMP, 1, 6), "O"), PCD_TEMP),
      # 6 character postcodes : 1st character (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 1, 1) == "5", paste0("S", substr(PCD_TEMP, 2, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 1, 1) == "0", paste0("O", substr(PCD_TEMP, 2, 6)), PCD_TEMP),
      # 6 character postcodes : 4th character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "S", paste0(substr(PCD_TEMP, 1, 3), "5", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "O", paste0(substr(PCD_TEMP, 1, 3), "0", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "I", paste0(substr(PCD_TEMP, 1, 3), "1", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 4, 4) == "L", paste0(substr(PCD_TEMP, 1, 3), "1", substr(PCD_TEMP, 5, 6)), PCD_TEMP),
      # 6 character postcodes : 5th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 5, 5) == "5", paste0(substr(PCD_TEMP, 1, 4), "S", substr(PCD_TEMP, 6, 6)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 5, 5) == "0", paste0(substr(PCD_TEMP, 1, 4), "O", substr(PCD_TEMP, 6, 6)), PCD_TEMP),
      # 6 character postcodes : 6th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 6, 6) == "5", paste0(substr(PCD_TEMP, 1, 5), "S"), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 6 & substr(PCD_TEMP, 6, 6) == "0", paste0(substr(PCD_TEMP, 1, 5), "O"), PCD_TEMP),
      # 5 character postcodes : 1st character (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 1, 1) == "5", paste0("S", substr(PCD_TEMP, 2, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 1, 1) == "0", paste0("O", substr(PCD_TEMP, 2, 5)), PCD_TEMP),
      # 5 character postcodes : 2nd character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "S", paste0(substr(PCD_TEMP, 1, 1), "5", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "O", paste0(substr(PCD_TEMP, 1, 1), "0", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "I", paste0(substr(PCD_TEMP, 1, 1), "1", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 2, 2) == "L", paste0(substr(PCD_TEMP, 1, 1), "1", substr(PCD_TEMP, 3, 5)), PCD_TEMP),
      # 5 character postcodes : 3rd character  (should be number)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "S", paste0(substr(PCD_TEMP, 1, 2), "5", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "O", paste0(substr(PCD_TEMP, 1, 2), "0", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "I", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 3, 3) == "L", paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 5)), PCD_TEMP),
      # 5 character postcodes : 4th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 4, 4) == "5", paste0(substr(PCD_TEMP, 1, 3), "S", substr(PCD_TEMP, 5, 5)), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 4, 4) == "0", paste0(substr(PCD_TEMP, 1, 3), "O", substr(PCD_TEMP, 5, 5)), PCD_TEMP),
      # 5 character postcodes : 6th character  (should be alpha)
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 5, 5) == "5", paste0(substr(PCD_TEMP, 1, 4), "S"), PCD_TEMP),
      PCD_TEMP = dplyr::if_else(LEN == 5 & substr(PCD_TEMP, 5, 5) == "0", paste0(substr(PCD_TEMP, 1, 4), "O"), PCD_TEMP),
      # replace postcode with formatted string
      {{ postcode }} := PCD_TEMP
    )
  
  
  # Rejoin back to original data
  df <- df %>%
    dplyr::select(-{{ postcode }}) %>%
    dplyr::left_join(y = output, by = "POSTCODE_OLD") %>%
    dplyr::select(-c(LEN, POSTCODE_OLD, PCD_TEMP))
  
  # Return formatted df
  return(df)
}

#' Merge Two Strings Together Whilst Retaining an Order of Some Kind
oracle_merge_strings_edit <- function(df, first_col, second_col, merge_col) {
  
  # Get the unique combinations we want to merge (in case there are duplicates)
  distinct_df <- df %>%
    dplyr::distinct(.data[[first_col]], .data[[second_col]])
  
  # Process columns (loop over each one as we repeat the processing)
  col_dfs <- list()
  for (col in c(first_col, second_col)) {
    col_dfs[[col]] <- distinct_df %>%
      # Get the unique values
      dplyr::distinct(.data[[col]]) %>%
      # Tokenise
      nhsbsaR::oracle_unnest_tokens(
        col = col,
        drop = FALSE
      ) %>%
      # Give each token a rank within the string (e.g. 'CITY-1', 'CITY-2', etc)
      dplyr::group_by(.data[[col]], TOKEN) %>%
      dplyr::mutate(TOKEN_RANK = dplyr::row_number(TOKEN_NUMBER)) %>%
      dplyr::ungroup() %>%
      # Rename the token number column
      dplyr::rename("{col}_TOKEN_NUMBER" := TOKEN_NUMBER) %>%
      # Join back to the unique combinations (handy for full_join later)
      dplyr::inner_join(y = distinct_df)
  }
  
  # Join the tokenised data together (attempt to join by TOKEN and TOKEN_RANK)
  distinct_df <-
    dplyr::full_join(
      x = col_dfs[[first_col]],
      y = col_dfs[[second_col]]
    )
  
  # Pull the DB connection
  db_connection <- df$src$con
  
  # Build SQL Query
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "WITH LT AS
    (
      SELECT ",
    dplyr::sql(first_col), ", ",
    dplyr::sql(second_col), ", ",
    dplyr::sql(first_col), "_TOKEN_NUMBER, ",
    dplyr::sql(second_col), "_TOKEN_NUMBER, ", "
        TOKEN,
        COALESCE(", dplyr::sql(first_col), "_TOKEN_NUMBER, ", "LEAD(", dplyr::sql(first_col), "_TOKEN_NUMBER IGNORE NULLS) OVER (PARTITION BY ", dplyr::sql(first_col), ", ", dplyr::sql(second_col), " ORDER BY ", dplyr::sql(second_col), "_TOKEN_NUMBER)) AS LEAD_TOKEN_NUMBER
      FROM
        (", dbplyr::sql_render(distinct_df), ")
    )
    SELECT ",
    dplyr::sql(first_col), ", ",
    dplyr::sql(second_col), ",
      LISTAGG(TOKEN, ' ') within group (order by LEAD_TOKEN_NUMBER, ", dplyr::sql(second_col), "_TOKEN_NUMBER) as ", dplyr::sql(merge_col), "
    FROM
      LT
    GROUP BY ",
    dplyr::sql(first_col), ", ",
    dplyr::sql(second_col)
  )
  
  # Generate merged strings from the query
  merged_df <- dplyr::tbl(src = db_connection, dplyr::sql(sql_query))
  
  # Output the original data with the merged string joined to it
  df %>%
    dplyr::left_join(y = merged_df)
}


#' Unite columns by specified prefix
#' 
#' @description Given a list of new col names expressed as plurals terms, unite
#'  all columns starting with their singular prefix into a single column, with 
#'  values being the values of the columns as a comma separated list.
#'  
#'  This assumes the plural is the simplest case, an 's'.
#'
#' @param data A data.frame or tibble
#' @param ... The new columns on which to base the unites
#'
#' @return The data with unite operations applied
#' 
#' @examples
#' tibble(
#'   col1 = c("a", "b"), col2 = c("c", "d"),
#'   foo1 = c("e", "f"), foo2 = c("g", "h")
#' ) %>% 
#' unite_to_plural(cols, foos)
#' 
#' # A tibble: 2 × 2
#' #  cols  foos 
#' #  <chr> <chr>
#' # 1 a|c   e|g  
#' # 2 b|d   f|h 
unite_to_plural <- function(data, ...) {
  args <- as.character(match.call(expand.dots = FALSE)$`...`)

  united_cols <- lapply(
    args,
    function(x) {
      tidyr::unite(
        data %>% select(starts_with(substr(x, 1, nchar(x) - 1))),
        x,
        everything(),
        sep = "|",
        na.rm = TRUE
      ) %>% 
        rename({{x}} := 1)
    }
  )
  
  data %>%
    select(-starts_with(substr(args, 1, nchar(args) - 1))) %>%
    bind_cols(united_cols)
}


#' Print number of NA values in selected columns
#'
#' @param data A data.frame, tibble or lazy table
#' @param ... Columns as bare names
#'
#' @return Used for side effect only
#'
#' @examples
#' airquality %>% 
#'   check_na(Ozone, Solar.R)
#'
#' #  Ozone Solar.R
#' #     37       7
check_na <- function(data, ...) {
  check <- data %>%
    as_tibble() %>% 
    select(...)
  
  print(colSums(is.na(check)))
}


#' Write data.frame or tibble to database, with datatype set to appropriate
#' value for strings longer than default 255 characters
#'
#' @param data A data.frame or tibble
#' @param con A DB connection (only tried with Oracle SQL)
#' @param table_name Name of new table
#'
#' @return Used for side effect only
#'
#' @examples
#' con <- nhsbsaR::con_nhsbsa(database = "DALP")
#' tib <- tibble(x = rep("long!!", 1000))
#' tib <- write_table_long_chars(con, "LONG_TABLE")
write_table_long_chars <- function(data, con, table_name) {
  field.types = list()
  
  iwalk(data, \(x, idx) {
    if (typeof(x) == "character") {
      max_chars <- data %>%
        select(all_of(idx)) %>%
        # Convert all character columns to UTF-8, this prevents an error in nchar.
        # https://stackoverflow.com/questions/60906507/
        # how-to-solve-error-error-in-ncharrownamesm-invalid-multibyte-string-ele
        mutate(
          !!sym(idx) := iconv(!!sym(idx), "WINDOWS-1252", "UTF-8", sub = "")
        ) %>%
        summarise(max(nchar(!!sym(idx)), na.rm = TRUE)) %>%
        pull()
      
      if (max_chars > 255) {
        # Need the <<- so that field.types in parent env (i.e. the actual
        # function env) is updated
        field.types <<- append(
          field.types,
          setNames(glue("varchar2({max_chars * 2})"), idx)
        )
      }
    }
  })
  
  dbWriteTable(
    con,
    Id(schema = toupper(con@info$username), table = table_name),
    data,
    field.types = field.types
  )
}


#' Add indexes to an existing table
#'
#' @param con A DBI connection
#' @param table_name Name of table
#' @param indexes Character vector of column names to index on
#'
#' @export Used for side effect only
#'
#' @examples
#' con <- nhsbsaR::con_nhsbsa(database = "DALP")
#' con %>% add_indexes("EXISTING_TABLE", c("UPRN", "POSTCODE"))
add_indexes <- function(con, table_name, indexes) {
  walk(
    indexes,
    \(x) dbGetQuery(
      con,
      glue("CREATE INDEX {table_name}_{x} ON {table_name}({x});")
    )
  )
}
