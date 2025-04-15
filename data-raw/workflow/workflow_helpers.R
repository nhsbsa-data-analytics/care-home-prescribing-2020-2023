
#' @description loads/installs all required packages and functions 
#' @noRd
load_all_packages_and_functions = function(){
  
  # Source script containing all packages and functions
  source("data-raw/workflow/workflow_packages.R")
  source("data-raw/workflow/workflow_helpers.R")
  source("data-raw/workflow/workflow_production.R")
}


#' @param table_name_db: name of proposed db table
#' @description deletes a db table if the name is already used
#' @noRd
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
    print(glue("Table dropped: {table_name_db}"))
  } else {
    print(glue("Table does not exist: {table_name_db}"))
  }
}


#' @param date_field: string in the form 'YYYY-MM-DD'
#' @description gets numerical year-month from string in date format
#' @noRd
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
#' @noRd
get_integer_from_date = function(x) as.integer(gsub("-", "", x))


#' @description gets a list of distinct cqc postcodes within a timeframe
#' @param  cqc_tbl: the name of the cqc db table
#' @param start_date: start date as a char in format 'YYYY-MM-DD'
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_cqc_postcodes = function(cqc_tbl, start_date, end_date){
  
  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")
  
  # Create a lazy table from the CQC care home table
  cqc_db <- con %>%
    dplyr::tbl(from = cqc_tbl)
  
  # Get cqc postcodes to include within later ab plus join
  cqc_postcodes = cqc_db %>% 
    dplyr::mutate(
      REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
      DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD")
    ) %>% 
    dplyr::filter(
      !is.na(UPRN),
      REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
      is.na(DEREGISTRATION_DATE) | 
        DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
    ) %>% 
    dplyr::select(POSTCODE_LOCATOR = POSTCODE) %>% 
    dplyr::distinct() %>% 
    dplyr::collect()
  
  # Disconnect now, in case the function crashes due to memory restriction
  DBI::dbDisconnect(con)
  
  # Assign postcodes to global env for ab plus script to use
  assign("cqc_postcodes", cqc_postcodes, envir = globalenv())
}

#' @description Get single distinct value from select column
#' @noRd
pull_date_string = function(data, string_date){
  
  data %>% 
    dplyr::select({{string_date}}) %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
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
  
  if(purrr::is_empty(field.types)) {
    dbWriteTable(
      con,
      Id(schema = toupper(con@info$username), table = table_name),
      data
    )
  } else {
    dbWriteTable(
      con,
      Id(schema = toupper(con@info$username), table = table_name),
      data,
      field.types = field.types
    )
  }
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


# Grant table access to multiple users
# NOTE: Relies on con being in environment already
grant_table_access <- function(schemas, table_name) {
  schemas %>% walk(
    \(x) {
      DBI::dbExecute(
        con, 
        paste0("GRANT SELECT ON ", table_name, " TO ", x)
      )
    }
  )
}


#' Coerce compute statements ro run with specified degree of parallel
#' 
#' @param lazy_tbl name of the dbplyr lazy table
#' @param create_table_name name of user created sql table
#' @param n the degree of parallelism to enforce
#' 
#' @examples compute_with_parallelism(table_db, "INT646_TABLE_DB", 32) 
# Function to create table from query with specified degree of parallelism
compute_with_parallelism = function(lazy_tbl, create_table_name, n){
  
  # Pull the DB connection
  db_connection <- lazy_tbl$src$con
  
  # Render the sql query as text
  query = dbplyr::sql_render(lazy_tbl)
  
  # Modify query text
  new_query = paste0(
    "CREATE TABLE ", create_table_name, " AS SELECT /*+ PARALLEL(", n, ") */ * FROM ", "(", query, ")"
  )
  
  # Send query to the database
  DBI::dbSendQuery(conn = db_connection, statement = new_query)
}


# Read AB+ csvs, filter out irrelevant entries, select columns of interest and
# apply minimal transformations. Finally append to temporary table in db.
# NOTE: it relies on following variables being defined:
#   csvs
#   abp_col_names
#   ab_plus_epoch_date
#   con
#   table_name_temp
process_csv = function(csv, index){
  # Print index to 
  print(paste0(index, " out of ", length(csvs), " files"))
  
  # Read in each csv and cast all columns as character
  data <- data.table::fread(
    csv,
    colClasses = "character"
  )
  
  # Apply column names from resource doc info
  names(data) = abp_col_names
  
  # Clean ab plus postcode data for binding and join
  data = data %>% 
    # Class filter
    filter(
      COUNTRY == "E",
      substr(CLASS, 1, 1) != "L", # Land
      substr(CLASS, 1, 1) != "O", # Other (Ordnance Survey only)
      substr(CLASS, 1, 2) != "PS", # Street Record
      substr(CLASS, 1, 2) != "RC", # Car Park Space
      substr(CLASS, 1, 2) != "RG", # Lock-Up / Garage / Garage Court
      substr(CLASS, 1, 1) != "Z", # Object of interest
    ) %>% 
    # Rename and remove column
    select(
      # DPA
      POST_TOWN,
      DEP_LOCALITY = DEPENDENT_LOCALITY,
      DOU_DEP_LOCALITY = DOUBLE_DEPENDENT_LOCALITY,
      THOROUGHFARE,
      DEP_THOROUGHFARE = DEPENDENT_THOROUGHFARE,
      PO_BOX_NUMBER,
      BUILDING_NUMBER,
      BUILDING_NAME,
      SUB_BUILDING_NAME,
      RM_ORGANISATION_NAME,
      DEPARTMENT_NAME,
      # GEO
      TOWN_NAME,
      LOCALITY,
      STREET_DESCRIPTION,
      PAO_END_SUFFIX,
      PAO_END_NUMBER,
      PAO_START_SUFFIX,
      PAO_START_NUMBER,
      PAO_TEXT,
      SAO_END_SUFFIX,
      SAO_END_NUMBER,
      SAO_START_SUFFIX,
      SAO_START_NUMBER,
      SAO_TEXT,
      LA_ORGANISATION,
      # Other
      POSTCODE_REMOVE = POSTCODE,
      POSTCODE = POSTCODE_LOCATOR,
      UPRN,
      PARENT_UPRN,
      CH_FLAG = CLASS
    ) %>% 
    # Generate new columns
    mutate(
      POSTCODE = toupper(gsub("[^[:alnum:]]", "", POSTCODE)),
      EPOCH = ab_plus_epoch_date,
      across(.cols = c('UPRN', 'PARENT_UPRN'), as.numeric),
      CH_FLAG = ifelse(CH_FLAG == "RI01", 1L, 0L)
    ) 
  
  # Create table
  dbWriteTable(
    conn = con,
    name = table_name_temp,
    value = data,
    temporary = FALSE,
    append = TRUE
  )
  
  # Remove data and clean
  rm(data); gc()
}

# modified code from https://github.com/ropensci/RSelenium/issues/221
getChromeDriverVersion <- function(versions = binman::list_versions("chromedriver")) {
  if ( xfun::is_unix() ) {
    chrome_driver_version <- system2(
      command = ifelse(
        xfun::is_macos(),
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
        "google-chrome-stable"
      ),
      args = "--version",
      stdout = TRUE,
      stderr = TRUE
    ) %>%
      stringr::str_extract(pattern = "(?<=Chrome )(\\d+\\.){3}")
    
  } else if ( xfun::is_windows() ) {
    chrome_driver_version <- system2(
      command = "wmic",
      args = glue(
        "datafile where name=\\
        'C:\\\\Program Files\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe' \\
        get Version /value"
      ),
      stdout = TRUE,
      stderr = TRUE
    ) %>%
      stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}")
    
  } else rlang::abort(
      message = 
        "Your OS couldn't be determined (Linux, macOS, Windows) or is not supported!"
    )
  
  # ... and determine most recent ChromeDriver version matching it
  chrome_driver_version %>%
    magrittr::extract(!is.na(.)) %>%
    stringr::str_replace_all(pattern = "\\.", replacement = "\\\\.") %>%
    paste0("^",  .) %>%
    stringr::str_subset(string = dplyr::last(versions)) %>%
    as.numeric_version() %>%
    max() %>%
    as.character()
}

# Simplified version of format postcode
simple_format_postcode_db <- function(df, postcode) {
  
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
      PCD_TEMP = case_when(
        LEN == 7 & substr(PCD_TEMP, 1, 1) == "5" ~ paste0("S", substr(PCD_TEMP, 2, 7)),
        LEN == 7 & substr(PCD_TEMP, 1, 1) == "0" ~ paste0("O", substr(PCD_TEMP, 2, 7)),
        # 7 character postcodes : 2nd character  (should be alpha)
        LEN == 7 & substr(PCD_TEMP, 2, 2) == "5" ~ paste0(substr(PCD_TEMP, 1, 1), "S", substr(PCD_TEMP, 3, 7)),
        LEN == 7 & substr(PCD_TEMP, 2, 2) == "0" ~ paste0(substr(PCD_TEMP, 1, 1), "O", substr(PCD_TEMP, 3, 7)),
        # 7 character postcodes : 3rd character  (should be number)
        LEN == 7 & substr(PCD_TEMP, 3, 3) == "S" ~ paste0(substr(PCD_TEMP, 1, 2), "5", substr(PCD_TEMP, 4, 7)),
        LEN == 7 & substr(PCD_TEMP, 3, 3) == "O" ~ paste0(substr(PCD_TEMP, 1, 2), "0", substr(PCD_TEMP, 4, 7)),
        LEN == 7 & substr(PCD_TEMP, 3, 3) == "I" ~ paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 7)),
        LEN == 7 & substr(PCD_TEMP, 3, 3) == "L" ~ paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 7)),
        # 7 character postcodes : 5th character  (should be number)
        LEN == 7 & substr(PCD_TEMP, 5, 5) == "S" ~ paste0(substr(PCD_TEMP, 1, 4), "5", substr(PCD_TEMP, 6, 7)),
        LEN == 7 & substr(PCD_TEMP, 5, 5) == "O" ~ paste0(substr(PCD_TEMP, 1, 4), "0", substr(PCD_TEMP, 6, 7)),
        LEN == 7 & substr(PCD_TEMP, 5, 5) == "I" ~ paste0(substr(PCD_TEMP, 1, 4), "1", substr(PCD_TEMP, 6, 7)),
        LEN == 7 & substr(PCD_TEMP, 5, 5) == "L" ~ paste0(substr(PCD_TEMP, 1, 4), "1", substr(PCD_TEMP, 6, 7)),
        # 7 character postcodes : 6th character  (should be alpha)
        LEN == 7 & substr(PCD_TEMP, 6, 6) == "5" ~ paste0(substr(PCD_TEMP, 1, 5), "S", substr(PCD_TEMP, 7, 7)),
        LEN == 7 & substr(PCD_TEMP, 6, 6) == "0" ~ paste0(substr(PCD_TEMP, 1, 5), "O", substr(PCD_TEMP, 7, 7)),
        # 7 character postcodes : 7th character  (should be alpha)
        LEN == 7 & substr(PCD_TEMP, 7, 7) == "5" ~ paste0(substr(PCD_TEMP, 1, 6), "S"),
        LEN == 7 & substr(PCD_TEMP, 7, 7) == "0" ~ paste0(substr(PCD_TEMP, 1, 6), "O"),
        # 6 character postcodes : 1st character (should be alpha)
        LEN == 6 & substr(PCD_TEMP, 1, 1) == "5" ~ paste0("S", substr(PCD_TEMP, 2, 6)),
        LEN == 6 & substr(PCD_TEMP, 1, 1) == "0" ~ paste0("O", substr(PCD_TEMP, 2, 6)),
        # 6 character postcodes : 4th character  (should be number)
        LEN == 6 & substr(PCD_TEMP, 4, 4) == "S" ~ paste0(substr(PCD_TEMP, 1, 3), "5", substr(PCD_TEMP, 5, 6)),
        LEN == 6 & substr(PCD_TEMP, 4, 4) == "O" ~ paste0(substr(PCD_TEMP, 1, 3), "0", substr(PCD_TEMP, 5, 6)),
        LEN == 6 & substr(PCD_TEMP, 4, 4) == "I" ~ paste0(substr(PCD_TEMP, 1, 3), "1", substr(PCD_TEMP, 5, 6)),
        LEN == 6 & substr(PCD_TEMP, 4, 4) == "L" ~ paste0(substr(PCD_TEMP, 1, 3), "1", substr(PCD_TEMP, 5, 6)),
        # 6 character postcodes : 5th character  (should be alpha)
        LEN == 6 & substr(PCD_TEMP, 5, 5) == "5" ~ paste0(substr(PCD_TEMP, 1, 4), "S", substr(PCD_TEMP, 6, 6)),
        LEN == 6 & substr(PCD_TEMP, 5, 5) == "0" ~ paste0(substr(PCD_TEMP, 1, 4), "O", substr(PCD_TEMP, 6, 6)),
        # 6 character postcodes : 6th character  (should be alpha)
        LEN == 6 & substr(PCD_TEMP, 6, 6) == "5" ~ paste0(substr(PCD_TEMP, 1, 5), "S"),
        LEN == 6 & substr(PCD_TEMP, 6, 6) == "0" ~ paste0(substr(PCD_TEMP, 1, 5), "O"),
        # 5 character postcodes : 1st character (should be alpha)
        LEN == 5 & substr(PCD_TEMP, 1, 1) == "5" ~ paste0("S", substr(PCD_TEMP, 2, 5)),
        LEN == 5 & substr(PCD_TEMP, 1, 1) == "0" ~ paste0("O", substr(PCD_TEMP, 2, 5)),
        # 5 character postcodes : 2nd character  (should be number)
        LEN == 5 & substr(PCD_TEMP, 2, 2) == "S" ~ paste0(substr(PCD_TEMP, 1, 1), "5", substr(PCD_TEMP, 3, 5)),
        LEN == 5 & substr(PCD_TEMP, 2, 2) == "O" ~ paste0(substr(PCD_TEMP, 1, 1), "0", substr(PCD_TEMP, 3, 5)),
        LEN == 5 & substr(PCD_TEMP, 2, 2) == "I" ~ paste0(substr(PCD_TEMP, 1, 1), "1", substr(PCD_TEMP, 3, 5)),
        LEN == 5 & substr(PCD_TEMP, 2, 2) == "L" ~ paste0(substr(PCD_TEMP, 1, 1), "1", substr(PCD_TEMP, 3, 5)),
        # 5 character postcodes : 3rd character  (should be number)
        LEN == 5 & substr(PCD_TEMP, 3, 3) == "S" ~ paste0(substr(PCD_TEMP, 1, 2), "5", substr(PCD_TEMP, 4, 5)),
        LEN == 5 & substr(PCD_TEMP, 3, 3) == "O" ~ paste0(substr(PCD_TEMP, 1, 2), "0", substr(PCD_TEMP, 4, 5)),
        LEN == 5 & substr(PCD_TEMP, 3, 3) == "I" ~ paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 5)),
        LEN == 5 & substr(PCD_TEMP, 3, 3) == "L" ~ paste0(substr(PCD_TEMP, 1, 2), "1", substr(PCD_TEMP, 4, 5)),
        # 5 character postcodes : 4th character  (should be alpha)
        LEN == 5 & substr(PCD_TEMP, 4, 4) == "5" ~ paste0(substr(PCD_TEMP, 1, 3), "S", substr(PCD_TEMP, 5, 5)),
        LEN == 5 & substr(PCD_TEMP, 4, 4) == "0" ~ paste0(substr(PCD_TEMP, 1, 3), "O", substr(PCD_TEMP, 5, 5)),
        # 5 character postcodes : 6th character  (should be alpha)
        LEN == 5 & substr(PCD_TEMP, 5, 5) == "5" ~ paste0(substr(PCD_TEMP, 1, 4), "S"),
        LEN == 5 & substr(PCD_TEMP, 5, 5) == "0" ~ paste0(substr(PCD_TEMP, 1, 4), "O"),
        T ~ PCD_TEMP
      ),
      
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

get_abp_epoch <- function(end_date) {
  con <- nhsbsaR::con_nhsbsa(database = "DALP")
  
  # Connect to ab plus in dall_ref
  ab <- con %>%
    tbl(from = in_schema("DALL_REF", "ADDRESSBASE_PLUS"))
  
  # Get closest release date
  ab %>% 
    select(RELEASE_DATE) %>% 
    distinct() %>% 
    collect() %>% 
    mutate(
      SELECT_DATE = as.Date(end_date),
      RELEASE_DATE = as.Date(RELEASE_DATE),
      DIFF = as.integer(abs(RELEASE_DATE - SELECT_DATE)),
      DB_DATE = as.integer(gsub("-", "", RELEASE_DATE))
    ) %>% 
    filter(DIFF == min(DIFF)) %>% 
    select(DB_DATE) %>% 
    pull()
}
