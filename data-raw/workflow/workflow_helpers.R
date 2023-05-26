
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
  if(DBI::dbExistsTable(conn = con, name = table_name_db) == T){
    DBI::dbRemoveTable(conn = con, name = table_name_db)
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
#' @param  cqc_data: the name of the cqc db table
#' @param start_date: start date as a char in format 'YYYY-MM-DD'
#' @param end_date: end date as a char in format 'YYYY-MM-DD'
#' @noRd
get_cqc_postcodes = function(cqc_data, start_date, end_date){
  
  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")
  
  # Create a lazy table from the CQC care home table
  cqc_db <- con %>%
    dplyr::tbl(from = cqc_data)
  
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
  # data <- read_csv(
  #   csv,
  #   col_names = FALSE,
  #   col_types = cols(.default = col_character())
  # )
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
