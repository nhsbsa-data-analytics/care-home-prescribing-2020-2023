
# Part One: get ab plus package info and data downoad url ----------------------

# Identify available data packages
url <- "https://api.os.uk/downloads/v1/dataPackages?key="
key <- Sys.getenv("OS_API_KEY")

# Paste together information to create URL
url <- paste0(url, key)

# Get package info
package_info <- GET(url)

# Get ID of the data package you want
package_info = fromJSON(rawToChar(package_info$content))

# Get url to get all 5km csv file names
package_info = package_info %>% 
  filter(name == "address_base_plus") %>% 
  select(versions) %>% 
  tidyr::unnest(cols = versions) %>% 
  mutate(
    end_date = as.Date(end_date),
    createdOn = as.Date(createdOn),
    date_diff = createdOn - end_date
  ) %>% 
  filter(date_diff >= 0) %>% 
  slice_min(date_diff, n = 1)

# AB created date
ab_plus_epoch_date = package_info %>% 
  select(createdOn) %>% 
  mutate(
    createdOn = as.character(createdOn),
    createdOn = gsub("-", "", createdOn)
  ) %>% 
  pull()

# Print epoch data
print(glue("This script will use AB Plus epoch: {ab_plus_epoch_date}"))

# Get ab plus epoch version api url
url = package_info %>% 
  select(url) %>% 
  pull()

# Csv file info
ab_info = GET(url)$content

# Get url for final download plus file name
ab_info = fromJSON(rawToChar(ab_info))$downloads

# Get final data url
data_url = ab_info %>% 
  select(url) %>% 
  pull()

# Get final data file name
data_file_name = ab_info %>% 
  select(fileName) %>% 
  pull()

# Part Two: get raw data, write as binary, then read and process ---------------

# Get content from url: 3 mins
data = httr::GET(data_url)$content

# Update message
print("Downloaded raw data")

# Define output directory
output_dir = tempdir()

# Define output file path
output_filepath = file.path(output_dir, data_file_name)

# Update message
print("Writing data from raw as binary")

# Write binary content to temp file path
writeBin(data, output_filepath)

# Remove api content and clean
rm(data); gc()

# Get project directory and set to temp dir
project_dir = setwd(output_dir)

# Get ab plus csv file names within directory
temp_dir_files = archive(data_file_name) %>% 
  select(path) %>% 
  filter(grepl("AddressBasePlus_FULL", path)) %>% 
  pull()

# Update message
print("Reading and processing binary")

# Extract ab plus column names
abp_col_names = names(readr::read_csv(
  archive_read(data_file_name, file = "resources/AddressBasePlus_Header.csv")
))

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Define table name
table_name = glue("INT646_ABP2_{ab_plus_epoch_date}")

# Define temp table name
table_name_temp = glue("{table_name}_TEMP")

# Drop table if it exists already
drop_table_if_exists_db(table_name_temp)

# Read ab plus csv file from directory and process
read_temp_dir_csv = function(index){
  
  # Print index to 
  print(glue("{index} out of {length(temp_dir_files)} files"))
  
  # Read in each csv and cast all columns as character
  data = readr::read_csv(
    archive_read("ABFLGB_CSV.zip", file = temp_dir_files[index]),
    col_names = FALSE,
    col_types = cols(.default = col_character())
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
  DBI::dbWriteTable(
    conn = con,
    name = table_name_temp,
    value = data,
    temporary = FALSE,
    append = TRUE
  )
  
  # Remove data and clean
  rm(data); gc()
}

# Process each ab plus file, each of which contain 1m records: 25 mins
lapply(1:length(temp_dir_files), read_temp_dir_csv); gc()

# Part three: save as db table in order to apply db functions ------------------

# Connect to temp table
ab_plus_db = con %>%
  tbl(from = table_name_temp) %>%
  # SLA creation plus formatting
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>% 
  oracle_merge_strings_edit(
    first_col = "DPA_SINGLE_LINE_ADDRESS",
    second_col = "GEO_SINGLE_LINE_ADDRESS",
    merge_col = "CORE_SINGLE_LINE_ADDRESS"
  ) %>% 
  select(
    UPRN,
    PARENT_UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS,
    CORE_SINGLE_LINE_ADDRESS,
    CH_FLAG,
    EPOCH
  ) 

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Write the table back to the DB with indexes
ab_plus_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# Drop table if it exists already
drop_table_if_exists_db(table_name_temp)

# Grant access
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO ADNSH"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MIGAR"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MAMCP"))

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(glue("This script has created table: {table_name}"))

# Return to project directory
setwd(project_dir)

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
