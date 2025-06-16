# Part One: get ab plus package info and data downoad url ----------------------

# Packages
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")

# Home directory
home_dir = getwd()

# Identify available data packages
url <- "https://api.os.uk/downloads/v1/dataPackages?key="
key <- Sys.getenv("OS_API_KEY")

# Paste together information to create URL
url <- paste0(url, key)

# Get package info
package_data <- GET(url)

# Convert to workable content
package_info = fromJSON(rawToChar(package_data$content))

# Get url to get all 5km csv file names
package_info = package_info %>% 
  filter(name == "address_base_plus") %>% 
  select(versions) %>% 
  tidyr::unnest(cols = versions) %>% 
  arrange(desc(createdOn)) %>% 
  slice_head(n = 1)

# AB created date
ab_plus_epoch_date = package_info %>% 
  select(createdOn) %>% 
  mutate(
    createdOn = as.character(createdOn),
    createdOn = gsub("-", "", createdOn)
  ) %>% 
  pull()

ab_plus_epoch_date_field = package_info %>% 
  select(createdOn) %>% 
  mutate(
    createdOn = as.character(createdOn)
  ) %>% 
  pull()

# Print epoch data
print(paste0("This script will use AB Plus epoch: ", ab_plus_epoch_date))

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

# Part Three: Create table metadata to populate --------------------------------

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Define table name
table_name = paste0("ABP_FULL_", ab_plus_epoch_date)

# Drop any existing table beforehand
drop_table_if_exists_db(table_name)

# Define field types for db upload
sql_fields <- c(
  RELEASE_DATE = "DATE", # Added as stacking different AddressBase Plus cuts
  UPRN = "NUMBER",
  UDPRN = "NUMBER",
  CHANGE_TYPE = "VARCHAR2(1)",
  STATE = "NUMBER",
  STATE_DATE = "DATE",
  CLASS = "VARCHAR2(6)",
  PARENT_UPRN = "NUMBER",
  X_COORDINATE = "FLOAT",
  Y_COORDINATE = "FLOAT",
  LATITUDE = "FLOAT",
  LONGITUDE = "FLOAT",
  RPC = "NUMBER",
  LOCAL_CUSTODIAN_CODE = "NUMBER",
  COUNTRY = "VARCHAR2(1)",
  LA_START_DATE = "DATE",
  LAST_UPDATE_DATE = "DATE",
  ENTRY_DATE = "DATE",
  RM_ORGANISATION_NAME = "VARCHAR2(60)",
  LA_ORGANISATION = "VARCHAR2(100)",
  DEPARTMENT_NAME = "VARCHAR2(60)",
  LEGAL_NAME = "VARCHAR2(60)",
  SUB_BUILDING_NAME = "VARCHAR2(30)",
  BUILDING_NAME = "VARCHAR2(50)",
  BUILDING_NUMBER = "NUMBER", # missing
  SAO_START_NUMBER = "NUMBER",
  SAO_START_SUFFIX = "VARCHAR2(2)",
  SAO_END_NUMBER = "NUMBER",
  SAO_END_SUFFIX = "VARCHAR2(2)",
  SAO_TEXT = "VARCHAR2(90)",
  ALT_LANGUAGE_SAO_TEXT = "VARCHAR2(90)",
  PAO_START_NUMBER = "NUMBER",
  PAO_START_SUFFIX = "VARCHAR2(2)",
  PAO_END_NUMBER = "NUMBER",
  PAO_END_SUFFIX = "VARCHAR2(2)",
  PAO_TEXT = "VARCHAR2(90)",
  ALT_LANGUAGE_PAO_TEXT = "VARCHAR2(90)",
  USRN = "NUMBER",
  USRN_MATCH_INDICATOR = "VARCHAR2(1)",
  AREA_NAME = "VARCHAR2(40)",
  LEVEL_FIELD = "VARCHAR2(30)",
  OFFICIAL_FLAG = "VARCHAR2(1)",
  OS_ADDRESS_TOID = "VARCHAR2(20)",
  OS_ADDRESS_TOID_VERSION = "NUMBER",
  OS_ROADLINK_TOID = "VARCHAR2(20)",
  OS_ROADLINK_TOID_VERSION = "NUMBER",
  OS_TOPO_TOID = "VARCHAR2(20)",
  OS_TOPO_TOID_VERSION = "NUMBER",
  VOA_CT_RECORD = "NUMBER",
  VOA_NDR_RECORD = "NUMBER",
  STREET_DESCRIPTION = "VARCHAR2(150)", # increased from VARCHAR2(101)
  ALT_LANGUAGE_STREET_DESCRIPTOR = "VARCHAR2(110)",
  DEP_THOROUGHFARE = "VARCHAR2(80)",
  THOROUGHFARE = "VARCHAR2(80)",
  WELSH_DEP_THOROUGHFARE = "VARCHAR2(80)",
  WELSH_THOROUGHFARE = "VARCHAR2(80)",
  DOU_DEP_LOCALITY = "VARCHAR2(35)",
  DEP_LOCALITY = "VARCHAR2(35)",
  LOCALITY = "VARCHAR2(35)",
  WELSH_DEP_LOCALITY = "VARCHAR2(35)",
  WELSH_DOU_DEP_LOCALITY = "VARCHAR2(35)",
  TOWN_NAME = "VARCHAR2(30)",
  ADMINISTRATIVE_AREA = "VARCHAR2(30)",
  POST_TOWN = "VARCHAR2(35)",
  WELSH_POST_TOWN = "VARCHAR2(30)",
  POSTCODE = "VARCHAR2(8)",
  POSTCODE_LOCATOR = "VARCHAR2(8)",
  POSTCODE_TYPE = "VARCHAR2(1)",
  DELIVERY_POINT_SUFFIX = "VARCHAR2(2)",
  ADDRESSBASE_POSTAL = "VARCHAR2(1)",
  PO_BOX_NUMBER = "VARCHAR2(6)",
  WARD_CODE = "VARCHAR2(9)",
  PARISH_CODE = "VARCHAR2(9)",
  RM_START_DATE = "DATE",
  MULTI_OCC_COUNT = "NUMBER",
  VOA_NDR_P_DESC_CODE = "VARCHAR2(5)",
  VOA_NDR_SCAT_CODE = "VARCHAR2(4)",
  ALT_LANGUAGE = "VARCHAR2(3)"
)

# Define sql statement
create_sql <- paste0(
  "CREATE TABLE ",
  table_name,
  "(\n  ",
  paste0(names(sql_fields), " ", unname(sql_fields), collapse = ",\n  "),
  "\n);"
)

# Execute sql statment
DBI::dbExecute(con, create_sql)

# Part Four: write data to created table ---------------------------------------

# Read ab plus csv file from directory and process
read_temp_dir_csv = function(index){
  
  # Print index to 
  print(paste0(index, " out of ", length(temp_dir_files), " files"))
  
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
    # Get rid of all NAs for blanks
    mutate_all(~ ifelse(is.na(.), "", .)) %>% 
    rename(
      # Rename columns
      LEVEL_FIELD = LEVEL,
      ALT_LANGUAGE_STREET_DESCRIPTOR = ALT_LANGUAGE_STREET_DESCRIPTION,
      DEP_THOROUGHFARE = DEPENDENT_THOROUGHFARE,
      WELSH_DEP_THOROUGHFARE = WELSH_DEPENDENT_THOROUGHFARE,
      DOU_DEP_LOCALITY = DOUBLE_DEPENDENT_LOCALITY,
      DEP_LOCALITY = DEPENDENT_LOCALITY,
      WELSH_DEP_LOCALITY = WELSH_DEPENDENT_LOCALITY,
      WELSH_DOU_DEP_LOCALITY = WELSH_DOUBLE_DEPENDENT_LOCALITY
    ) %>% 
    mutate(
      # Dates
      RELEASE_DATE = as.Date(ab_plus_epoch_date_field),
      LA_START_DATE = as.Date(LA_START_DATE),
      LAST_UPDATE_DATE = as.Date(LAST_UPDATE_DATE),
      STATE_DATE = as.Date(STATE_DATE),
      ENTRY_DATE = as.Date(ENTRY_DATE),
      RM_START_DATE = as.Date(RM_START_DATE),
      # Numeric
      UPRN = as.numeric(UPRN),
      UDPRN = as.numeric(UDPRN),
      STATE = as.numeric(STATE),
      PARENT_UPRN = as.numeric(PARENT_UPRN),
      X_COORDINATE = as.numeric(X_COORDINATE),
      Y_COORDINATE = as.numeric(Y_COORDINATE),
      LATITUDE = as.numeric(LATITUDE),
      LONGITUDE = as.numeric(LONGITUDE),
      RPC = as.numeric(RPC),
      LOCAL_CUSTODIAN_CODE = as.numeric(LOCAL_CUSTODIAN_CODE),
      BUILDING_NUMBER = as.numeric(BUILDING_NUMBER),
      SAO_START_NUMBER = as.numeric(SAO_START_NUMBER),
      SAO_END_NUMBER = as.numeric(SAO_END_NUMBER),
      PAO_START_NUMBER = as.numeric(PAO_START_NUMBER),
      PAO_END_NUMBER = as.numeric(PAO_END_NUMBER),
      USRN = as.numeric(USRN),
      OS_ADDRESS_TOID_VERSION = as.numeric(OS_ADDRESS_TOID_VERSION),
      OS_ROADLINK_TOID_VERSION = as.numeric(OS_ROADLINK_TOID_VERSION),
      OS_TOPO_TOID_VERSION = as.numeric(OS_TOPO_TOID_VERSION),
      VOA_CT_RECORD = as.numeric(VOA_CT_RECORD),
      VOA_NDR_RECORD = as.numeric(VOA_NDR_RECORD),
      MULTI_OCC_COUNT = as.numeric(MULTI_OCC_COUNT)
    ) %>% 
    # Column reorder
    select(RELEASE_DATE, everything())
  
  # Create table
  dbWriteTable(
    conn = con,
    name = table_name,
    value = data,
    temporary = FALSE,
    append = TRUE
  )
  
  # Remove data and clean
  rm(data); gc()
}

# Process each ab plus file, each of which contain 1m records: 25 mins
lapply(1:length(temp_dir_files), read_temp_dir_csv); gc()

# Part Five: remove temp file and revert back to initial dir and clean ---------

# Unlink temp dir
unlink(output_dir, recursive = T)

# Back to initial directory
setwd(home_dir)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()
