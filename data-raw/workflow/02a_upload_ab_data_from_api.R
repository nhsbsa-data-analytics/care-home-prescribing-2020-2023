
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
csvs = archive(data_file_name) %>% 
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
table_name = paste0("INT646_ABP_", ab_plus_epoch_date)

# Define temp table name
table_name_temp = paste0(table_name, "_TEMP")

# Drop table if it exists already
drop_table_if_exists_db(table_name_temp)

# Process each ab plus file, each of which contain 1m records: 25 mins
csvs %>% iwalk(process_csv); gc()

# Part three: save as db table in order to apply db functions ------------------

# Connect to temp table
ab_plus_db = con %>%
  tbl(from = table_name_temp) %>%
  # SLA creation plus formatting
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>% 
  nhsbsaR::oracle_merge_strings(
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
c("MIGAR", "ADNSH", "MAMCP") %>% lapply(
  \(x) {
    DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO ", x))
  }
) %>% invisible()

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Return to project directory
setwd(project_dir)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
