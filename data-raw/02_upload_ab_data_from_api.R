
# Load packages and global variables
source("R/analysis_packages.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = "INT646_CQC_202301")

# Get cqc postcodes
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
  mutate(createdOn = as.character(createdOn)) %>% 
  pull()

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

# Get content from url: 3 mins
data = httr::GET(data_url)$content

# Define output directory
output_dir = tempdir()

# Define output file path
output_filepath = file.path(output_dir, data_file_name)

# Write binary content to temp file path
writeBin(data, output_filepath)

# Remove api content and clean
rm(data); gc()

# Get project directory
project_dir = getwd()

# Set to temp dir
setwd(output_dir)

# Get ab plus csv file names within directory
temp_dir_files = archive(data_file_name) %>% 
  select(path) %>% 
  filter(grepl("AddressBasePlus_FULL", path)) %>% 
  pull()

# Extract ab plus column names
abp_col_names = names(readr::read_csv(
  archive_read(data_file_name, file = "resources/AddressBasePlus_Header.csv")
  ))

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
    mutate(POSTCODE_LOCATOR = toupper(gsub("[^[:alnum:]]", "", POSTCODE_LOCATOR))) %>% 
    filter(
      substr(CLASS, 1, 1) != "L", # Land
      substr(CLASS, 1, 1) != "O", # Other (Ordnance Survey only)
      substr(CLASS, 1, 2) != "PS", # Street Record
      substr(CLASS, 1, 2) != "RC", # Car Park Space
      substr(CLASS, 1, 2) != "RG", # Lock-Up / Garage / Garage Court
      substr(CLASS, 1, 1) != "Z", # Object of interest
    )
  
  # Get postcodes with a care home
  ch_postcodes = data %>% 
    filter(COUNTRY == "E") %>% 
    filter(CLASS == 'RI01') %>% 
    select(POSTCODE_LOCATOR) %>% 
    rbind(cqc_postcodes) %>% 
    distinct()
  
  # Only uprn with a postcode where there's a ch
  output = data %>% 
    inner_join(ch_postcodes, by = "POSTCODE_LOCATOR")
  
  # Remove data and clean
  rm(data); gc()
  
  # Return data
  return(output)
}

# Process each ab plus file, each of which contain 1m records: 25 mins
results = lapply(1:length(temp_dir_files), read_temp_dir_csv)

# Bind into df then additional processing
results_df = results %>% 
  bind_rows() %>% 
  mutate(
    EPOCH = ab_plus_epoch_date,
    across(.cols = c('UPRN', 'UDPRN', 'PARENT_UPRN'), as.numeric),
    CH_FLAG = ifelse(CLASS == "RI01", 1L, 0L)
  ) %>% 
  select(-POSTCODE) %>% 
  mutate(POSTCODE = POSTCODE_LOCATOR)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Define table name
table_name = paste0(
  "INT646_AB_PLUS_", 
  substr(gsub("-", "", ab_plus_epoch_date),1,6)
  )

# Check if the table exists and drop any existing table beforehand
if(DBI::dbExistsTable(conn = con, name = table_name) == T){
  DBI::dbRemoveTable(conn = con, name = table_name)
}

# Upload to DB with indexes
con %>%
  copy_to(
    df = results_df,
    name = table_name,
    indexes = list(c("UPRN"), c("POSTCODE")),
    temporary = FALSE
  )

ab_plus_db = con %>%
  tbl(from = table_name)

ab_plus_db %>% 
  # Rename required as OS table names have changed
  rename(
    DEP_THOROUGHFARE = DEPENDENT_THOROUGHFARE,
    DOU_DEP_LOCALITY = DOUBLE_DEPENDENT_LOCALITY,
    DEP_LOCALITY = DEPENDENT_LOCALITY
  ) %>% 
  # Generate and clean SLA
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>% 
  select(
    UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS,
    CH_FLAG
  )

# When DPA != GEO then add a CORE single line address
ab_plus_db <-
  union_all(
    x = ab_plus_db %>%
      filter(
        is.na(DPA_SINGLE_LINE_ADDRESS) |
          is.na(GEO_SINGLE_LINE_ADDRESS) |
          DPA_SINGLE_LINE_ADDRESS == GEO_SINGLE_LINE_ADDRESS
      ),
    y = ab_plus_db %>%
      filter(
        !is.na(DPA_SINGLE_LINE_ADDRESS),
        !is.na(GEO_SINGLE_LINE_ADDRESS),
        DPA_SINGLE_LINE_ADDRESS != GEO_SINGLE_LINE_ADDRESS
      ) %>%
      nhsbsaR::oracle_merge_strings(
        first_col = "DPA_SINGLE_LINE_ADDRESS",
        second_col = "GEO_SINGLE_LINE_ADDRESS",
        merge_col = "CORE_SINGLE_LINE_ADDRESS"
      )
  ) %>% 
  mutate(UPRN = as.double(UPRN))

# Write the table back to the DB with indexes
ab_plus_db %>%
  compute(
    name = table_name,
    indexes = list(c("UPRN", c("POSTCODE"))),
    temporary = FALSE
  )

# Disconnect connection to database
DBI::dbDisconnect(con)

# Return to project directory
setwd(project_dir)

# Clear environment and clean
rm(list = ls()); gc()
