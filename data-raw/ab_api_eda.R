
# Identify available data packages
url <- "https://api.os.uk/downloads/v1/dataPackages?key="
key <- "oxuBbtUDR3G7WBZfAGMY39UGaR4gI3n6"

# Paste together information to create URL then GET
url_key <- paste0(url, key)
get_url_key <- GET(url_key)

# Get ID of the data package you want
package_info = fromJSON(rawToChar(get_url_key$content))

# Get url to get all 5km csv file names
url = package_info %>% 
  filter(name == "address_base_plus_5km") %>% 
  select(versions) %>% 
  tidyr::unnest(cols = versions) %>% 
  select(url) %>% 
  pull()

# Csv file info
csv_file_info = GET(url)
file_name_urls = fromJSON(rawToChar(csv_file_info$content))$downloads$url

# Get content frmo url
get_api_info = function(index){httr::GET(file_name_urls[index])$content}

# Run the process in parallel using n-2 cores
clust = parallel::makeCluster(parallel::detectCores()-2)

# Export required packages to each cluster
parallel::clusterEvalQ(clust, {library(httr)})

# Export required objects to cluster
parallel::clusterExport(clust, c('get_api_info', 'file_name_urls'))

# Define function within parlaplly: ~50mins
api_bin = parallel::parLapply(clust, 1:length(file_name_urls), get_api_info)

# Stop cluster
parallel::stopCluster(clust)

# Save data
saveRDS(api_bin, "API_BIN.Rds")

# Function to wriet then read csv file
write_read_csv = function(index){
  
  # Write csv to temp dir
  writeBin(api_bin[[index]], output_file, size = 1)
  
  # Handle errors
  out = tryCatch({
    
    # Read csv from temp dir
    data = readr::read_csv(
      output_file, 
      col_names = FALSE, 
      col_types = cols(.default = "c")
    )
    
    # Get ch uprn
    ch_postcodes = data %>%
      filter(X6 == 'RI01') %>%
      select(X66) %>%
      distinct()
    
    # Only uprn from a postcode with a ch
    output = data %>% inner_join(ch_postcodes, by = 'X66')
    
    # Return output
    return(output)
  },
  
  error = function(cond) {
    
    # Error message
    message(paste0("There was an error reading the file: ", index))
    
    # Return empty df
    output = data.frame(matrix(ncol=77, nrow=0)) %>%
      mutate_all(.funs = as.character)
    
    # Return output
    return(output)
  })
  
  # Return output
  return(out)
}

# Write read and filter csv files and bind into single df
results = lapply(1:length(api_bin), write_read_csv) %>% bind_rows()

# Part Two ---------------------------------------------------------------------

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
key <- "oxuBbtUDR3G7WBZfAGMY39UGaR4gI3n6"

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
    mutate(POSTCODE_LOCATOR = toupper(gsub("[^[:alnum:]]", "", POSTCODE_LOCATOR)))
  
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
results_df = lapply(1:length(temp_dir_files), read_temp_dir_csv) %>% 
  bind_rows() %>% 
  mutate(EPOCH = ab_plus_epoch_date) %>% 
  mutate(across(.cols = c('UPRN', 'UDPRN', 'PARENT_UPRN'), as.numeric))

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

# Disconnect connection to database
DBI::dbDisconnect(con)

# Clear environment and clean
rm(list = ls()); gc()
