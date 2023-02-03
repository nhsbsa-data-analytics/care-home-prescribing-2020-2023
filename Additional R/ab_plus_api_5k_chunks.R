
# Identify available data packages
url <- "https://api.os.uk/downloads/v1/dataPackages?key="
key <- Sys.getenv("OS_API_KEY")

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