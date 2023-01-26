# Install and load packages
source("R/analysis_packages.R")

# Determine number of cqc care home records
care_home_total = httr::GET("https://api.cqc.org.uk/public/v1/locations?careHome=Y&page=1&perPage=1")

# Convert binary to character
no_of_pages = jsonlite::fromJSON(rawToChar(care_home_total$content))

# Get number of 10k blocks required
no_of_pages = ceiling(no_of_pages$total / 10000)

# Set a partner code (if we don't set this then we struggle to throttle calls)
get_cqc_locations_details = function(page_num){
  
  # Url with page number pasted inside
  url = paste0("https://api.cqc.org.uk/public/v1/locations?careHome=Y&page=", page_num, "&perPage=10000")
  
  # Get api data
  data = httr::GET(url)
  
  # Convert binary to character
  data = jsonlite::fromJSON(rawToChar(data$content))
  
  # Get location ino as df within list
  locations = data$locations
  
  # Return location info df
  return(locations)
}

# Get all location info, with 10k records per age retrieved
cqc_locations = lapply(1:no_of_pages, get_cqc_locations_details) %>% 
  bind_rows()

# Vector of locations
location_vec = cqc_locations %>%
  select(locationId) %>%
  pull()

# Function to query cqc api
get_cqc_api_location_data = function(loc_num){
  
  # Paste location url with location_id
  url = paste0("https://api.cqc.org.uk/public/v1/locations/", location_vec[loc_num])
  
  # Get data
  data = httr::GET(url)
  
  # Convert binary to character and convert json into df
  data = jsonlite::fromJSON(rawToChar(data$content)) %>% 
    unlist() %>% 
    bind_rows()
  
  # Return data
  return(data)
}

# Generate appropriate number o cores
n_cores = parallel::detectCores() - 2

# Set up parallel
clust = parallel::makeCluster(n_cores)

# Export libraries to cluster
parallel::clusterEvalQ(
  cl = clust,
  {
    library(dplyr); 
    library(httr);
    library(jsonlite)
  }
)

# Export required objects to cluster
parallel::clusterExport(
  cl = clust,
  varlist = c(
    "get_cqc_api_location_data",
    "location_vec"
  ),
  envir = environment()
)

# Generate cqc details
cqc_details = parallel::parLapply(
  cl = clust, 
  X = 1:length(location_vec), 
  fun = get_cqc_api_location_data
  )

# Stop Cluster
parallel::stopCluster(clust)

# Select care homes project columns of interest
cqc_details_df <- cqc_details %>%
  bind_rows() %>% 
  janitor::clean_names() %>% 
  # Filter to the period of interest
  mutate(
    # Add the nursing home / residential home flag
    nursing_home = ifelse(
      test = if_any(
        .cols = starts_with("gac") & contains("name"),
        .fns = ~ grepl(pattern = "Nursing home", x = .x)
      ),
      yes = 1L,
      no = 0L
    ),
    residential_home = ifelse(
      test = if_any(
        .cols = starts_with("gac") & contains("name"),
        .fns = ~ grepl(pattern = "Residential home", x = .x)
      ),
      yes = 1L,
      no = 0L
    ),
    # Change type of numeric col
    number_of_beds = as.integer(number_of_beds)
  ) %>%
  # Select the required cols and uppercase
  select(
    location_id,
    uprn,
    registration_status,
    registration_date,
    deregistration_date,
    dormancy,
    name,
    # Change in postal_line_address formatting
    postal_address_line1,
    postal_address_line2,
    postal_address_town_city,
    postal_address_county,
    postal_code,
    nursing_home,
    residential_home,
    type,
    number_of_beds
  ) %>%
  rename_with(toupper); gc()

# Get current year_month
year_month = as.integer(substr(gsub('-', '', Sys.Date()), 1, 6))

# Create table name
table_name = paste0("INT646_CQC_", year_month)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Check if the table exists and drop any existing table beforehand
if(DBI::dbExistsTable(conn = con, name = table_name) == T){
  DBI::dbRemoveTable(conn = con, name = table_name)
}

# Upload to DB with indexes
con %>%
  copy_to(
    df = cqc_details_df,
    name = table_name,
    indexes = list(c("LOCATION_ID"), c("UPRN"), c("POSTAL_CODE")),
    temporary = FALSE
  )

# Disconnect connection to database
DBI::dbDisconnect(con)

# Clear environment and clean
rm(list = ls()); gc()

#-------------------------------------------------------------------------------

# # Enter NHSBSA credentials
# Sys.setenv(CQC_PARTNER_CODE = "NHSBSA")
# cqcr::cqc_partner_code(check_env = TRUE)
# 
# # Pull the CQC ID name and postcode for every care home (~ 60 requests)
# cqc_locations_df <- cqcr::cqc_locations_search(care_home = TRUE)
# 