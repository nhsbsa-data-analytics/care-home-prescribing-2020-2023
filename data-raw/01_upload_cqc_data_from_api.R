# Install and load packages
source("R/analysis_packages.R")

# Connections and Existing Table check

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Get current year_month
year_month = as.integer(substr(gsub('-', '', Sys.Date()), 1, 6))

# Create table name
table_name = paste0("INT646_CQC_", year_month)

# Check if the table exists
exists <- DBI::dbExistsTable(conn = con, name = table_name)

# Drop any existing table beforehand
if (exists) DBI::dbRemoveTable(conn = con, name = table_name)

# Pull CQC data

# Set a partner code (if we don't set this then we struggle to throttle calls)
Sys.setenv(CQC_PARTNER_CODE = "NHSBSA")
cqcr::cqc_partner_code(check_env = TRUE)

# When we use the CQC API we need to be careful not to exceed 600 requests per
# minute (10 per second)

# Pull the CQC ID name and postcode for every care home (~ 60 requests)
cqc_locations_df <- cqcr::cqc_locations_search(care_home = TRUE)

# Vector of locations
location_vec = cqc_locations_df %>% 
  select(location_id) %>% 
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

# Get maximum chunk num
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

Sys.time()
tic()

# Generate word output
cqc_details = parallel::parLapply(
  clust, 
  1:length(location_vec), 
  get_cqc_api_location_data
  )

Sys.time()
toc()

Sys.time()
tic()
# Loop through vector of locations_ids
cqc_details = lapply(1:length(location_vec), get_cqc_api_location_data)
Sys.time()
toc()




# Stop Cluster
parallel::stopCluster(clust)

# For care homes project we are only interested in a subset of columns, so lets
# extract them
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

gc()

# Upload to DB with indexes
con %>%
  copy_to(
    df = cqc_details_df,
    name = "INT646_CQC",
    indexes = list(c("LOCATION_ID"), c("UPRN"), c("POSTAL_CODE")),
    temporary = FALSE
  )

# Disconnect connection to database
DBI::dbDisconnect(con)



# results = list()
# 
# for(i in 1:length(location_vec)){
#   print(i)
#   results[[i]] = get_cqc_api_location_data(i)
# }
