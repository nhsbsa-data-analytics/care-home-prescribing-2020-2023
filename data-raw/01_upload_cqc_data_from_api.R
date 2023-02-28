
# Function to get api content from url
get_api_content = function(url){
  
  # Get api data
  data = httr::GET(url)
  
  # Convert binary to character
  content = jsonlite::fromJSON(rawToChar(data$content))
  
  # Return content
  return(content)
}

# Get number of cqc pages for main api query
no_of_pages = get_api_content(
  "https://api.cqc.org.uk/public/v1/locations?careHome=Y&page=1&perPage=1"
  )

# Get number of 10k blocks required
no_of_pages = ceiling(no_of_pages$total / 10000)

# Set a partner code (if we don't set this then we struggle to throttle calls)
get_cqc_locations_details = function(page_num){
  
  # Url with page number pasted inside
  url = paste0(
    "https://api.cqc.org.uk/public/v1/locations?careHome=Y&page=", 
    page_num, 
    "&perPage=10000"
    )
  
  # Get api data
  data = get_api_content(url)
  
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
  url = paste0(
    "https://api.cqc.org.uk/public/v1/locations/", 
    location_vec[loc_num]
    )
  
  # Get data
  data = get_api_content(url) %>% 
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
    "get_api_content",
    "get_cqc_api_location_data",
    "location_vec"
  ),
  envir = environment()
)

# Print script update
print("Now downloading CQC API data ...")

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
    registration_date,
    deregistration_date,
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
  ) ; gc()

# Get current year_month
download_date = as.integer(gsub('-', '', Sys.Date()))

# Process the cqc df output, in preparation for matching
cqc_process_df = cqc_details_df %>% 
  rename(postcode = postal_code) %>% 
  mutate(
    # Paste fields together to create single line address
    single_line_address = toupper(paste(
      ifelse(is.na(name), "", name),
      ifelse(is.na(postal_address_line1), "", postal_address_line1),
      ifelse(is.na(postal_address_line2), "", postal_address_line2),
      ifelse(is.na(postal_address_town_city), "", postal_address_town_city),
      ifelse(is.na(postal_address_county), "", postal_address_county)
    )),
    # Postcode Cleaning
    postcode = toupper(gsub("[^[:alnum:]]", "", postcode)),
    # Address cleaning
    single_line_address = gsub(" & ", "and", single_line_address),
    single_line_address = gsub("(\\D)(\\d)", "\\1 \\2", single_line_address),
    single_line_address = gsub("(\\d)(\\D)", "\\1 \\2", single_line_address),
    single_line_address = gsub("[,.();:#'']", " ", single_line_address),
    single_line_address = stringr::str_squish(single_line_address),
    single_line_address = ifelse(
      grepl("[0-9] - [0-9]", single_line_address) == T,
      gsub(" - ", "-", single_line_address),
      single_line_address
    ),
    cqc_date = download_date
  ) %>% 
  select(
    uprn,
    location_id,
    registration_date,
    deregistration_date,
    single_line_address,
    postcode,
    nursing_home_flag = nursing_home,
    residential_home_flag = residential_home,
    type,
    number_of_beds,
    cqc_date
  ) %>% 
  rename_with(toupper)



# Create table name
table_name = paste0("INT646_CQC_", download_date)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Upload to DB with indexes
con %>%
  copy_to(
    df = cqc_process_df,
    name = table_name,
    indexes = list(c("LOCATION_ID"), c("UPRN"), c("POSTCODE")),
    temporary = FALSE
  )

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Clear environment and clean
rm(list = ls()); gc()
