# Function to get api content from url
get_api_content <- function(url) {

  # Get api data
  data = httr::GET(url)

  # Convert binary to character
  content = jsonlite::fromJSON(rawToChar(data$content))

  # Return content
  return(content)
}

# Get number of cqc pages for main api query
api_content <- get_api_content(
  "https://api.cqc.org.uk/public/v1/locations?careHome=Y&page=1&perPage=1"
)

# Get number of 10k blocks required
no_of_pages = ceiling(api_content$total / 10000)

get_cqc_locations_details <- function(page_num) {
  
  # Url with page number pasted inside
  url = paste0(
    "https://api.cqc.org.uk/public/v1/locations?careHome=Y&page=", 
    page_num, 
    "&perPage=10000"
  )
  
  # Get api data
  data = get_api_content(url)
  
  # Get location info as df within list
  locations = data$locations
  
  # Return location info df
  return(locations)
}

# Get all location info, with 10k records per page retrieved
cqc_locations <- lapply(1:no_of_pages, get_cqc_locations_details) %>% 
  bind_rows()

# Vector of locations
location_vec = cqc_locations %>% pull(locationId)

# Function to query cqc api
get_cqc_api_location_data <- function(loc_num) {
  
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

# Generate appropriate number of cores
n_cores <- parallel::detectCores() - 2

# Set up parallel
clust <- parallel::makeCluster(n_cores)

# Export libraries to cluster
parallel::clusterEvalQ(
  cl = clust,
  {
    library(dplyr); 
    library(httr);
    library(jsonlite);
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
print("Now downloading CQC API location data ...")

# Generate cqc details
cqc_details <- parallel::parLapply(
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
  select(
    location_id,
    provider_id,
    uprn,
    registration_date,
    deregistration_date,
    last_inspection_date,
    name,
    postal_address_line1,
    postal_address_line2,
    postal_address_town_city,
    postal_address_county,
    postal_code,
    type,
    number_of_beds,
    current_rating = current_ratings_overall_rating,
    starts_with("gac_service_types_name")
  ) %>% 
  mutate(
    # Add the nursing home / residential home flag
    nursing_home_flag = if_else(
      if_any(
        .cols = starts_with("gac"),
        .fns = ~ grepl(pattern = "Nursing home", x = .x)
      ),
      1L,
      0L
    ),
    residential_home_flag = if_else(
      if_any(
        .cols = starts_with("gac"),
        .fns = ~ grepl(pattern = "Residential home", x = .x)
      ),
      1L,
      0L
    ),
    # Change type of numeric col
    number_of_beds = as.integer(number_of_beds)
  ) %>% 
  select(-starts_with("gac_service_types_name")); gc()

# Get provider id vec
provider_vec = cqc_details_df %>% 
  select(provider_id) %>% 
  distinct() %>% 
  pull()

# NOTE: provider info doesn't work in parallel due to cqc api
get_cqc_api_provider_data = function(loc_num){
  
  # Wait
  Sys.sleep(0.05)
  
  # Paste location url with location_id
  url = paste0(
    "https://api.cqc.org.uk/public/v1/providers/", 
    provider_vec[loc_num]
  )
  
  # Get data
  data = get_api_content(url) %>% 
    unlist() %>% 
    bind_rows()
  
  # Return data
  return(data)
}

# Print script update
print("Now downloading CQC API provider data ...")

# With sys.sleep takes ~30 mins
cqc_details = lapply(
  X = 1:length(provider_vec), 
  FUN = get_cqc_api_provider_data
)

# Process the provider data
cqc_providers_df = cqc_details %>% 
  bind_rows() %>% 
  janitor::clean_names() %>% 
  mutate(
    # Paste fields together to create single line address
    provider_sla = toupper(paste(
      ifelse(is.na(name), "", name),
      ifelse(is.na(postal_address_line1), "", postal_address_line1),
      ifelse(is.na(postal_address_town_city), "", postal_address_town_city),
      ifelse(is.na(region), "", region)
    )),
    # Postcode Cleaning
    postal_code = toupper(gsub("[^[:alnum:]]", "", postal_code))
  ) %>% 
  select(
    provider_id, 
    provider_uprn = uprn,
    provider_companies_house_number = companies_house_number,
    provider_sla,
    provider_postcode = postal_code,
    provider_last_inspection_date = last_inspection_date
  )

# Get current year_month
download_date <- as.integer(format(today(), "%Y%m%d"))

# Process the cqc df output, in preparation for matching
cqc_process_df <- cqc_details_df %>% 
  rename(postcode = postal_code) %>%
  # Paste fields together to create single line address
  tidyr::unite(
    single_line_address,
    c(
      "name", 
      "postal_address_line1", 
      "postal_address_line2",
      "postal_address_town_city",
      "postal_address_county"
    ),
    sep = " ",
    na.rm = TRUE
  ) %>% 
  mutate(
    # Postcode Cleaning
    postcode = toupper(gsub("[^[:alnum:]]", "", postcode)),
    cqc_date = download_date
  ) %>% 
  select(
    uprn,
    location_id,
    provider_id,
    last_inspection_date,
    registration_date,
    deregistration_date,
    single_line_address,
    postcode,
    nursing_home_flag,
    residential_home_flag,
    type,
    number_of_beds,
    current_rating,
    cqc_date
  ) %>% 
  left_join(cqc_providers_df, by = "provider_id") %>% 
  tidy_df_single_line_address(single_line_address) %>% 
  tidy_df_single_line_address(provider_sla) %>% 
  rename_with(toupper)

# Check na count per column
print("NA count per column")
print(colSums(is.na(cqc_process_df)))

# Create table name
table_name <- paste0("INT646_CQC_", download_date)

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

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
