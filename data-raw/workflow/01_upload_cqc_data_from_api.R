
# Functions and variables ------------------------------------------------------

# Get cqc primary key from environ file
key = Sys.getenv("CQC_PRIMARY_KEY")

# Number of results per page in API calls
api_perPage = 1000

# Get current year_month
download_date <- as.integer(format(today(), "%Y%m%d"))

# Define cqc columns of interest 
cqc_cols = c(
  'name', 
  'postalCode',
  'uprn',
  'locationId',
  'providerId',
  'organisationType',
  'type',
  'lastInspection',
  'deregistrationDate',
  'registrationStatus',
  'registrationDate',
  'postalAddressLine1', 
  'postalAddressLine2',
  'postalAddressTownCity',
  'postalAddressCounty',
  'numberOfBeds',
  'gacServiceTypes',
  'specialisms',
  'currentRatings',
  'odsCode'
  )

# Define provider cols
provider_cols = c(
  "providerId",
  "name",
  "postalAddressLine1",
  "postalAddressLine2",
  "postalAddressTownCity",
  "postalAddressCounty",
  "region",
  "postalCode",
  "uprn",
  "companiesHouseNumber",
  "lastInspection"
  )

# Function to get api content from url
get_api_content <- function(url){
  
  # Get api data
  data = httr::GET(url, httr::add_headers(`Ocp-Apim-Subscription-Key` = key))
  
  # Convert binary to character
  content = jsonlite::fromJSON(rawToChar(data$content))
  
  # Return content
  return(content)
}

# Get number of pages
get_number_of_pages = function(){
  
  # Define url
  url = "https://api.service.cqc.org.uk/public/v1/locations?careHome=Y"
  
  # Get locations overview
  api_content = get_api_content(url)
  
  # Get number of pages
  total_pages = api_content$totalPages
  
  # Return
  return(total_pages)
}

# Get all locations per page
get_location_ids_per_page = function(page_num){
  
  # Define url
  url = paste0(
    "https://api.service.cqc.org.uk/public/v1/locations?careHome=Y&page=",
    page_num,
    "&perPage=",
    api_perPage
    )
  
  # Get locations overview
  api_content = get_api_content(url)
  
  # Get locations ids
  location_vec = api_content$locations$locationId
  
  # Return
  return(location_vec)
}

# Get all locations by location_vec index
get_location_info_by_id <- function(loc_num) {
  
  # Paste location url with location_id
  url = paste0(
    "https://api.service.cqc.org.uk/public/v1/locations/", 
    location_vec[loc_num]
  )
  
  # Get data
  data = get_api_content(url)

  # Filter data 
  filtered_data = data[names(data) %in% cqc_cols]
  
  # Flat data
  flat_data = filtered_data %>% 
    unlist() %>% 
    bind_rows() %>% 
    janitor::clean_names()
  
  # Return data
  return(flat_data)
}

# Function to query cqc api
get_provider_info_by_id = function(loc_num){
  
  # Paste location url with location_id
  url = paste0(
    "https://api.service.cqc.org.uk/public/v1/providers/", 
    provider_vec[loc_num]
  )
  
  # Get data
  data = get_api_content(url)
  
  # Filter data 
  filtered_data = data[names(data) %in% provider_cols]
  
  # Flat data
  flat_data = filtered_data %>% 
    unlist() %>% 
    bind_rows() %>% 
    janitor::clean_names()
  
  # Return data
  return(flat_data)
}

# Concatenate all columns with same prefix (optionally uniquely and without NA)
concatenate_by_prefix = function(df, prefix, .unique = TRUE, na.rm = TRUE){
  maybe_na.rm <- if(na.rm) na.omit else \(x) x
  maybe_unique <- if(.unique) unique else \(x) x
  
  before_collapse <- \(x) maybe_unique(maybe_na.rm(x))
  
  df %>% 
    mutate({{ prefix }} := pmap_chr(
      select(., starts_with(prefix)), 
      ~ paste(before_collapse(c(...)), collapse = "|")
      ))
}

# Generate location output -----------------------------------------------------

# Get total pages
total_pages = get_number_of_pages()

# Get all location ids
location_vec = lapply(1:total_pages, get_location_ids_per_page)

# Unlist into a single vector
location_vec = unlist(location_vec)

# Generate appropriate number of cores
n_cores <- parallel::detectCores() - 1

# Set up parallel
clust <- parallel::makeCluster(n_cores)

# Export libraries to cluster
parallel::clusterEvalQ(
  cl = clust,
  {
    library(dplyr); 
    library(janitor);
    library(httr);
    library(jsonlite);
  }
)

# Export required objects to cluster
parallel::clusterExport(
  cl = clust,
  varlist = c(
    "get_api_content",
    "location_vec",
    "key",
    "cqc_cols"
  ),
  envir = environment()
)

# Generate cqc details: ~25 mins
cqc_data <- parallel::parLapply(
  cl = clust, 
  X = 1:length(location_vec), 
  fun = get_location_info_by_id
)

# Stop Cluster
parallel::stopCluster(clust)

# Format location output -------------------------------------------------------

# Bind all dfs together and clean column names
cqc_details_df <- cqc_data %>%
  bind_rows() %>%
  # Expect total locations to be at least the number on 'full' pages
  verify(nrow(.) > ((total_pages - 1) * api_perPage)) %>% 
  janitor::clean_names() %>% 
  # Expect location_id to be unique
  assert.alt(is_uniq.alt, location_id) %>% 
  concatenate_by_prefix('specialisms') %>% 
  concatenate_by_prefix('current_ratings_overall_key_question_ratings_name') %>% 
  # Want each entry to be kept, as multiple ratings may be the same value, or missing
  concatenate_by_prefix(
    'current_ratings_overall_key_question_ratings_rating',
    .unique = FALSE,
    na.rm = FALSE
  ) %>% 
  concatenate_by_prefix('gac_service_types_name') %>% 
  tidyr::unite(
    single_line_address,
    c(
      name, 
      postal_address_line1, 
      postal_address_line2,
      postal_address_town_city,
      postal_address_county
    ),
    sep = " ",
    na.rm = TRUE
  ) %>% 
  mutate(
    uprn = as.numeric(uprn),
    location_id,
    provider_id,
    last_inspection_date,
    registration_date,
    deregistration_date,
    single_line_address,
    postcode = toupper(gsub("[^[:alnum:]]", "", postal_code)),
    nursing_home_flag = as.integer(grepl(
      "Nursing home", gac_service_types_name
    )),
    residential_home_flag = as.integer(grepl(
      "Residential home", gac_service_types_name
    )),
    number_of_beds = as.integer(number_of_beds),
    current_rating = current_ratings_overall_rating,
    # When blank, all values were NA ...
    key_question_names = case_when(
      current_ratings_overall_key_question_ratings_name == "" ~ NA,
      TRUE ~ current_ratings_overall_key_question_ratings_name
    ),
    # When key_question_names, created just before this, is NA, make this NA also
    key_question_ratings = case_when(
      is.na(key_question_names) ~ NA,
      TRUE ~ current_ratings_overall_key_question_ratings_rating
    ),
    cqc_date = download_date,
    ods_code,
    specialisms,
    gac_service_types = gac_service_types_name,
    .keep = "none"
  ) %>%
  addressMatchR::tidy_single_line_address_df(single_line_address)

# Generate provider output -----------------------------------------------------

# Get provider id vec
provider_vec = cqc_details_df %>% 
  select(provider_id) %>% 
  distinct() %>% 
  pull()

# Generate appropriate number of cores
n_cores <- parallel::detectCores() - 1

# Set up parallel
clust <- parallel::makeCluster(n_cores)

# Export libraries to cluster
parallel::clusterEvalQ(
  cl = clust,
  {
    library(dplyr); 
    library(janitor);
    library(httr);
    library(jsonlite);
  }
)

# Export required objects to cluster
parallel::clusterExport(
  cl = clust,
  varlist = c(
    "get_api_content",
    "provider_vec",
    "key",
    "provider_cols"
  ),
  envir = environment()
)

# Generate provider details: ~15 mins
provider_data <- parallel::parLapply(
  cl = clust, 
  X = 1:length(provider_vec), 
  fun = get_provider_info_by_id
)

# Stop Cluster
parallel::stopCluster(clust)

# Process provider output ------------------------------------------------------

# Process the provider data
cqc_providers_df = provider_data %>% 
  bind_rows() %>%
  # Expect total providers to be same as length of vector of provider ids
  verify(nrow(.) == length(provider_vec)) %>% 
  janitor::clean_names() %>% 
  # Expect provider_id to be unique
  assert.alt(is_uniq.alt, provider_id) %>% 
  tidyr::unite(
    provider_sla,
    c(
      name, 
      postal_address_line1, 
      postal_address_line2,
      postal_address_town_city,
      postal_address_county
    ),
    sep = " ",
    na.rm = TRUE
  ) %>% 
  mutate(postal_code = toupper(gsub("[^[:alnum:]]", "", postal_code))) %>% 
  select(
    provider_id, 
    provider_uprn = uprn,
    provider_companies_house_number = companies_house_number,
    provider_sla,
    provider_postcode = postal_code,
    provider_last_inspection_date = last_inspection_date
  ) %>% 
  addressMatchR::tidy_single_line_address_df(provider_sla)

# Join location and provider data then save -----------------------------------

# Process the cqc df output, in preparation for matching
cqc_final_df <- cqc_details_df %>% 
  left_join(cqc_providers_df, by = "provider_id") %>% 
  rename_with(toupper)

# Create table name
table_name <- paste0("INT646_CQC_", download_date)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Upload to DB
cqc_final_df %>% write_table_long_chars(con, table_name)

# Add indexes
con %>% add_indexes(table_name, c("UPRN", "POSTCODE"))

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% grant_table_access (table_name)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
