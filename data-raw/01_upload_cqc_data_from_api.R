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
api_content = get_api_content(
  "https://api.cqc.org.uk/public/v1/locations?careHome=Y&page=1&perPage=1"
)

# Get number of 10k blocks required
no_of_pages = ceiling(api_content$total / 10000)

# Set a partner code (if we don't set this then we struggle to throttle calls)
# MMc: not sure what code this is?
get_cqc_locations_details = function(page_num){
  
  # Url with page number pasted inside
  url = glue(
    "https://api.cqc.org.uk/public/v1/locations?careHome=Y&page={page_num}&perPage=10000", 
  )
  
  # Get api data
  data = get_api_content(url)
  
  # Get location info as df within list
  locations = data$locations
  
  # Return location info df
  return(locations)
}

# Get all location info, with 10k records per page retrieved
cqc_locations = lapply(1:no_of_pages, get_cqc_locations_details) %>% 
  bind_rows()

# Vector of locations
location_vec = cqc_locations %>% pull(locationId)

# Function to query cqc api
get_cqc_api_location_data = function(loc_num){
  
  # Paste location url with location_id
  url = glue("https://api.cqc.org.uk/public/v1/locations/{location_vec[loc_num]}")
  
  # Get data
  data = get_api_content(url) %>% 
    unlist() %>% 
    bind_rows()
  
  # Return data
  return(data)
}

# Generate appropriate number of cores
n_cores = parallel::detectCores() - 2

# Set up parallel
clust = parallel::makeCluster(n_cores)

# Export libraries to cluster
parallel::clusterEvalQ(
  cl = clust,
  {
    library(dplyr); 
    library(httr);
    library(jsonlite);
    library(glue);
  }
)

# Export required objects to cluster
parallel::clusterExport(
  cl = clust,
  varlist = c(
    "get_api_content",
    "get_cqc_api_location_data",
    "location_vec"
  )#,
  # MMc: not sure why envir is specified, it evaluates to the default of 
  # .GlobalEnv anyway?
  #envir = environment()
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
  # MMc: what rules do you use for when to namespace functions with ::?
  # In this case I can see janitor is only used here, so was not loaded.
  # But I notice that parallel functions are all namespaced, even tho loaded in
  # original script (in my modified version this is needed as I took out parallel,
  # as noted in analysis_packages.R)
  janitor::clean_names() %>%
  # Select the required cols
  # MMc: I moved the selection here to help me understand the data of interest, 
  # as there were 1000+ columns not used
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
    type,
    number_of_beds,
    current_rating = current_ratings_overall_rating,
    starts_with("gac_service_types_name")
  ) %>% 
  # Filter to the period of interest
  # MMc: out of date comment, no date filtering here?
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

# Get current year_month
download_date = as.integer(format(today(), "%Y%m%d"))

# Process the cqc df output, in preparation for matching
cqc_process_df = cqc_details_df %>% 
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
    # Address cleaning
    single_line_address = gsub(" & ", "and", single_line_address),
    single_line_address = gsub("(\\D)(\\d)", "\\1 \\2", single_line_address),
    single_line_address = gsub("(\\d)(\\D)", "\\1 \\2", single_line_address),
    single_line_address = gsub("[,.();:#'']", " ", single_line_address),
    single_line_address = stringr::str_squish(single_line_address),
    single_line_address = gsub(
      "(?<=\\d)\\s-\\s(?=\\d)", "-",
      single_line_address,
      perl = T
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
    nursing_home_flag,
    residential_home_flag,
    type,
    number_of_beds,
    current_rating,
    cqc_date
  ) %>% 
  rename_with(toupper)

# Check na count per column
print("NA count per column")
print(colSums(is.na(cqc_process_df)))

# Create table name
table_name = glue("INT646_CQC_{download_date}")

# Set up connection to the DB
# MMc: Another example of namespacing a loaded package
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
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MIGAR"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO ADNSH"))
DBI::dbExecute(con, glue("GRANT SELECT ON {table_name} TO MAMCP"))

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(glue("This script has created table: {table_name}"))

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
