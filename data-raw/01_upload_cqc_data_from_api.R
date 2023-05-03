
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
cqc_details_reduced <- cqc_details %>%
  map(
    ~ .x %>% 
      select(
        any_of(
          c(
            "locationId",
            "providerId",
            "type",
            "name",
            "ods_code",
            "uprn",
            "registrationDate",
            "deregistrationDate",
            "numberOfBeds",
            "localAuthority",
            "lastInspection.date",
            "gacServiceTypes.name"
          )
        ),
        starts_with("postal"),
        starts_with("onspd"),
        starts_with("relationships"),
        starts_with("regulatedActivities.name"),
        starts_with("regulatedActivities.code"),
        starts_with("specialisms"),
        starts_with("currentRatings.overall.rating")#,
        # starts_with("currentRatings.service") # could take this too, but adds 14 cols
      )
  )

# Bind all dfs together and apply some transformations
cqc_details_df <- cqc_details_reduced %>%
  bind_rows() %>% 
  janitor::clean_names() %>%
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
  unite_to_plural(
    specialisms,
    regulated_activities_names,
    regulated_activities_codes,
    relationships_related_location_ids,
    relationships_related_location_names,
    relationships_types,
    relationships_reasons
  )
  
gc()

# Get current year_month
download_date = as.integer(gsub('-', '', Sys.Date()))

# Process the cqc df output, in preparation for matching
cqc_process_df = cqc_details_df %>% 
  rename(
    postcode = postal_code,
    current_rating = current_ratings_overall_rating,
    nursing_home_flag = nursing_home,
    residential_home_flag = residential_home,
    ccg_code = onspd_ccg_code,
    ccg_name = onspd_ccg_name,
    icb_code = onspd_icb_code,
    icb_name = onspd_icb_name,
    latitude = onspd_latitude,
    longitude = onspd_longitude
  ) %>% 
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
    cqc_date = download_date,
    specialisms = gsub("Caring for ", "", specialisms),
    regulated_activities_names = gsub(
      "Accommodation for ", "", regulated_activities_names
    )
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
    cqc_date,
    provider_id,
    name,
    local_authority,
    last_inspection_date,
    ccg_code,
    ccg_name,
    icb_code,
    icb_name,
    latitude,
    longitude,
    specialisms,
    regulated_activities_names,
    regulated_activities_codes,
    relationships_related_location_ids,
    relationships_related_location_names,
    relationships_types,
    relationships_reasons
  ) %>% 
  rename_with(toupper)

# Check na count per column
print("NA count per column")
print(colSums(is.na(cqc_process_df)))

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

# Grant access
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MIGAR"))
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO ADNSH"))
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MAMCP"))

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
