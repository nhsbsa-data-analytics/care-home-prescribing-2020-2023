
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
  bind_rows()

cqc_details_df %>% saveRDS("data-raw/cqc_details_df_raw.rds")

cqc_details_names <- cqc_details_df %>%
  names() %>%
  tibble() %>% 
  rename(name = 1) %>% 
  mutate(name = stringr::str_replace_all(name, "[:digit:]|\\.\\.\\.", "")) %>% 
  distinct()

names_missing <- tibble(
  locationId = character(),
  names_missing_count = integer()
)

for (entry in cqc_details) {
  entry_names <- entry %>% 
    names() %>%
    tibble() %>% 
    rename(name = 1) %>% 
    mutate(name = stringr::str_replace_all(name, "[:digit:]|\\.\\.\\.", "")) %>% 
    distinct()
  
  names_not_in_entry <- setdiff(
    cqc_details_names %>% pull(),
    entry_names %>% pull()
  )
  
  names_missing_entry <- tibble(
    locationId = c(entry$locationId),
    names_missing_count = length(names_not_in_entry)
  )
  
  names_missing <- names_missing %>% 
    bind_rows(names_missing_entry)
}

most_names_ch <- names_missing %>% 
  arrange(names_missing_count) %>% 
  head(1) %>% 
  pull(locationId)

one_ch <- cqc_details[[which(cqc_details_df$locationId == most_names_ch)]]

library(purrr)

tic()
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
        starts_with("currentRatings.overall.rating"),
        starts_with("currentRatings.service")
      )
  )
toc()

tic()
cqc_details_df <- cqc_details_reduced %>%
  head(1000) %>% 
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
  unite(
    "specialisms", starts_with("specialisms"),
    sep = ",", na.rm = TRUE
  ) %>%
  unite(
    "regulated_activities_names", starts_with("regulated_activities_name"),
    sep = ",", na.rm = TRUE
  ) %>%
  unite(
    "regulated_activities_codes", starts_with("regulated_activities_code"),
    sep = ",", na.rm = TRUE
  ) %>%
  unite(
    "relationships_related_location_ids", starts_with("relationships_related_location_id"),
    sep = ",", na.rm = TRUE
  ) %>%
  unite(
    "relationships_related_location_names", starts_with("relationships_related_location_name"),
    sep = ",", na.rm = TRUE
  ) %>%
  unite(
    "relationships_types", starts_with("relationships_type"),
    sep = ",", na.rm = TRUE
  ) %>%
  unite(
    "relationships_reasons", starts_with("relationships_reason"),
    sep = ",", na.rm = TRUE
  )
toc()

unite_to_plural <- function(data, new_col) {
  data <- tidyr::unite(
    data,
    new_col,
    starts_with(substr(new_col, 1, nchar(new_col) - 1)),
    sep = ",",
    na.rm = TRUE
  )
}
