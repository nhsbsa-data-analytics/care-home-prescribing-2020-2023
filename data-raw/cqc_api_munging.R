
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

check_specs <- cqc_process_df %>%
  select(REGULATED_ACTIVITIES_NAMES) %>%
  unique() %>%
  mutate(
    REGULATED_ACTIVITIES_NAMES = gsub("Accomodation for ", "", REGULATED_ACTIVITIES_NAMES),
    char_count = nchar(REGULATED_ACTIVITIES_NAMES)
  )



################### Script 3

ab_plus_data = "INT646_ABP_20220422"
cqc_data = "INT646_CQC_20230504"
start_date = "2021-04-01"
end_date = "2022-03-31"

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = cqc_data)

# Create a lazy table addressbase data
ab_plus_db <- con %>%
  tbl(from = ab_plus_data)

# Get 4 values for final output
ab_epoch = pull_date_string(ab_plus_db, EPOCH)
cqc_date = pull_date_string(cqc_db, CQC_DATE)

# Part one: Process cqc data ---------------------------------------------------

# Distinct SLA and postcode per uprn and location-id 

# CAVEAT: A relatively small number of entries (264) were removed. We only kept
# the latest record for each combination of postcode and single line address.
# Latest is determined by latest inspection if it exists, or registration date
# if it does not. Of these removed entries, 11 individual care homes had changed
# their type from residential to nursing home, or vice versa.
cqc_db_raw <- cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD"),
    LAST_INSPECTION_DATE = TO_DATE(LAST_INSPECTION_DATE, "YYYY-MM-DD"),
    CH_FLAG = 1L
  ) %>% 
  filter(
    #!is.na(UPRN),  # Do not exclude records with null UPRNs, as these will be used for CH/non-CH level analysis
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  mutate(ROW_ID = rank(LOCATION_ID))

cqc_db_trimmed <- cqc_db_raw %>% 
  mutate(TEMP_DECIDER = coalesce(LAST_INSPECTION_DATE, REGISTRATION_DATE)) %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  slice_max(
    TEMP_DECIDER,
    with_ties = FALSE
  ) %>% 
  ungroup() %>% 
  select(-TEMP_DECIDER)

# Validation and checking

## Get all removed entries. Count is 264.
cqc_db_removed <- cqc_db_raw %>% 
  anti_join(
    cqc_db_trimmed,
    by = c("ROW_ID")
  )

removed_count = cqc_db_removed %>% collect() %>% nrow()

print(glue("Number of entries removed: {removed_count}"))
  
## Get all entries for which the postcode/SLA combination is among the removed
## rows. This is simply for sanity checking by visual inspection.
cqc_db_trimmed_check <- cqc_db_trimmed %>% 
  transmute(
    POSTCODE,
    SINGLE_LINE_ADDRESS,
    REGISTRATION_DATE,
    DEREGISTRATION_DATE,
    LAST_INSPECTION_DATE,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    CURRENT_RATING
  ) %>%
  collect()

cqc_db_removed_check <- cqc_db_removed %>% 
  transmute(
    POSTCODE,
    SINGLE_LINE_ADDRESS,
    REGISTRATION_DATE,
    DEREGISTRATION_DATE,
    LAST_INSPECTION_DATE,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG,
    CURRENT_RATING
  ) %>%
  collect()

cqc_db_check <- cqc_db_removed_check %>% 
  left_join(
    cqc_db_trimmed_check,
    by = c("POSTCODE", "SINGLE_LINE_ADDRESS"),
    suffix = c(".REMOVED", ".KEPT")
  )

cqc_db_check <- cqc_db_check %>%
  pivot_longer(
    cols = c(ends_with(".REMOVED"), ends_with(".KEPT")),
    names_to = c(".value", "STATUS"),
    names_pattern = "(.*)\\.(.*)"
  ) %>% 
  arrange(POSTCODE, SINGLE_LINE_ADDRESS, STATUS)

## Find care homes which changed type - count is 11
cqc_changed_type <- cqc_db_raw %>%
  mutate(
    NH = if_else(NURSING_HOME_FLAG == 1, "NH", NA_character_),
    RH = if_else(RESIDENTIAL_HOME_FLAG == 1, "RH", NA_character_),
  ) %>% 
  collect() %>% 
  unite(HOME_TYPE, NH, RH, sep = " ", na.rm = TRUE) %>% 
  select(
    POSTCODE,
    SINGLE_LINE_ADDRESS,
    HOME_TYPE,
    NURSING_HOME_FLAG,
    RESIDENTIAL_HOME_FLAG
  ) %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS, HOME_TYPE) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  filter(n > 1) %>% 
  select(-n)

changed_type_count = cqc_changed_type %>% collect() %>% nrow()

print(glue("Number of homes changing type: {cqc_changed_type}"))

##################

cqc_db_trimmed_grouped <- cqc_db_trimmed %>% 
  add_count(POSTCODE, SINGLE_LINE_ADDRESS) %>% 
  as_tibble()



cqc_changed_types_only <- cqc_changed_types1 %>% 
  inner_join(
    cqc_db,
    by = c("POSTCODE", "SINGLE_LINE_ADDRESS")
  ) %>% 
  slice_max(REGISTRATION_DATE, by = c(POSTCODE, SINGLE_LINE_ADDRESS))

cqc_db1 <- cqc_db %>% 
  anti_join(
    cqc_changed_types_only1,
    by = c("POSTCODE", "SINGLE_LINE_ADDRESS")
  ) %>% 
  union_all(cqc_changed_types_only1)

cqc_non_uniform_ratings1 <- cqc_db1 %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS, CURRENT_RATING) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  filter(n > 1) %>% 
  select(-n)

cqc_non_uniform_ratings_only1 <- cqc_non_uniform_ratings1 %>% 
  inner_join(
    cqc_db1,
    by = c("POSTCODE", "SINGLE_LINE_ADDRESS")
  ) %>%
  slice_max(LAST_INSPECTION_DATE, by = c(POSTCODE, SINGLE_LINE_ADDRESS))

cqc_db1 <- cqc_db1 %>% 
  anti_join(
    cqc_non_uniform_ratings_only1,
    by = c("POSTCODE", "SINGLE_LINE_ADDRESS")
  ) %>% 
  union_all(cqc_non_uniform_ratings_only1)

########

cqc_non_uniform_ratings2 <- cqc_db1 %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS, CURRENT_RATING) %>%
  summarise(n = n()) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  filter(n > 1) %>% 
  select(-n)

cqc_non_uniform_ratings_only2 <- cqc_non_uniform_ratings2 %>% 
  inner_join(
    cqc_db,
    by = c("POSTCODE", "SINGLE_LINE_ADDRESS")
  ) %>%
  slice_max(LAST_INSPECTION_DATE, by = c(POSTCODE, SINGLE_LINE_ADDRESS))

cqc_db2 <- cqc_db1 %>% 
  anti_join(
    cqc_non_uniform_ratings_only2,
    by = c("POSTCODE", "SINGLE_LINE_ADDRESS")
  ) %>% 
  union_all(cqc_non_uniform_ratings_only2)

missed_ratings <- cqc_non_uniform_ratings2 %>% 
  left_join(cqc_db2)
