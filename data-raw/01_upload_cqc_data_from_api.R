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

# List for results
location_list = list()

# Loop through location_ids
for (i in 1:length(location_vec)) {
  
  # Record the start time
  start <- Sys.time()
  print(i)
  
  # Get the batch results and append them to the existing ones
  location_list[[i]] = cqcr::cqc_location(location_vec[i]) %>% 
    unlist() %>% 
    bind_rows()
  
  # Pause for the remainder of just over a second
  Sys.sleep(max(0, 0.1 - as.numeric(Sys.time() - start)))
}
















cqc_locations_dfs <- split(cqc_locations_df, seq(nrow(cqc_locations_df)) %/% 10)
cqc_batch_details <- list()

for (batch in cqc_locations_dfs) {
  
  # Record the start time
  start <- Sys.time()
  
  # Get the batch results and append them to the existing ones
  cqc_batch_details <- c(cqc_batch_details, cqcr::cqc_location_details(batch))
  
  # Pause for the remainder of just over a second
  Sys.sleep(max(0, 1.1 - as.numeric(Sys.time() - start)))
}

cqc_batch_details <- list()

a=cqc_locations_dfs[[1]]

cqcr::cqc_location(a)
cqcr::cqc_location_inspection_area(a)
cqcr::cqc_locations_search(a)

A = cqcr::cqc_location('1-10000302982') 















# Process CQC data and write to DB

# Convert the batch results into a dataframe
cqc_details_df <- purrr::map_df(
  .x = cqc_batch_details,
  .f = ~ bind_rows(unlist(x = .x))
)

# For care homes project we are only interested in a subset of columns, so lets
# extract them
cqc_details_df <- cqc_details_df %>%
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
    postal_address_line_1,
    postal_address_line_2,
    postal_address_town_city,
    postal_address_county,
    postal_code,
    nursing_home,
    residential_home,
    type,
    number_of_beds
  ) %>%
  rename_with(toupper)

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
