source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")

# Identify closest dall_ref release date ---------------------------------------

# Define end date
end_date = "2024-03-31"

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Connect to ab plus in dall_ref
ab <- con %>%
  tbl(from = in_schema("DALL_REF", "ADDRESSBASE_PLUS"))

# Get closest release date
select_date = ab %>% 
  select(RELEASE_DATE) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(
    SELECT_DATE = as.Date(end_date),
    RELEASE_DATE = as.Date(RELEASE_DATE),
    DIFF = as.integer(abs(RELEASE_DATE - SELECT_DATE)),
    DB_DATE = as.integer(gsub("-", "", SELECT_DATE))
    ) %>% 
  filter(DIFF == min(DIFF)) %>% 
  select(DB_DATE) %>% 
  pull()

# Filter ab by most appropriate release date
ab = ab %>% filter(TO_NUMBER(TO_CHAR(RELEASE_DATE, 'YYYYMMDD')) == db_date)

# Temp table just with cleaned geo and dpa sla ---------------------------------

# Define temp table name
table_name_temp = "TEMP_AB_PLUS"

# Check if temp table exists
drop_table_if_exists_db(table_name_temp)

# Sla generation and clean
ab_sla = ab %>% 
  # Create CH flag
  mutate(CH_FLAG = ifelse(CLASS == "RI01", 1L, 0L)) %>% 
  # SLA creation plus formatting
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>% 
  select(
    UPRN,
    PARENT_UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS,
    CH_FLAG,
    RELEASE_DATE
  ) 

# Save intermediate table under temp table name
ab_sla %>%
  compute(
    name = table_name_temp,
    temporary = FALSE
  )

# Re-connect to temp table and finish processing -------------------------------

# Connect and process 
ab_plus = con %>%
  tbl(from = table_name_temp) %>%
  nhsbsaR::oracle_merge_strings(
    first_col = "DPA_SINGLE_LINE_ADDRESS",
    second_col = "GEO_SINGLE_LINE_ADDRESS",
    merge_col = "CORE_SINGLE_LINE_ADDRESS"
  ) %>% 
  select(
    UPRN,
    PARENT_UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS,
    CORE_SINGLE_LINE_ADDRESS,
    CH_FLAG,
    RELEASE_DATE
  ) 

# Define table name
table_name = paste0("ADDRESSBASE_PLUS_", db_date)

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Write the table back to the DB with indexes
ab_plus %>%
  compute(
    name = table_name,
    temporary = FALSE,
    indexes = c("UPRN", "PARENT_UPRN", "POSTCODE")
  )

# Drop intermediate tables now the final table is done
drop_table_if_exists_db(table_name_temp)

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% grant_table_access (table_name)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Return to project directory
setwd(project_dir)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
