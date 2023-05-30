# Load/install all required packages and functions
source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")

# Table names
patient_address_data = "INT646_FORMS_20210401_20220331"
lookup_address_data = "INT646_ABP_CQC_20210401_20220331"

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level FACT table
patient_db <- con %>%
  tbl(from = patient_address_data)

# Create a lazy table from the AddressBase Plus and CQC care home table
address_db <- con %>%
  tbl(from = lookup_address_data) %>% 
  rename(AB_FLAG = CH_FLAG)

# Process and match address data -----------------------------------------------

# Get distinct patient-level address-postcode information
patient_address_db = patient_db %>% 
  # If the address is NA we don't want to consider it
  filter(
    !is.na(SINGLE_LINE_ADDRESS),
    CALC_AGE >= 65,
    POSTCODE_CH == 1
  ) %>%
  # Add monthly patient count
  group_by(YEAR_MONTH, POSTCODE, SINGLE_LINE_ADDRESS) %>%
  mutate(MONTHLY_PATIENTS = n_distinct(NHS_NO)) %>%
  ungroup() %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  # Get max monthly patient count
  summarise(MAX_MONTHLY_PATIENTS = max(MONTHLY_PATIENTS, na.rm = TRUE)) %>% 
  ungroup()

# Match the patients address to the AddressBase Plus and CQC care home addresses
match_db = addressMatchR::calc_match_addresses(
  primary_df = patient_address_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "SINGLE_LINE_ADDRESS",
  lookup_df = address_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SINGLE_LINE_ADDRESS"
)

# Function to compare create table speed ---------------------------------------

# Function to create table from query with specified degree of parallelism
compute_with_parallelism = function(lazy_tbl, create_table_name, n){
  
  # Pull the DB connection
  db_connection <- lazy_tbl$src$con
  
  # Render the sql query as text
  query = dbplyr::sql_render(lazy_tbl)
  
  # Modify query text
  new_query = paste0(
    "CREATE TABLE ", create_table_name, " AS SELECT /*+ PARALLEL(", n, ") */ * FROM ", query
  )
  
  # Send query to the database
  DBI::dbSendQuery(conn = db_connection, statement = new_query)
}

# Test time taken for function to run with 2 parallel: 1623 secs ( mins)
tic(); compute_with_parallelism(match_db, "INT646_ABC", 2); toc()

# Drop table and try again
drop_table_if_exists_db("INT646_ABC")

# Test time taken for function to run with 16 parallel: 1419 secs (24 mins)
tic(); compute_with_parallelism(match_db, "INT646_ABC", 16); toc()

# Conclusion: the code runs with the intended of parallelism, as intended
# However, the lack of performance in this instance are perhaps use case specific
# A different create table statment with varying parallel may have greater gains

# Disconnect and clear
dbDisconnect(con); rm(list = ls()); gc()
