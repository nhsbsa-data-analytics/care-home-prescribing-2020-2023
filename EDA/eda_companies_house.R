
# Load/install all required packages and functions
source("R/analysis_packages.R")
source("R/workflow_helpers.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# 1.1. Bulk download of companies house data -----------------------------------

# Get project directory
project_dir = getwd()

# Define temp dir
output_dir = tempdir()

# Download URL
url = "http://download.companieshouse.gov.uk/BasicCompanyDataAsOneFile-2023-04-01.zip"

# New file name
file_name = basename(url)

# Define output file path
output_filepath = file.path(output_dir, file_name)

# Set time out
options(timeout = 180)

# Download zip file into temp (NOTE: extend 180s timeout if necessary)
download.file(url, destfile = output_filepath)
  
# Set to temp dir
setwd(output_dir)

# Get ab plus csv file names within directory
zip_file_name = archive(file_name) %>% 
  select(path) %>% 
  pull()

# Extract ab plus column names
data = readr::read_csv(
  archive_read(file_name, file = zip_file_name)
  )

# Process data from zip file
data = data %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  rename(POSTCODE = REG_ADDRESS_POST_CODE) %>% 
  mutate(
    # Paste fields together to create single line address
    COMPANY_SLA = toupper(paste(
      ifelse(is.na(COMPANY_NAME), "", COMPANY_NAME),
      ifelse(is.na(REG_ADDRESS_ADDRESS_LINE1), "", REG_ADDRESS_ADDRESS_LINE1),
      ifelse(is.na(REG_ADDRESS_ADDRESS_LINE2), "", REG_ADDRESS_ADDRESS_LINE2),
      ifelse(is.na(REG_ADDRESS_POST_TOWN), "", REG_ADDRESS_POST_TOWN),
      ifelse(is.na(REG_ADDRESS_COUNTY), "", REG_ADDRESS_COUNTY)
    )),
    COMPANY_SLA = stringr::str_squish(COMPANY_SLA),
    POSTCODE = toupper(gsub("[^[:alnum:]]", "", POSTCODE)),
    ) %>% 
  # Remove variables
  select(
    -contains("ACCOUNTS_"),
    -contains("RETURNS_"),
    -contains("PREVIOUS_"),
    -contains("REG_"),
    -contains("MORTGAGES_"),
    -contains("CONF_"),
    -contains("LIMITED_")
  ) %>% 
  # Order variables
  select(COMPANY_SLA, POSTCODE, everything()) %>% 
  # Tidy sla
  tidy_df_single_line_address(., COMPANY_SLA); gc()

# Revert back to project dir
setwd(project_dir)

# Define table name
table_name = "INT646_COMPANIES_BASE"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Create table: ~
DBI::dbWriteTable(
  conn = con,
  name = table_name,
  value = data,
  temporary = FALSE,
  append = TRUE
)

# Remove companies data as quite large
rm(data); gc()

# 1.2. Match companies to abp and cqc care homes -------------------------------

# Create a lazy table from the CQC care home table
house_db = con %>%
  tbl(from = "INT646_COMPANIES_BASE")

# Care home names
ch_db = con %>%
  tbl(from = "INT646_ABP_CQC_20210401_20220331")

# Process companies data
house_db = house_db %>% 
  filter(
    REGEXP_LIKE(
      # Relevant care home related codes
      SIC_CODE_SIC_TEXT_1, '86102|86900|87100|87200|87300|87900'
      )
    ) %>% 
  select(COMPANY_SLA, POSTCODE, COMPANY_NUMBER, COMPANY_TYPE = SIC_CODE_SIC_TEXT_1) 
  
# Process care home address data
ch_db = ch_db %>% 
  filter(CH_FLAG == 1) %>% 
  select(SINGLE_LINE_ADDRESS, POSTCODE, UPRN)

# Match: ~ 30 mins
match_db = addressMatchR::calc_match_addresses(
  primary_df = house_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "COMPANY_SLA",
  lookup_df = ch_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SINGLE_LINE_ADDRESS"
)

# Define table name
table_name = "INT646_COMPANIES_CH_MATCH"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Write the table back to DALP with indexes
match_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# 1.3. Process companies care home match output --------------------------------

# Companies match output
match_db = con %>% 
  tbl(from = "INT646_COMPANIES_CH_MATCH")

# Process results
match_db = match_db %>% 
  # Filter appropriate matches
  filter(SCORE > 0.5) %>% 
  # Single sla per uprn
  group_by(UPRN) %>% 
  mutate(SINGLE_LINE_ADDRESS = max(SINGLE_LINE_ADDRESS)) %>% 
  ungroup() %>% 
  # Take top sla-uprn info per company
  group_by(COMPANY_SLA, COMPANY_NUMBER, POSTCODE) %>% 
  slice_max(
    order_by = SCORE,
    with_ties = FALSE
  ) %>% 
  ungroup() %>% 
  # Take single company per uprn
  group_by(POSTCODE, UPRN, SINGLE_LINE_ADDRESS) %>%
  slice_max(
   order_by = SCORE,
   with_ties = FALSE
  ) %>%
  ungroup()

# Define table name
table_name = "INT646_COMPANIES_CH_RESULTS"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Save table
match_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# 1.4. Match companies house to gp practice info -------------------------------

# Get relevant practices from base table
presc_db = con %>% 
  tbl(from = "INT646_BASE_20210401_20220331")

# Create a lazy table from the CQC care home table
house_db = con %>%
  tbl(from = "INT646_COMPANIES_BASE")

# Alt codes that appear for ch prescribing in base table
presc_db = presc_db %>%
  filter(
    CH_FLAG == 1,
    !is.na(PRESC_POSTCODE),
    PRESC_SLA != '-',
    PRESC_ORG_TYPE != "HOSPITAL"
    ) %>%
  group_by(
    PRESC_SLA,
    PRESC_CODE = PRESC_ORG_CODE,
    POSTCODE = PRESC_POSTCODE,
    PRESC_ORG_TYPE
  ) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  tidy_postcode(., POSTCODE) %>%
  tidy_single_line_address(., PRESC_SLA) %>% 
  group_by(PRESC_CODE) %>% 
  slice_max(
    order_by = ITEMS,
    with_ties = FALSE
  ) %>% 
  select(-ITEMS)
    
# Process companies data
house_db = house_db %>% 
  filter(
    REGEXP_LIKE(
      # Relevant care home related codes
      SIC_CODE_SIC_TEXT_1, '86102|86210|86220|86900'
    )
  ) %>%
  select(COMPANY_SLA, POSTCODE, COMPANY_NUMBER, COMPANY_TYPE = SIC_CODE_SIC_TEXT_1) 

# Match
match_db = addressMatchR::calc_match_addresses(
  primary_df = house_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "COMPANY_SLA",
  lookup_df = presc_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "PRESC_SLA"
)

# Define table name
table_name = "INT646_COMPANIES_GP_MATCH"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Write the table back to DALP with indexes: ~15mins
match_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# 1.5. Process companies gp match ------------------------------------------------

# Companies match output
match_db = con %>% 
  tbl(from = "INT646_COMPANIES_CH_RESULTS")

# Process output
match_db = match_db %>% 
  filter(
    SCORE > 0.5,
    !is.na(PRESC_CODE)
  ) %>% 
  group_by(COMPANY_NUMBER) %>% 
  slice_max(
    order_by = SCORE,
    with_ties = FALSE
  ) %>% 
  ungroup() %>% 
  group_by(POSTCODE, PRESC_CODE) %>% 
  slice_max(
    order_by = SCORE,
    with_ties = FALSE
  ) %>% 
  ungroup()

# Define table name
table_name = "INT646_COMPANIES_GP_RESULTS"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Save table
match_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# 1.6. Get directors from company numbers ----------------------------------------

# Install package
#devtools::install_github("MatthewSmith430/CompaniesHouse")

# Create account
#browseURL('https://account.companieshouse.gov.uk/user/register?request=ey')

# Request a key
#browseURL("https://developer.companieshouse.gov.uk/developer/applications/register ")

# Get key from renviron file
key <- Sys.getenv("CH_API_KEY")

# Companies match output
ch_match_db = con %>% 
  tbl(from = "INT646_COMPANIES_CH_RESULTS")

# Care home companies match vector
ch_vec = ch_match_db %>% 
  select(COMPANY_NUMBER) %>% 
  distinct() %>% 
  pull()

# Companies match output
gp_match_db = con %>% 
  tbl(from = "INT646_COMPANIES_GP_RESULTS")

# Care home companies match vector
gp_vec = gp_match_db %>% 
  select(COMPANY_NUMBER) %>% 
  distinct() %>% 
  pull()

# Get director id function
get_director_id = function(company_num){
  
  # Wait
  Sys.sleep(0.5)
  
  # Skip errors
  out = tryCatch({
    
    # Try get director data
    CompaniesHouse::company_ExtractDirectorsData(company_num, key)
    },
    error = function(cond){
      message(paste0("Error with ", company_num))
    }
  )
  
  # Return
  return(out)
}

# Output
results = lapply(ch_vec, get_director_id)

# Care home results
ch_results = results %>% 
  bind_rows() %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  rename(COMPANY_NUMBER = COMPANY_ID) %>% 
  rename_all(.funs = function(x) paste0("CH_", x))

# Output
tic()
results = lapply(gc_vec, get_director_id)
toc()

# Care home results
gp_results = results %>% bind_rows()

# Generate results
results = list()

