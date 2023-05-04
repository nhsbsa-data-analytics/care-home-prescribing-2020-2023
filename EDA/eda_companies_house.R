
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
  select(
    COMPANY_SLA, 
    POSTCODE, 
    COMPANY_NUMBER, 
    COMPANY_TYPE = SIC_CODE_SIC_TEXT_1
    ) 
  
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

# Process results
match_db = match_db %>% 
  # Filter appropriate matches
  filter(
    SCORE > 0.5,
    !is.na(UPRN)
  ) %>% 
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
  group_by(UPRN, SINGLE_LINE_ADDRESS) %>%
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

# 1.3. Match companies house to gp practice info -------------------------------

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
  select(
    COMPANY_SLA, 
    POSTCODE, 
    COMPANY_NUMBER, 
    COMPANY_TYPE = SIC_CODE_SIC_TEXT_1
    ) 

# Match
match_db = addressMatchR::calc_match_addresses(
  primary_df = house_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "COMPANY_SLA",
  lookup_df = presc_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "PRESC_SLA"
)

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

# 1.4. Match to company directors ----------------------------------------------

# Install package
#devtools::install_github("MatthewSmith430/CompaniesHouse")

# Create account
#browseURL('https://account.companieshouse.gov.uk/user/register?request=ey')

# Request a key
#browseURL("https://developer.companieshouse.gov.uk/developer/applications/register ")

# Get key from renviron file
key <- Sys.getenv("CH_API_KEY")

# Base data
base_db = con %>% 
  tbl(from = "INT646_BASE_20210401_20220331")

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

# Get director id function, 0.8s sleep for throttling 
get_director_id = function(company_num){
  
  # Wait
  Sys.sleep(0.8)
  
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
results = lapply(gp_vec, get_director_id)

# Care home results
gp_results = results %>% 
  bind_rows() %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  rename(COMPANY_NUMBER = COMPANY_ID) %>% 
  rename_all(.funs = function(x) paste0("GP_", x))

# Alt code to uprn item count
items = base_db %>% 
  filter(UPRN_FLAG == 1) %>% 
  group_by(
    PRESC_CODE = PRESC_ORG_CODE,
    UPRN
    ) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  collect_with_parallelism(., 8)

# Care home results
ch = ch_match_db %>% 
  collect_with_parallelism(., 8) %>% 
  rename_all(.funs = function(x) paste0("CH_", x))

# GP results
gp = gp_match_db %>% 
  collect_with_parallelism(., 8) %>% 
  rename_all(.funs = function(x) paste0("GP_", x))

# Final output
output = items %>% 
  left_join(gp, by = c("PRESC_CODE" = "GP_PRESC_CODE")) %>% 
  left_join(ch, by = c("UPRN" = "CH_UPRN")) %>% 
  filter(!is.na(GP_COMPANY_SLA) & !is.na(CH_COMPANY_SLA)) %>% 
  left_join(
    gp_results %>% 
      select(GP_COMPANY_NUMBER, GP_DIRECTOR_ID, GP_DIRECTORS),
    relationship = "many-to-many"
  ) %>% 
  left_join(
    ch_results %>% 
      select(CH_COMPANY_NUMBER, CH_DIRECTOR_ID, CH_DIRECTORS),
    relationship = "many-to-many"
  ) %>% 
  select(
    GP_DIRECTORS,
    GP_DIRECTOR_ID,
    GP_COMPANY_NUMBER, 
    GP_COMPANY_SLA, 
    GP_PRESC_SLA, 
    PRESC_CODE, 
    ITEMS, 
    UPRN, 
    CH_PRESC_SLA = CH_SINGLE_LINE_ADDRESS, 
    CH_COMPANY_SLA, 
    CH_COMPANY_NUMBER,
    CH_DIRECTOR_ID,
    CH_DIRECTORS
    ) %>% 
  filter(
    GP_DIRECTOR_ID == CH_DIRECTOR_ID,
    GP_COMPANY_NUMBER != CH_COMPANY_NUMBER
    ) %>% 
  distinct()

# 2.1. Get director and links info ---------------------------------------------

# CQC data
cqc_db = con %>% 
  tbl(from = "INT646_CQC_20230502")

# CQC data
ab_db = con %>% 
  tbl(from = "INT646_ABP_CQC_20210401_20220331") %>% 
  filter(!is.na(LOCATION_ID)) %>% 
  select(LOCATION_ID) %>% 
  distinct()

# Collect and filter
cqc = cqc_db %>% 
  inner_join(ab_db) %>% 
  collect_with_parallelism(., 8) %>% 
  filter(!is.na(PROVIDER_COMPANIES_HOUSE_NUMBER)) %>% 
  select(
    UPRN, 
    LOCATION_ID, 
    SINGLE_LINE_ADDRESS, 
    POSTCODE, 
    PROVIDER_UPRN, 
    PROVIDER_ID,
    PROVIDER_COMPANIES_HOUSE_NUMBER,
    PROVIDER_SLA,
    PROVIDER_POSTCODE
    )

# Provider company numbers: ~90mins
prov_vec = cqc %>% 
  select(PROVIDER_COMPANIES_HOUSE_NUMBER) %>% 
  distinct() %>% 
  pull()

# Output
results = lapply(prov_vec, get_director_id)

# Provider results
prov_results = results %>% 
  bind_rows() %>% 
  janitor::clean_names() %>% 
  rename_all(.funs = toupper) %>% 
  mutate(POSTCODE = toupper(gsub("[^[:alnum:]]", "", POSTCODE)))

# Director ids
director_vec = prov_results %>% 
  select(DIRECTOR_ID) %>% 
  distinct() %>% 
  pull()

# Get information for every company a director is associated with
get_all_directors_company_info = function(dir_id){
  
  # Wait
  Sys.sleep(0.4)
  
  # Skip errors
  out = tryCatch({
    
    # Paste director id into url
    url<-paste0("https://api.company-information.service.gov.uk/officers/", dir_id,"/appointments")
    
    # Get content
    content <- httr::GET(url, httr::authenticate(key, "")) #returns an R list object
    
    # Convert binary to character
    content = jsonlite::fromJSON(rawToChar(content$content))
    
    # Insert fields into df
    df = cbind(
      DIRECTOR_ID = dir_id,
      DIRECTOR_NAME = content$name,
      SECONDARY_DIRECTOR_ROLE = content$items$officer_role,
      SECONDARY_COMPANY_NAME = content$items$appointed_to$company_name,
      SECONDARY_COMPANY_NUMBER = content$items$appointed_to$company_number,
      SECONDARY_COMPANY_STATUS = content$items$appointed_to$company_status, 
      ADDRESS_ONE = content$items$address$address_line_1,
      ADDRESS_TWO = content$items$address$address_line_2,
      ADDRESS_THREE = content$items$address$locality,
      ADDRESS_FOUR = content$items$address$region,
      SECONDARY_POSTCODE = content$items$address$postal_code,
      SECONDARY_APPOINMENT_TYPE = content$kind
    ) %>% 
      as.data.frame()
    
    # Return
    return(df)
  },
  
  # Error 
  error = function(cond){
    message(paste0("Error with ", dir_id))
    }
  )
  
  # Return
  return(out)
}

# Data
results = lapply(director_vec, get_all_directors_company_info)

# Bind rows
dir_results = results %>% 
  bind_rows() %>% 
  group_by(DIRECTOR_ID) %>% 
  mutate(COMPANY_COUNT = n()) %>% 
  ungroup() %>% 
  filter(COMPANY_COUNT >= 2)

# Data
saveRDS(dir_results, "../../Desktop/DIRECTOR_LOOOKUP.Rds")

# Get associated company numbers
comp_vec = dir_results %>% 
  select(SECONDARY_COMPANY_NUMBER) %>% 
  distinct() %>% 
  pull()

# Get sic code from company number
get_company_sic = function(comp_num){
  
  # Wait
  Sys.sleep(0.2)
  
  # Skip errors
  out = tryCatch({
    
    # Df output
    df = data.frame(
      COMPANY_NUMBER = comp_num,
      COMPANY_SIC = CompaniesHouse::CompanySIC("00041424", key)
    )
    
    # Return
    return(df)
  },
  
  # Error 
  error = function(cond){
    message(paste0("Error with ", comp_num))
    }
  )
  
  # Return
  return(out)
}
  
# Data
results = lapply(comp_vec, get_company_sic)
  
# Df of associated sic codes
sic = results %>% 
  bind_rows() 

