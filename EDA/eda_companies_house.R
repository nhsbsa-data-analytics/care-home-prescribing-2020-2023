
# Load/install all required packages and functions
source("R/analysis_packages.R")
source("R/workflow_helpers.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# 1. Bulk download of companies house data -------------------------------------

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

# Download zip file into temp (NOTE: 60s timeout might need several attempts)
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
  select(COMPANY_SLA, POSTCODE, everything())

# Revert back to project dir
setwd(project_dir)

# Define table name
table_name = "INT646_COMPANIES_BASE"

# Remove table if exists
drop_table_if_exists_db(table_name)

# Create table
DBI::dbWriteTable(
  conn = con,
  name = table_name,
  value = data,
  temporary = FALSE,
  append = TRUE
)

# Remove companies data as quite large
#rm(data); gc()

# 2. Match companies to abp and cqc care homes ---------------------------------

#'https://www.cqc.org.uk/sites/default/files/2023-04/26_April_2023_CQC_directory.csv'

df = readODS::read_ods(
  "C:/Users/ADNSH/OneDrive - NHS Business Services Authority/Desktop/03_April_2023_HSCA_Active_Locations.ods",
  sheet = 2
  )

options(java.parameters = "-Xmx16g")
library(xlsx)
library(readODS)

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx32g"))
gc()

df = readODS::read_ods(
  path = "C:/Users/ADNSH/OneDrive - NHS Business Services Authority/Desktop/03_April_2023_HSCA_Active_Locations.ods",
  sheet = 2,
  range = "A1:A2"
)

df = readxl::read_xlsx(
  path = "C:/Users/ADNSH/OneDrive - NHS Business Services Authority/Desktop/03_April_2023_HSCA_Active_Locations.xlsx",
  sheet = 2
)

df =readxl::read_xlsx(
  path = "C:/Users/ADNSH/OneDrive - NHS Business Services Authority/Desktop/03_April_2023_HSCA_Active_Locations.xlsx",
  sheet = 2
)

df = data.table::fread("C:/Users/ADNSH/OneDrive - NHS Business Services Authority/Desktop/CQC_location_information.csv")


a = read.csv(url(cqc_url))

download = RCurl::getURL(cqc_url)
output_file = tempfile()

# Download zip file into temp (NOTE: 60s timeout might need several attempts)
download.file(url, output_file)

# Set to temp dir
setwd(output_dir)

connect = httr::GET(url)

# Get ab plus csv file names within directory
zip_file_name = archive(file_name) %>% 
  select(path) %>% 
  pull()

# Extract ab plus column names
data = readr::read_csv(
  archive_read(file_name, file = zip_file_name)
)


a = xlsx::read.xlsx(
  file = url,
  sheetIndex = 0
)

data = readODS::read_ods(
  paths = ,
  sheet = 2
)






















# Create a lazy table from the CQC care home table
house_db = con %>%
  tbl(from = "INT646_COMPANIES_BASE")

# Care home names
ch_db = con %>%
  tbl(from = "INT646_ABP_CQC_20210401_20220331")

# Process companies data
house_db = house_db %>% 
  select(COMPANY_SLA, POSTCODE, COMPANY_NUMBER) %>% 
  tidy_single_line_address(., COMPANY_SLA)

# Process care home address data
ch_db = ch_db %>% 
  select(SINGLE_LINE_ADDRESS, POSTCODE, UPRN, CH_FLAG)

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

# 3. Process companies caare home match output ---------------------------------

# Relevant care home uprns (i.e. not young people ch)
uprn_db = con %>% 
  tbl(from = "INT646_MATCH_20210401_20220331") %>% 
  filter(CH_FLAG == 1) %>% 
  select(UPRN) %>% 
  distinct()

# Companies match output
match_db = con %>% 
  tbl(from = "INT646_COMPANIES_CH_MATCH")

# Process care home address data
ch_db = ch_db %>% 
  select(SINGLE_LINE_ADDRESS, POSTCODE, UPRN, CH_FLAG)

# Process results
match_db = match_db %>% 
  # Filter appropriate matches
  filter(
    SCORE > 0.5,
    CH_FLAG == 1
  ) %>% 
  inner_join(uprn_db, by = "UPRN") %>% 
  # Take top sla-uprn info per company
  group_by(COMPANY_SLA, COMPANY_NUMBER, POSTCODE) %>% 
  slice_max(
    order_by = SCORE,
    with_ties = FALSE
  ) %>% 
  ungroup() %>% 
  # Take single company per uprn
  group_by(POSTCODE, UPRN, SINGLE_LINE_ADDRESS, CH_FLAG, SCORE) %>% 
  slice_max(
    order_by = COMPANY_NUMBER,
    with_ties = FALSE
  ) %>% 
  ungroup() %>% 
  # Single sla per uprn
  group_by(UPRN) %>% 
  mutate(SINGLE_LINE_ADDRESS = max(SINGLE_LINE_ADDRESS)) %>% 
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

# 4. Match companies house to gp practice info ---------------------------------

# Get relevant practices from base table
base_db = con %>% 
  tbl(from = "INT646_BASE_20210401_20220331")

# Care home names
abp_db = con %>%
  tbl(from = "INT646_ABP_20220422")

# Create a lazy table from the CQC care home table
house_db = con %>%
  tbl(from = "INT646_COMPANIES_BASE")

# Alt codes that appear for ch prescribing in base table
presc_db = base_db %>%
  filter(
    CH_FLAG == 1,
    !is.na(PRESC_POSTCODE),
    PRESC_SLA != '-'
    ) %>%
  group_by(
    PRESC_CODE = PRESC_ORG_CODE,
    POSTCODE = PRESC_POSTCODE,
  ) %>%
  summarise(
    SLA = max(PRESC_SLA, na.rm = TRUE),
    UPRN = NA
  ) %>% 
  ungroup() %>%
  tidy_postcode(., POSTCODE) %>%
  tidy_single_line_address(., SLA)

# Postcodes to filer ab
postcode_db = base_db %>% 
  filter(
    CH_FLAG == 1,
    !is.na(PRESC_POSTCODE),
    PRESC_SLA != '-'
  ) %>% 
  group_by(POSTCODE = PRESC_POSTCODE) %>% 
  summarise() %>% 
  ungroup() %>% 
  tidy_postcode(., POSTCODE)

# Process companies data
house_db = house_db %>% 
  inner_join(postcode_db, by = "POSTCODE") %>% 
  select(COMPANY_SLA, POSTCODE, COMPANY_NUMBER) %>% 
  tidy_single_line_address(., COMPANY_SLA)

# Filter by postcode and prepare for union
abp_db = abp_db %>% 
  inner_join(postcode_db, by = "POSTCODE") %>% 
  mutate(PRESC_CODE = NA) %>% 
  select(
    SLA = GEO_SINGLE_LINE_ADDRESS, 
    POSTCODE, 
    UPRN,
    PRESC_CODE
  )

# Union data
presc_db = union(presc_db, abp_db)

# Match
match_db = addressMatchR::calc_match_addresses(
  primary_df = house_db,
  primary_postcode_col = "POSTCODE",
  primary_address_col = "COMPANY_SLA",
  lookup_df = presc_db,
  lookup_postcode_col = "POSTCODE",
  lookup_address_col = "SLA"
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

# 5. Process companies gp match ------------------------------------------------

# Companies match output
match_db = con %>% 
  tbl(from = "INT646_COMPANIES_GP_MATCH")

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

# 6. Get directors from company numbers ----------------------------------------

# Create account
browseURL('https://account.companieshouse.gov.uk/user/register?request=ey')

# Request a key
browseURL("https://developer.companieshouse.gov.uk/developer/applications/register ")

# Get key from renviron file
key <- Sys.getenv("CH_API_KEY")

# Care home results
ch_db = con %>% 
  tbl(from = "INT646_COMPANIES_CH_RESULTS")

# GP results
gp_db = con %>% 
  tbl(from = "INT646_COMPANIES_GP_RESULTS")

# CH company number vec
ch = ch_db %>% 
  select(COMPANY_NUMBER) %>% 
  pull()

# GP company number vec
gp = gp_db %>% 
  select(COMPANY_NUMBER) %>% 
  pull()



# Disconnect
DBI::dbDisconnect(con); rm(list = ls()); gc()

# Clean and remove objects


df = CompaniesHouse::company_ExtractDirectorsData('03260168', key)

df

# One
CompaniesHouse::CompanyDataExtract(03260168, key)

# Two
company_ExtractDirectorsData("00041424", key)

# Three
indiv_ExtractDirectorsData(df$director.id, mkey)

df



CompaniesHouse::DirectorSearch_limit_first("66Fxv2-ucvvtVhvVuia-qhJf0tA", mkey)




company_ExtractDirectorsData("SL58AN", key)
CompanySearchList<-CompanySearch_limit_first(company = "unilever", mkey = key)

?CompanySearch_limit

devtools::install_github("MatthewSmith430/CompaniesHouse")
library(CompaniesHouse)

for(i in 130:180){
  a=company_ExtractDirectorsData(i, key)
  print(a)
}
a

url = 'https://api.company-information.service.gov.uk/search/companies/company_number?q=00041424'

get_check <-httr::GET(url, httr::authenticate(key, ""))

text = httr::content(get_check, as="text")

#'  Returns companies officer details for identified company (company number) on Companies House via the API 
#' 
#'  @param comp_num The company number you want a list of officers for
#'  @param mkey The api key which you need to generate - see above for more details
#'  @return The results of the search performed
#'  @example 
#'  search_officers("Company Number", "a_fake_api_key")  
#'  
search_officers <- function(comp_num, key){
  
  print("running")
  
  # concatenate API URL with a CompaniesHouse company number 
  curl_cmd <- paste0("https://api.companieshouse.gov.uk/company/", ch[1], "/officers")
  
  curl_cmd
  
  # Run the call and collect the response data
  get_check <-httr::GET(curl_cmd, httr::authenticate(key, ""))
  get_data <- httr::content(get_check, as="text")
  json_data <- jsonlite::fromJSON(get_data, flatten = TRUE)
  
  return (json_data)
}

ch[1]

get_check

a = search_officers(ch[1], key)

a


#' Returns a list of company appointments for a officer done via the CompaniesHouse API 
#' 
#'  @param officer_id The officer id for the officer you want to search for
#'  @param mkey The api key which you need to generate - see above for more details
#'  @return The results of the search performed
#'  @example 
#'  search_officers("CompanyOfficerID", "a_fake_api_key")  
#'  
search_appointments <- function(officer_id, mkey){
  
  print("running")
  
  # concatenate API URL with a CompaniesHouse officer id
  curl_cmd <- paste0("https://api.companieshouse.gov.uk/officers/", officer_id, "/appointments")
  
  # Run the call and collect the response data
  get_check <-httr::GET(curl_cmd, httr::authenticate(mkey, ""))
  get_data <- httr::content(get_check, as="text")
  json_data <- jsonlite::fromJSON(get_data, flatten = TRUE)
  
  
  return (json_data)
}
