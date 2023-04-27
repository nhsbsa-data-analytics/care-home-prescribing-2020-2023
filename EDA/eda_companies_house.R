
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

# Prescribing address info
presc_db = con %>% 
  tbl(from = in_schema("DIM", "CUR_EP_LEVEL_5_FLAT_DIM"))

# Create a lazy table from the CQC care home table
house_db = con %>%
  tbl(from = "INT646_COMPANIES_BASE")

# Process companies data
house_db = house_db %>% 
  select(COMPANY_SLA, POSTCODE, COMPANY_NUMBER) %>% 
  tidy_single_line_address(., COMPANY_SLA)

# Alt codes that appear ifor ch prescribing in base table
presc_code_db = base_db %>% 
  filter(CH_FLAG == 1) %>% 
  select(PRESC_CODE = PRESC_ORG_CODE) %>% 
  distinct()

# Filter by alt codes and prepare for union
presc_db = presc_db %>% 
  mutate(UPRN = NA) %>% 
  select(
    SLA = LVL_5_CUR_FULL_ADDRESS,
    POSTCODE = LVL_5_CUR_POSTCODE,
    UPRN,
    PRESC_CODE = LVL_5_LTST_ALT_CDE,
  ) %>% 
  inner_join(presc_code_db, by = "PRESC_CODE") %>% 
  tidy_single_line_address(., SLA) %>% 
  tidy_postcode(., POSTCODE)

# Filter by postcode and prepare for union
abp_db = abp_db %>% 
  mutate(PRESC_CODE = NA) %>% 
  select(
    SLA = GEO_SINGLE_LINE_ADDRESS, 
    POSTCODE, 
    UPRN,
    PRESC_CODE
  )

# Union data
presc_db = union_all(presc_db, abp_db)

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

# Write the table back to DALP with indexes
tic()
match_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )
toc()

rm(precs_db)


# Disconnect
DBI::dbDisconnect(con)

# Clean and remove objects
rm(list = ls()); gc()













# 1. Generate companies house account ------------------------------------------

# Create account
browseURL('https://account.companieshouse.gov.uk/user/register?request=ey')

# Request a key
browseURL("https://developer.companieshouse.gov.uk/developer/applications/register ")

# Get key from renviron file
key <- Sys.getenv("CH_API_KEY")