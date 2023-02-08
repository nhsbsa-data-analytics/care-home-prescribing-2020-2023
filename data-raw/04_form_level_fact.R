
# Load packages and global variables
source("R/analysis_packages.R")
source("R/workflow_helpers.R")

# Set up connection to DWCP and DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table addressbase data
ab_plus_cqc_db <- con %>%
  tbl(from = "INT646_AB_PLUS_CQC_STACK")

# Create a lazy table from year month dim table in DWCP
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Lazy table for paper info
paper_db <- con %>%
  tbl(from = in_schema("DALL_REF", "PX_PAPER_PFID_ADDRESS"))

# Lazy table for fact table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Lazy table for fact table
eps_db <- con %>%
  tbl(from = in_schema("SCD2", "SCD2_ETP_DY_PAYLOAD_MSG_DATA"))

# Define start and end dates
start_date = "2021-04-01"
end_date = "2022-03-31"

# Derive start and end year months
start_year_month = get_year_month_from_date(start_date)
end_year_month = get_year_month_from_date(end_date)

# Define 'buffered' eps date range: for query efficiency
eps_start_date = as.Date(start_date) %m-% months(2)
eps_end_date = (as.Date(end_date)+10) %m+% months(2)

# Start and end date as integers
eps_start_int = get_integer_from_date(eps_start_date)
eps_end_int = get_integer_from_date(eps_end_date)

# Part one: filter two fact table cuts for eps and paper info ------------------

# Get appropriate year month fields as a table
year_month_db = year_month_db %>% 
  select(YEAR_MONTH) %>% 
  filter(
    YEAR_MONTH >= start_year_month,
    YEAR_MONTH <= end_year_month
    )

# Initial fact table filter
fact_db = fact_db %>% 
  inner_join(year_month_db) %>% 
  filter(
    CALC_AGE >= 65L,
    PATIENT_IDENTIFIED == "Y",
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N", # remove dummy ldp forms
    ITEM_COUNT >= 1 # remove element-level rows
    )

# Fact table eps info
fact_eps_db = fact_db %>% 
  filter(EPS_FLAG == "Y") %>% 
  select(
    PF_ID,
    PART_DATE = EPS_PART_DATE,
    EPM_ID
    )

# Fact table paper info
fact_paper_db = fact_db %>% 
  filter(EPS_FLAG == "N") %>% 
  select(PF_ID)

# Part two: process paper info -------------------------------------------------

# Get ab plus and cqc postcodes
ab_plus_cqc_db = ab_plus_cqc_db %>% 
  select(POSTCODE) %>% 
  distinct()

# Process paper info
paper_info_db = paper_db %>% 
  inner_join(year_month_db) %>% 
  addressMatchR::tidy_postcode(col = POSTCODE) %>% 
  inner_join(ab_plus_cqc_db) %>%  
  inner_join(fact_paper_db, by = "PF_ID") %>%  
  addressMatchR::tidy_single_line_address(col = ADDRESS) %>% 
  select(
    PF_ID,
    SINGLE_LINE_ADDRESS = ADDRESS,
    POSTCODE
    )

# Part three: process electronic info ------------------------------------------

# Create the single line address and subset columns
eps_info_db = eps_db %>%
  # Bring back ETP data
  filter(
    PART_DATE >= eps_start_int,
    PART_DATE <= eps_end_int
  ) %>% 
  # Concatenate fields together by a single space for the single line address
  mutate(
    SINGLE_LINE_ADDRESS = paste(
      PAT_ADDRESS_LINE1,
      PAT_ADDRESS_LINE2,
      PAT_ADDRESS_LINE3,
      PAT_ADDRESS_LINE4
    )
  ) %>%
  # Tidy postcode and format single line addresses
  addressMatchR::tidy_postcode(col = PAT_ADDRESS_POSTCODE) %>%
  inner_join(ab_plus_cqc_db, by = c("PAT_ADDRESS_POSTCODE" = "POSTCODE")) %>% 
  inner_join(fact_eps_db, by = c("EPM_ID", "PART_DATE")) %>% 
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS) %>% 
  select(
    PF_ID,
    SINGLE_LINE_ADDRESS,
    POSTCODE = PAT_ADDRESS_POSTCODE
  )

# Part three: stack paper and eps info and save --------------------------------

# Stack info
total_db = eps_info_db %>% 
  dplyr::union(paper_info_db)

# Define output table name
table_name = "INT646_FORM_LEVEL_FACT"

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Write the table back to DALP with indexes
total_db %>%
  compute(
    name = table_name,
    indexes = list(c("YEAR_MONTH", "PF_ID"), c("POSTCODE")),
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()
