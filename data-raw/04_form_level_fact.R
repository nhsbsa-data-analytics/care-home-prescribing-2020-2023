
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
forms_db <- con %>%
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

# Start and end date as integers
start_int = get_integer_from_date(start_date)
end_int = get_integer_from_date(end_date)

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
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
    ) %>%
  select(
    YEAR_MONTH,
    PF_ID,
    EPS_FLAG,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PDS_GENDER,
    CALC_AGE,
    NHS_NO,
    ITEM_COUNT
  )

# Fact table eps info
fact_eps_db = fact_db %>% 
  filter(EPS_FLAG == "Y")

# Fact table paper info
fact_paper_db = fact_db %>% 
  filter(EPS_FLAG == "N") %>% 
  select(
    PF_ID, 
    CALC_AGE, 
    ITEM_COUNT, 
    PDS_GENDER
    )

# Part two: process paper info -------------------------------------------------

# Get ab plus and cqc postcodes
ab_cqc_postcodes_db = ab_plus_cqc_db %>% 
  select(POSTCODE) %>% 
  distinct()

# Process paper info
forms_info_db = forms_db %>% 
  inner_join(year_month_db) %>% 
  addressMatchR::tidy_postcode(col = POSTCODE) %>% 
  inner_join(ab_cqc_postcodes_db) %>% 
  inner_join(fact_paper_db, by = "PF_ID") %>% 
  addressMatchR::tidy_postcode(col = ADDRESS) 
  select(
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    SINGLE_LINE_ADDRESS = ADDRESS,
    POSTCODE,
    CALC_AGE, 
    ITEM_COUNT, 
    PDS_GENDER
    )
forms_info_db
# Part two: get electronic address information --------------------------------

# Create the single line address and subset columns
eps_db = eps_db %>%
  # Bring back ETP data from the month previous until 2 months after
  filter(
    PART_DATE >= start_int,
    PART_DATE <= end_int
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
  select(
    PART_DATE,
    EPM_ID,
    POSTCODE = PAT_ADDRESS_POSTCODE,
    SINGLE_LINE_ADDRESS
  ) %>% 
  # Tidy postcode and format single line addresses
  addressMatchR::tidy_postcode(col = POSTCODE) %>%
  inner_join(ab_cqc_postcodes_db) %>% 
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS)

# Get relevant fact table pf_ids
eps_info_db = fact_db %>% 
  inner_join(year_month_db) %>% 
  filter(
    CALC_AGE >= 65L,
    EPS_FLAG == "Y",
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N" # excludes LDP dummy forms
  ) %>%
  select(
    YEAR_MONTH,
    PF_ID,
    EPS_FLAG,
    PART_DATE = EPS_PART_DATE,
    EPM_ID,
    PDS_GENDER,
    CALC_AGE,
    NHS_NO,
    ITEM_COUNT
    ) %>% 
  inner_join(y = eps_db) %>% 
  select(
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    SINGLE_LINE_ADDRESS,
    POSTCODE,
    CALC_AGE, 
    ITEM_COUNT, 
    PDS_GENDER
  )

# Part three: stack paper and eps info and save --------------------------------

# Stack info
total_db = eps_info_db %>% 
  dplyr::union(paper_info_db)

# Define output table name
table_name = "INT646_FORM_LEVEL_FACT"

# Drop table if it exists already
drop_table_if_exists_db(table_name)

tic()
# Write the table back to DALP with indexes
total_db %>%
  compute(
    name = table_name,
    indexes = list(c("YEAR_MONTH", "PF_ID"), c("POSTCODE")),
    temporary = FALSE
  )
toc()
# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()
