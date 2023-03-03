
# Set up connection to DWCP and DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Get start and end dates
start_date = stringr::str_extract_all(address_data, "\\d{8}")[[1]][1]
end_date = stringr::str_extract_all(address_data, "\\d{8}")[[1]][2]

# Convert to year_months
start_year_month = as.integer(substr(start_date, 1, 6))
end_year_month = as.integer(substr(end_date, 1, 6))

# Modify dates for eps buffer
eps_start_date = as.Date(start_date, "%Y%m%d") %m-% months(2)
eps_end_date = (as.Date(end_date, "%Y%m%d")+10) %m+% months(2)

# Change dates back to integers
eps_start_date = as.integer(gsub("-", "", eps_start_date))
eps_end_date = as.integer(gsub("-", "", eps_end_date))

# Create a lazy table addressbase data
ab_plus_cqc_db <- con %>%
  tbl(from = address_data)

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
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
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
  select(POSTCODE, START_DATE, END_DATE, AB_DATE, CQC_DATE) %>% 
  distinct()

# Process paper info
paper_info_db = paper_db %>% 
  inner_join(y = year_month_db) %>% 
  addressMatchR::tidy_postcode(col = POSTCODE) %>% 
  inner_join(y = ab_plus_cqc_db) %>%  
  inner_join(y = fact_paper_db) %>%  
  addressMatchR::tidy_single_line_address(col = ADDRESS) %>% 
  select(
    YEAR_MONTH,
    NHS_NO,
    PF_ID,
    SINGLE_LINE_ADDRESS = ADDRESS,
    POSTCODE,
    START_DATE,
    END_DATE,
    AB_DATE,
    CQC_DATE
    )

# Part three: process electronic info ------------------------------------------

# Create the single line address and subset columns
eps_info_db = eps_db %>%
  # Bring back ETP data
  filter(
    PART_DATE >= eps_start_date,
    PART_DATE <= eps_end_date
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
    YEAR_MONTH,
    NHS_NO,
    PF_ID,
    SINGLE_LINE_ADDRESS,
    POSTCODE = PAT_ADDRESS_POSTCODE,
    START_DATE,
    END_DATE,
    AB_DATE,
    CQC_DATE
  )

# Part three: stack paper and eps info and save --------------------------------

# Stack info (this returns distinct rows, reducing the table to form level)
total_db = eps_info_db %>% 
  dplyr::union(paper_info_db) 

# Define output table name
table_name = paste0("INT646_PFID_", start_date, "_", end_date)

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to DALP with indexes
total_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# Grant access
DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO MIGAR"))

# Disconnect from database
DBI::dbDisconnect(con)

# Print created table name output
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars = setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
   