
# Set up connection to DWCP and DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Get start and end dates
start_date = stringr::str_extract_all(address_data, "\\d{8}")[[1]][1]
end_date = stringr::str_extract_all(address_data, "\\d{8}")[[1]][2]

# Convert to year_months
start_year_month = as.integer(substr(start_date, 1, 6))
end_year_month = as.integer(substr(end_date, 1, 6))

# Modify dates for eps buffer
eps_start_date = ymd(start_date) %m-% months(2)
eps_end_date = ymd(end_date) %m+% days(10) %m+% months(2)

# Change dates back to integers
eps_start_date = as.integer(gsub("-", "", eps_start_date))
eps_end_date = as.integer(gsub("-", "", eps_end_date))

# Create a lazy table from year month dim table in DWCP
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Lazy table for paper info
paper_db <- con %>%
  tbl(from = in_schema("DALL_REF", "PX_PAPER_PFID_ADDRESS"))

# Lazy table for fact table
fact_db <- con %>%
  tbl(from = in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT"))

# Lazy table for eps table
eps_db <- con %>%
  tbl(from = in_schema("SCD2", "SCD2_ETP_DY_PAYLOAD_MSG_DATA"))

# Postcodes with a care home
postcode_db <- con %>%
  tbl(from = address_data)

# Part one: filter two fact table cuts for eps and paper info ------------------

# Label CH postcodes
postcode_db = postcode_db %>% 
  select(POSTCODE) %>% 
  distinct() %>% 
  mutate(POSTCODE_CH = 1)

# Get appropriate year month fields as a table
year_month = year_month_db %>% 
  select(YEAR_MONTH) %>% 
  filter(
    YEAR_MONTH >= start_year_month,
    YEAR_MONTH <= end_year_month
  ) %>% 
  pull()

# Initial fact table filter
fact_db = fact_db %>% 
  select(
    # Group by vars
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    CALC_AGE,
    EPS_PART_DATE,
    EPM_ID,
    # filter vars
    PATIENT_IDENTIFIED,
    PAY_DA_END,
    PAY_ND_END,
    PAY_RB_END,
    CD_REQ,
    OOHC_IND,
    PRIVATE_IND,
    IGNORE_FLAG,
    ITEM_COUNT 
  ) %>% 
  filter(
    YEAR_MONTH %in% year_month,
    PATIENT_IDENTIFIED == "Y",
    PAY_DA_END == "N", # excludes disallowed items
    PAY_ND_END == "N", # excludes not dispensed items
    PAY_RB_END == "N", # excludes referred back items
    CD_REQ == "N", # excludes controlled drug requisitions
    OOHC_IND == 0L, # excludes out of hours dispensing
    PRIVATE_IND == 0L, # excludes private dispensers
    IGNORE_FLAG == "N", # remove dummy ldp forms
    ITEM_COUNT >= 1 # remove element-level rows
  ) %>% 
  group_by(
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    CALC_AGE,
    PART_DATE = EPS_PART_DATE,
    EPM_ID
  ) %>% 
  summarise() %>% 
  ungroup()

# Process paper info
paper_db = paper_db %>% 
  filter(YEAR_MONTH %in% year_month) %>% 
  select(
    YEAR_MONTH,
    PF_ID,
    PAPER_SINGLE_LINE_ADDRESS = ADDRESS,
    PAPER_POSTCODE = POSTCODE
  )

# Process electronic info
eps_db = eps_db %>%
  # Bring back ETP data within dates
  filter(
    PART_DATE >= eps_start_date,
    PART_DATE <= eps_end_date
  ) %>% 
  # Concatenate fields together by a single space for the single line address
  mutate(
    EPS_SINGLE_LINE_ADDRESS = paste(
      PAT_ADDRESS_LINE1,
      PAT_ADDRESS_LINE2,
      PAT_ADDRESS_LINE3,
      PAT_ADDRESS_LINE4
    )
  ) %>%
  select(
    EPM_ID,
    PART_DATE,
    EPS_SINGLE_LINE_ADDRESS,
    EPS_POSTCODE = PAT_ADDRESS_POSTCODE
  )

# Part two: fact join and postcode & SLA tidy ----------------------------------

# Join all tables and clean
fact_join_db = fact_db %>% 
  left_join(y = paper_db, by = c("YEAR_MONTH", "PF_ID")) %>% 
  left_join(y = eps_db, by = c("PART_DATE", "EPM_ID")) %>% 
  mutate(
    POSTCODE = coalesce(
      EPS_POSTCODE, 
      PAPER_POSTCODE
    ),
    SINGLE_LINE_ADDRESS = coalesce(
      EPS_SINGLE_LINE_ADDRESS,
      PAPER_SINGLE_LINE_ADDRESS
    )
  ) %>% 
  addressMatchR::tidy_postcode(col = POSTCODE) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS) %>% 
  left_join(postcode_db, by = "POSTCODE") %>% 
  select(
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    CALC_AGE,
    POSTCODE,
    POSTCODE_CH,
    SINGLE_LINE_ADDRESS
  )

# Part three: stack paper and eps info and save --------------------------------

# Define output table name
table_name = paste0("INT646_FORMS_", start_date, "_", end_date)

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Print that table has been created
print("Output being computed to be written back to the db ...")

# Write the table back to DALP
fact_join_db %>%
  compute(
    name = table_name,
    temporary = FALSE
  )

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% lapply(
  \(x) {
    DBI::dbExecute(con, paste0("GRANT SELECT ON ", table_name, " TO ", x))
  }
) %>% invisible()

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
