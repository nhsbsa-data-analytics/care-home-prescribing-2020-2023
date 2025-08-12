source("data-raw/workflow/workflow_packages.R")
source("data-raw/workflow/workflow_helpers.R")
source("data-raw/workflow/workflow_production.R")

# Verification variables --------------------------------------------------

# thousand <- 10^3
# million <- 10^6

# Thresholds based off 2020-21 run.
# We use values around 10% lower than the count for this year, or when numbers.
# relatively small just take a step down to a round number - e.g. 3,000 or 1 million.
# Expectation is that these will vary, so don't want thresholds to be too close.
# Overall trend is likely to be upward, so these values should be good for future
# runs, but can be adjusted if necessary.

# POSTCODE_DB_ROW_COUNT_THRESHOLD <- 20 * thousand
# FACT_DB_ROW_COUNT_THRESHOLD <- 240 * million
# PAPER_DB_ROW_COUNT_THRESHOLD <- 50 * million
# EPS_DB_ROW_COUNT_THRESHOLD <- 600 * million

# END - Verification variables ---

start_str <- "202004"
end_str <- "202004"
start_date <- as.character(as.Date(start_str, format = '%Y%m%d'))
end_date <- as.character(as.Date(end_str, format = '%Y%m%d'))

# Set up connection to DWCP and DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Convert to year_months
start_year_month = as.integer(substr(start_str, 1, 6))
end_year_month = as.integer(substr(end_str, 1, 6))

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
# postcode_db <- con %>%
#   tbl(from = address_tbl)

# Part one: filter two fact table cuts for eps and paper info ------------------

# Label CH postcodes
# postcode_db = postcode_db %>%
#   distinct(POSTCODE) #%>%
  # verify(nrow.alt(.) > POSTCODE_DB_ROW_COUNT_THRESHOLD) %>%
  # mutate(POSTCODE_CH = 1)

# Get appropriate year month fields as a vector
year_month = year_month_db %>%
  select(YEAR_MONTH) %>%
  filter(
    YEAR_MONTH >= start_year_month,
    YEAR_MONTH <= end_year_month
  ) %>%
  pull()

# Initial fact table filter
fact_db = fact_db %>%
  filter(
    # CALC_AGE >= 65,
    YEAR_MONTH >= year_month,
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
  distinct(
    YEAR_MONTH,
    PF_ID,
    NHS_NO,
    CALC_AGE,
    EPS_PART_DATE,
    EPM_ID
  ) %>% 
  # verify(nrow.alt(.) > FACT_DB_ROW_COUNT_THRESHOLD) %>% 
  rename(PART_DATE = EPS_PART_DATE)

# Process paper info
paper_db = paper_db %>%
  filter(YEAR_MONTH %in% year_month) %>%
  # verify(nrow.alt(.) > PAPER_DB_ROW_COUNT_THRESHOLD) %>%
  # assert.alt(is_uniq.alt, PF_ID) %>%
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
  # verify(nrow.alt(.) > EPS_DB_ROW_COUNT_THRESHOLD) %>%
  # assert.alt(is_uniq.alt, EPM_ID) %>%
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
  # addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS) %>%
  # addressMatchR::tidy_postcode(POSTCODE) %>% 
  # left_join(postcode_db, by = "POSTCODE") %>% 
  # personMatchR::format_postcode_db(POSTCODE) %>%
  mutate(
    FY = substr(YEAR_MONTH, 1, 4),
    PF_ID,
    NHS_NO,
    CALC_AGE,
    POSTCODE,
    # POSTCODE_CH,
    # SINGLE_LINE_ADDRESS
    .keep = "used"
  )

all_ages_db <- fact_join_db %>% 
  summarise(
    AGE_BAND = "All ages",
    HAS_AGE = !is.na(CALC_AGE),
    HAS_NHSNO = !is.na(NHS_NO),
    HAS_POSTCODE = !is.na(POSTCODE),
    HAS_AGE_NHSNO = !is.na(CALC_AGE) & !is.na(NHS_NO),
    HAS_AGE_POSTCODE = !is.na(CALC_AGE) & !is.na(POSTCODE),
    HAS_NHSNO_POSTCODE = !is.na(NHS_NO) & !is.na(POSTCODE),
    HAS_ALL = !is.na(CALC_AGE) & !is.na(NHS_NO) & !is.na(POSTCODE),
    .by = FY
  )

over_65_db <- fact_join_db %>% 
  filter(CALC_AGE >= 65) %>% 
  summarise(
    AGE_BAND = "65+",
    HAS_AGE = !is.na(CALC_AGE),
    HAS_NHSNO = !is.na(NHS_NO),
    HAS_POSTCODE = !is.na(POSTCODE),
    HAS_AGE_NHSNO = !is.na(CALC_AGE) & !is.na(NHS_NO),
    HAS_AGE_POSTCODE = !is.na(CALC_AGE) & !is.na(POSTCODE),
    HAS_NHSNO_POSTCODE = !is.na(NHS_NO) & !is.na(POSTCODE),
    HAS_ALL = !is.na(CALC_AGE) & !is.na(NHS_NO) & !is.na(POSTCODE),
    .by = FY
  )

final_db <- union(
  all_ages_db,
  over_65_db
)

final_db %>% show_query()
  

# Part three: stack paper and eps info and save --------------------------------

# Print that table has been created
print("Output being computed to be written back to the db ...")

table_name = "EDA_AGE_NHSNO_POSTCODE_COMBINATIONS"

# Drop table if it exists already
drop_table_if_exists_db(table_name)

# Just format postcode
final_db %>% 
  compute_with_parallelism(table_name, 32)

# Grant access
# c("MIGAR", "ADNSH", "MAMCP") %>% grant_table_access (table_name)

# Disconnect connection to database
# DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
# remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
# rm(list = remove_vars, remove_vars); gc()
