library(dplyr)
library(dbplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Manually specify start date and end date
start_date = '2021-04-01'
end_date = '2022-03-31'

# Part One: Get cqc postcodes --------------------------------------------------

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = "INT646_CQC_202301")

# Remove na uprn, filter by relevant date
cqc_postcodes_db = cqc_db %>%   
  mutate(
    REGISTRATION_DATE = ifelse(
      test = is.na(REGISTRATION_DATE),
      yes = NA,
      no = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD")
    ),
    DEREGISTRATION_DATE = ifelse(
      test = is.na(DEREGISTRATION_DATE),
      yes = NA,
      no = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD")
    )
  ) %>%
  filter(
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) |
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD"),
    !is.na(UPRN)
  ) %>% 
  select(POSTCODE) %>% 
  distinct()

# Part Two: Filter addressbase data --------------------------------------------

# Create a lazy table addressbase data in scd2
addressbase_plus_db <- con %>%
  tbl(from = in_schema("SCD2", sql("SCD2_OS_ADDRESS_BASE_DATA@DWCP.WORLD")))

# Filter AddressBase Plus to English properties in at the end of 2021 FY with ch flag
addressbase_plus_db <- addressbase_plus_db %>%
  filter(
    !is.na(DW_END_DATE),
    COUNTRY == "E",
    substr(CLASS, 1, 1) != "L", # Land
    substr(CLASS, 1, 1) != "O", # Other (Ordnance Survey only)
    substr(CLASS, 1, 2) != "PS", # Street Record
    substr(CLASS, 1, 2) != "RC", # Car Park Space
    substr(CLASS, 1, 2) != "RG", # Lock-Up / Garage / Garage Court
    substr(CLASS, 1, 1) != "Z", # Object of interest
  ) %>%
  mutate(CH_FLAG = ifelse(CLASS == "RI01", 1L, 0L)) %>%
  # Take POSTCODE_LOCATOR as the postcode as it is equal to POSTCODE (whenever
  # one exists) but more complete and tidy it
  mutate(POSTCODE = POSTCODE_LOCATOR) %>%
  addressMatchR::tidy_postcode(col = POSTCODE)

# Get postcodes where there is a care home present (including CQC data)
care_home_postcodes_db <-
  union_all(
    x = addressbase_plus_db %>%
      filter(CH_FLAG == 1L) %>%
      select(POSTCODE),
    y = cqc_db %>%
      select(POSTCODE)
  )

# Filter AddressBase Plus to postcodes where there is a care home present
addressbase_plus_df <- addressbase_plus_db %>%
  semi_join(y = care_home_postcodes_db) %>% 
  collect()
  

# Part Three: Save as table in dw ----------------------------------------------

# Drop any existing table beforehand
if(DBI::dbExistsTable(conn = con, name = "INT646_ADDRESSBASE") == T){
  DBI::dbRemoveTable(conn = con, name = "INT646_ADDRESSBASE")
}

# Write the table back to the DB with indexes
addressbase_plus_db %>%
  compute(
    name = "INT646_ADDRESSBASE",
    indexes = "POSTCODE",
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)

# Remove and clean environment
rm(list = ls()); gc()
