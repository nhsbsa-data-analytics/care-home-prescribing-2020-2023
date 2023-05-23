#TODO: add lat/long, add PCN

# The script creates a postcode lookup table that
# correlates with one of the three financial years.

if (fy == "2020/2021") {
  
  # Fill later
  return(NULL)
  
} else if (fy == "2021/2022") {
  
  LSOA_NHSREG = "LSOA_NHSREG2022"
  LSOA_ICB = "LSOA_ICB2022"
  # This mapping is available only up to 2021 at the time of running (May 2023)
  # Ideally, we'd use the mapping as of 2022, but since that is available, we'll use the 2021 version
  # Steven says there doesn't appear to be a change from 2021 to 2022: same number of LAD records and same codes
  LSOA_LAD = "LSOA_LAD2021"
  TABLE_SUFFIX="2022"
  
} else if (fy == "2022/2023") {
  
  # Fill later
  return(NULL)
  
}

# Note:
# IMD Year is hard-coded at 2019 at the moment, since it's updated every 5 years

# Execute the script only if the workflow function calls it for a supported FY
if (exists("TABLE_SUFFIX")) {

library(dplyr)
library(dbplyr)

con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography mapping table
geography_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_GEOGRAPHY_MAPPING"))

# Create a lazy table from the postcode data table
postcode_db <- con %>%
  tbl(from = in_schema("DIM", sql("ONS_POSTCODE_DATA_DIM@DWCP.WORLD")))

# Create a lazy table for IMD data
imd_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_INDEX_OF_MULTIPLE_DEPRIVATION"))

# Get the latest POSTCODE-LSOA mappings within the target FY
year_month_db <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM")) %>%
  filter(FINANCIAL_YEAR==fy) %>%
  select(YEAR_MONTH)

postcode_db <- postcode_db %>%
  inner_join(year_month_db) %>%
  group_by(POSTCODE) %>%
  window_order(desc(YEAR_MONTH)) %>%
  mutate(RANK = rank()) %>%
  filter(RANK == 1) %>%
  select(POSTCODE, LSOA_CODE = CENSUS_LOWER, YEAR_MONTH) %>%
  addressMatchR::tidy_postcode(POSTCODE)
# At the time of running (May 2023) the latest YEAR_MONTH = 202211

# Join to the postcode lookup to get NHS Region, ICB and LA based on their mappings to LSOAs
postcode_db <- postcode_db %>%
  # ICB
  left_join(
    y = geography_db %>%
      filter(RELATIONSHIP == LSOA_ICB) %>%
      select(
        LSOA_CODE = CHILD_ONS_CODE,
        PCD_ICB_CODE = PARENT_ONS_CODE,
        PCD_ICB_NAME = PARENT_NAME
      )
  ) %>%
  # LA
  left_join(
    y = geography_db %>%
      filter(RELATIONSHIP == LSOA_LAD) %>% # 
      select(
        LSOA_CODE = CHILD_ONS_CODE,
        PCD_LAD_CODE = PARENT_ONS_CODE,
        PCD_LAD_NAME = PARENT_NAME
      )
  ) %>%
  # NHS Region
  left_join(
    y = geography_db %>%
      filter(RELATIONSHIP == LSOA_NHSREG) %>%
      select(
        LSOA_CODE = CHILD_ONS_CODE,
        PCD_REGION_CODE = PARENT_ONS_CODE,
        PCD_REGION_NAME = PARENT_NAME
      )
  ) %>%
  # Index of Multiple Deprivation
  left_join(
    y = imd_db %>%
      filter(IMD_YEAR == 2019) %>%
      select(LSOA_CODE, IMD_DECILE = INDEX_OF_MULT_DEPRIV_DECILE)
  )

# Reorder the columns
postcode_db <- postcode_db %>%
  select(
    POSTCODE,
    PCD_REGION_CODE,
    PCD_REGION_NAME,
    PCD_ICB_CODE,
    PCD_ICB_NAME,
    PCD_LAD_CODE,
    PCD_LAD_NAME,
    IMD_DECILE
  )

# Write the table back to the DB with indexes
table_name = paste0("INT646_POSTCODE_LOOKUP_",TABLE_SUFFIX)
drop_table_if_exists_db(table_name)
postcode_db %>%
  compute(
    name = table_name,
    indexes = "POSTCODE",
    temporary = F
  )

# Disconnect from database
DBI::dbDisconnect(con)

} else {
  print("Error: unsupported financial year")
}
