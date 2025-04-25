# The script creates a postcode lookup table using the latest available mappings

library(dplyr)
library(dbplyr)

# Postcodes are mapped to REG/ICB/LAD mappings via LSOAs
# Currently PCD-LSOA mappings are available in the DWCP, but include only the old
# 2011-LSOA structure. The LSOA structure was updated in 2021 and is entirely different.
# The mappings from https://geoportal.statistics.gov.uk which feed DALL_REF.ONS_GEOGRAPHY_MAPPING
# have switched to using the 2021-LSOA structure in recent years, so we cannot use the
# newer LSOA->REG/ICB/LAD mappings.
# The latest mappings which use the 2011-LSOAs (i.e. usable here) are hard-coded below:

# LSOA_NHSREG = "LSOA_NHSREG2022"
# LSOA_ICB = "LSOA_ICB2022"
# LSOA_LAD = "LSOA_LAD2021"
LSOA_NHSREG = "LSOA21_NHSREG2023"
LSOA_ICB = "LSOA21_ICB2023"
LSOA_LAD = "LSOA21_LAD2023"


con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography mapping table
geography_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_GEOGRAPHY_MAPPING"))

# Create a lazy table from the postcode data table
postcode_db <- con %>%
  tbl(from = in_schema("DIM", sql("ONS_POSTCODE_DATA_DIM")))

# Create a lazy table from postcode to lat-long mappings
postcode_latlong <- con %>%
  tbl(from = in_schema("DIM", sql("ONS_POSTCODE_LAT_LON_DIM")))

# Create a lazy table for IMD data
imd_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_INDEX_OF_MULTIPLE_DEPRIVATION"))

postcode_db <- postcode_db %>%
  filter(COUNTRY_CODE == "E92000001") %>%
  simple_format_postcode_db(POSTCODE) %>%
  group_by(POSTCODE) %>%
  window_order(desc(YEAR_MONTH)) %>%
  mutate(RANK = rank()) %>%
  filter(RANK == 1) %>%
  ungroup() %>% 
  select(POSTCODE, LSOA_CODE = CENSUS_LOWER, YEAR_MONTH, PCD_NORTHING = OSNRTH1M, PCD_EASTING = OSEAST1M) 

postcode_latlong <- postcode_latlong %>%
  simple_format_postcode_db(POSTCODE) %>%
  group_by(POSTCODE) %>%
  window_order(desc(YEAR_MONTH)) %>%
  mutate(RANK = rank()) %>%
  filter(RANK == 1) %>%
  ungroup() %>%
  select(POSTCODE, PCD_LAT = LATITUDE, PCD_LONG = LONGITUDE)
  
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
      ),
    by = "LSOA_CODE"
  ) %>%
  # LA
  left_join(
    y = geography_db %>%
      filter(RELATIONSHIP == LSOA_LAD) %>% 
      select(
        LSOA_CODE = CHILD_ONS_CODE,
        PCD_LAD_CODE = PARENT_ONS_CODE,
        PCD_LAD_NAME = PARENT_NAME
      ),
    by = "LSOA_CODE"
  ) %>%
  # NHS Region
  left_join(
    y = geography_db %>%
      filter(RELATIONSHIP == LSOA_NHSREG) %>%
      select(
        LSOA_CODE = CHILD_ONS_CODE,
        PCD_REGION_CODE = PARENT_ONS_CODE,
        PCD_REGION_NAME = PARENT_NAME
      ),
    by = "LSOA_CODE"
  ) %>%
  # Latitude & Longitude
  left_join(postcode_latlong, by = "POSTCODE") %>%
  # Index of Multiple Deprivation
  left_join(
    y = imd_db %>%
      filter(IMD_YEAR == 2019) %>%
      select(LSOA_CODE, IMD_DECILE = INDEX_OF_MULT_DEPRIV_DECILE),
    by = "LSOA_CODE"
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
    PCD_LAT,
    PCD_LONG,
    PCD_NORTHING,
    PCD_EASTING,
    IMD_DECILE
  )

# Write the table back to the DB with indexes
table_name = "INT646_POSTCODE_LOOKUP"
drop_table_if_exists_db(table_name)
postcode_db %>%
  compute(
    name = table_name,
    indexes = "POSTCODE",
    temporary = F
  )

# Grant access
c("MIGAR", "ADNSH", "MAMCP") %>% grant_table_access (table_name)

# Disconnect connection to database
DBI::dbDisconnect(con)

# Print that table has been created
print(paste0("This script has created table: ", table_name))

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
