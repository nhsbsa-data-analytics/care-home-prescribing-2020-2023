# The script creates a postcode lookup table using the latest available mappings
library(dplyr)
library(dbplyr)

# Postcodes are mapped to REG/ICB/LAD mappings via LSOAs
# The mappings from https://geoportal.statistics.gov.uk feed DALL_REF.ONS_GEOGRAPHY_MAPPING
LSOA_NHSREG = "LSOA21_NHSREG2023"
LSOA_ICB = "LSOA21_ICB2023"
LSOA_LAD = "LSOA21_LAD2023"

# Connect to dalp
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the geography mapping table
geography_db <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_GEOGRAPHY_MAPPING"))

# Create a lazy table from the postcode data table
postcode_db <- con %>%
  tbl(from = in_schema("DIM", sql("ONS_POSTCODE_DATA_DIM")))

# Single record per postcode (7200 records have NA lsoa code information)
postcode_db <- postcode_db %>%
  filter(COUNTRY_CODE == "E92000001") %>%
  simple_format_postcode_db(POSTCODE) %>%
  group_by(POSTCODE) %>%
  window_order(desc(YEAR_MONTH)) %>%
  mutate(RANK = rank()) %>%
  filter(RANK == 1) %>%
  ungroup() %>% 
  select(POSTCODE, LSOA_CODE = LSOA21_CODE, YEAR_MONTH)

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
  # Reorder the columns
  select(
    POSTCODE,
    PCD_REGION_CODE,
    PCD_REGION_NAME,
    PCD_ICB_CODE,
    PCD_ICB_NAME,
    PCD_LAD_CODE,
    PCD_LAD_NAME
  )

# Define table name
table_name = "INT646_POSTCODE_LOOKUP"

# Drop table if already exists
drop_table_if_exists_db(table_name)

# Write the table back to the DB with indexes
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
