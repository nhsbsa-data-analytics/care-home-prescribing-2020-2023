library(dplyr)
library(dbplyr)

# Set up connection to the DB
con_dalp <- nhsbsaR::con_nhsbsa(database = "DALP")
con_dwcp <- nhsbsaR::con_nhsbsa(database = "DWCP")

# Part One: Create postcodes --------------------------------------------------

# Create a lazy table from the CQC care home table
cqc_db <- con_dalp %>%
  tbl(from = "INT646_CQC_202301")

# Convert registration and deregistration columns within 2021/22
cqc_db <- cqc_db %>%
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
    REGISTRATION_DATE <= TO_DATE("2022-03-31", "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) |
      DEREGISTRATION_DATE >= TO_DATE("2021-04-01", "YYYY-MM-DD"),
    !is.na(UPRN)
  ) %>% 
  addressMatchR::tidy_postcode(col = POSTAL_CODE) %>%
  select(POSTAL_CODE) %>% 
  distinct()

# Part Two: process Addressbase data -------------------------------------------

# Create a lazy table from the AddressBase Plus table
addressbase_plus_db <- con_dwcp %>%
  tbl(from = in_schema("SCD2", "SCD2_OS_ADDRESS_BASE_DATA"))

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
    y = cqc_uprn_postcode_address_db %>%
      select(POSTCODE),
    # Due to differing data sources
    copy = TRUE,
    overwrite = TRUE
  )

# Filter AddressBase Plus to postcodes where there is a care home present
addressbase_plus_db <- addressbase_plus_db %>%
  semi_join(y = care_home_postcodes_db)

# Create and tidy the DPA and GEO single line addresses
addressbase_plus_db <- addressbase_plus_db %>%
  # Rename required as OS table names have changed
  rename(
    DEP_THOROUGHFARE = DEPENDENT_THOROUGHFARE,
    DOU_DEP_LOCALITY = DOUBLE_DEPENDENT_LOCALITY,
    DEP_LOCALITY = DEPENDENT_LOCALITY
  ) %>% 
  addressMatchR::calc_addressbase_plus_dpa_single_line_address() %>%
  addressMatchR::calc_addressbase_plus_geo_single_line_address() %>%
  addressMatchR::tidy_single_line_address(col = DPA_SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_single_line_address(col = GEO_SINGLE_LINE_ADDRESS) %>%
  select(
    UPRN,
    POSTCODE,
    DPA_SINGLE_LINE_ADDRESS,
    GEO_SINGLE_LINE_ADDRESS,
    CH_FLAG
  )

# When DPA != GEO then add a CORE single line address
addressbase_plus_db <-
  union_all(
    x = addressbase_plus_db %>%
      filter(
        is.na(DPA_SINGLE_LINE_ADDRESS) |
          is.na(GEO_SINGLE_LINE_ADDRESS) |
          DPA_SINGLE_LINE_ADDRESS == GEO_SINGLE_LINE_ADDRESS
      ),
    y = addressbase_plus_db %>%
      filter(
        !is.na(DPA_SINGLE_LINE_ADDRESS),
        !is.na(GEO_SINGLE_LINE_ADDRESS),
        DPA_SINGLE_LINE_ADDRESS != GEO_SINGLE_LINE_ADDRESS
      ) %>%
      nhsbsaR::oracle_merge_strings(
        first_col = "DPA_SINGLE_LINE_ADDRESS",
        second_col = "GEO_SINGLE_LINE_ADDRESS",
        merge_col = "CORE_SINGLE_LINE_ADDRESS"
      )
  )

# Part Three: Save as table in dw ----------------------------------------------

# Drop any existing table beforehand
if(DBI::dbExistsTable(conn = con_dalp, name = "INT646_ADDRESSBASE") == T){
  DBI::dbRemoveTable(conn = con_dalp, name = "INT646_ADDRESSBASE")
}

# Write the table back to the DB with indexes
addressbase_plus_db %>%
  compute(
    name = "INT646_ADDRESSBASE",
    indexes = list(c("UPRN", c("POSTCODE"))), # single line address too long
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con_dalp)
DBI::dbDisconnect(con_dwcp)