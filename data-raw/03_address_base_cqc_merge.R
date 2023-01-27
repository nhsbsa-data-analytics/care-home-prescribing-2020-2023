library(dplyr)
library(dbplyr)

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Part One: process cqc data ---------------------------------------------------

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = "INT646_CQC_202301")

# Convert registration and deregistration columns to dates and filter to 2020/21
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
  addressMatchR::tidy_postcode(col = POSTAL_CODE)

# Create a tidy single line address and postcode
cqc_db <- cqc_db %>%
  mutate(
    SINGLE_LINE_ADDRESS = paste(
      NAME,
      POSTAL_ADDRESS_LINE1,
      POSTAL_ADDRESS_LINE2,
      POSTAL_ADDRESS_TOWN_CITY,
      POSTAL_ADDRESS_COUNTY
    )
  ) %>%
  addressMatchR::tidy_single_line_address(col = SINGLE_LINE_ADDRESS) %>%
  addressMatchR::tidy_postcode(col = POSTAL_CODE) %>%
  rename(POSTCODE = POSTAL_CODE)

# Convert to a distinct postcode and single line address table by taking the
# max of the attribute columns
cqc_uprn_postcode_address_db <- cqc_db %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
    # Change UPRN to numeric and loose 2 doing max
    UPRN = max(as.integer(UPRN), na.rm = TRUE),
    UPRN = ifelse(is.infinite(UPRN), NA, UPRN),
    NURSING_HOME_FLAG = max(as.integer(NURSING_HOME), na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(as.integer(RESIDENTIAL_HOME), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  relocate(UPRN, LOCATION_ID)

# Part Two: process Addressbase data -------------------------------------------

# Create a lazy table from the AddressBase Plus table
addressbase_plus_db <- con %>% 
  tbl(from = "INT646_ADDRESSBASE")

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
  ) %>% 
  mutate(UPRN = as.double(UPRN))

# Part three: Combine AddressBase Plus (care home postcodes) and CQC -----------

# Join the CQC attributes to existing UPRNs (take max flags when duplicate UPRN)
addressbase_plus_cqc_db <- addressbase_plus_db %>%
  left_join(
    y = cqc_uprn_postcode_address_db %>%
      group_by(UPRN) %>%
      summarise(
        LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
        NURSING_HOME_FLAG = max(NURSING_HOME_FLAG, na.rm = TRUE),
        RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG, na.rm = TRUE)
      ) %>%
      ungroup()
  )

# Convert to a long table of distinct stacked single line addresses
addressbase_plus_cqc_db <- addressbase_plus_cqc_db %>%
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>%
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  select(-ADDRESS_TYPE) %>%
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE)

# Stack the CQC data and make distinct (take max row)
addressbase_plus_cqc_db <- addressbase_plus_cqc_db %>%
  union_all(y = cqc_uprn_postcode_address_db %>% mutate(CH_FLAG = 1L)) %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_max(order_by = UPRN, with_ties = FALSE) %>%
  ungroup()

# Part Four: Save as table in dw -----------------------------------------------

# Drop any existing table beforehand
if(DBI::dbExistsTable(conn = con, name = "INT646_ADDRESSBASE_PLUS_CQC") == T){
  DBI::dbRemoveTable(conn = con, name = "INT646_ADDRESSBASE_PLUS_CQC")
}

# Write the table back to the DB with indexes
addressbase_plus_cqc_db %>%
  compute(
    name = "INT646_ADDRESSBASE_PLUS_CQC",
    indexes = list(c("UPRN", c("POSTCODE"))), # single line address too long
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()
