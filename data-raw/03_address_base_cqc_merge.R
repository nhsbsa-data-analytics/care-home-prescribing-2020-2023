
# Load packages and global variables
source("R/analysis_packages.R")

# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the CQC care home table
cqc_db <- con %>%
  tbl(from = "INT646_CQC_202301")

# Create a lazy table addressbase data in scd2
ab_plus_db <- con %>%
  tbl(from = "INT646_AB_PLUS_202204")

# Part one: Process cqc data ---------------------------------------------------

cqc_db = cqc_db %>% 
  mutate(
    REGISTRATION_DATE = TO_DATE(REGISTRATION_DATE, "YYYY-MM-DD"),
    DEREGISTRATION_DATE = TO_DATE(DEREGISTRATION_DATE, "YYYY-MM-DD")
  ) %>% 
  filter(
    !is.na(UPRN),
    REGISTRATION_DATE <= TO_DATE(end_date, "YYYY-MM-DD"),
    is.na(DEREGISTRATION_DATE) | 
      DEREGISTRATION_DATE >= TO_DATE(start_date, "YYYY-MM-DD")
  ) %>% 
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  summarise(
    UPRN = max(as.integer(UPRN), na.rm = TRUE),
    NURSING_HOME_FLAG = max(as.integer(NURSING_HOME_FLAG), na.rm = TRUE),
    RESIDENTIAL_HOME_FLAG = max(as.integer(RESIDENTIAL_HOME_FLAG), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  relocate(UPRN, LOCATION_ID)

# Part Two: Process ab plus data -----------------------------------------------

# When DPA != GEO then add a CORE single line address
# NOTE: REQUIRES EDITING!
# ab_plus_db <-
#   union_all(
#     x = ab_plus_db %>%
#       filter(
#         is.na(DPA_SINGLE_LINE_ADDRESS) |
#           is.na(GEO_SINGLE_LINE_ADDRESS) |
#           DPA_SINGLE_LINE_ADDRESS == GEO_SINGLE_LINE_ADDRESS
#       ),
#     y = ab_plus_db %>%
#       filter(
#         !is.na(DPA_SINGLE_LINE_ADDRESS),
#         !is.na(GEO_SINGLE_LINE_ADDRESS),
#         DPA_SINGLE_LINE_ADDRESS != GEO_SINGLE_LINE_ADDRESS
#       ) %>%
#       nhsbsaR::oracle_merge_strings(
#         first_col = "DPA_SINGLE_LINE_ADDRESS",
#         second_col = "GEO_SINGLE_LINE_ADDRESS",
#         merge_col = "CORE_SINGLE_LINE_ADDRESS"
#       )
#   ) %>% 
#   mutate(UPRN = as.double(UPRN))

# Part three: Combine AddressBase Plus (care home postcodes) and CQC -----------

# Join the CQC attributes to existing UPRNs (take max flags when duplicate UPRN)
ab_plus_cqc_db <- ab_plus_db %>%
  left_join(
    y = cqc_db %>%
      group_by(UPRN) %>%
      summarise(
        LOCATION_ID = max(LOCATION_ID, na.rm = TRUE),
        NURSING_HOME_FLAG = max(NURSING_HOME_FLAG, na.rm = TRUE),
        RESIDENTIAL_HOME_FLAG = max(RESIDENTIAL_HOME_FLAG, na.rm = TRUE)
      ) %>%
      ungroup()
  )

# Convert to a long table of distinct stacked single line addresses
ab_plus_cqc_db <- ab_plus_cqc_db %>%
  tidyr::pivot_longer(
    cols = ends_with("SINGLE_LINE_ADDRESS"),
    names_to = "ADDRESS_TYPE",
    values_to = "SINGLE_LINE_ADDRESS"
  ) %>%
  filter(!is.na(SINGLE_LINE_ADDRESS)) %>%
  select(-ADDRESS_TYPE) %>%
  relocate(SINGLE_LINE_ADDRESS, .after = POSTCODE)

# Stack the CQC data and make distinct (take max row)
ab_plus_cqc_db <- ab_plus_cqc_db %>%
  union_all(y = cqc_db %>% mutate(CH_FLAG = 1L)) %>%
  group_by(POSTCODE, SINGLE_LINE_ADDRESS) %>%
  slice_max(order_by = UPRN, with_ties = FALSE) %>%
  ungroup()

# Part Four: Save as table in dw -----------------------------------------------

# Drop any existing table beforehand
if(DBI::dbExistsTable(conn = con, name = "INT646_AB_PLUS_CQC") == T){
  DBI::dbRemoveTable(conn = con, name = "INT646_AB_PLUS_CQC")
}
tictoc::tic()
# Write the table back to the DB with indexes
ab_plus_cqc_db %>%
  compute(
    name = "INT646_AB_PLUS_CQC",
    indexes = list(c("UPRN", c("POSTCODE"))),
    temporary = FALSE
  )

# Disconnect from database
DBI::dbDisconnect(con)

# Remove objects and clean environment
rm(list = ls()); gc()
