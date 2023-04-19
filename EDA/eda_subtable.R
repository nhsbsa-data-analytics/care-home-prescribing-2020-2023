# Libraries
library(dplyr)
library(dbplyr)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from year month dim table in DWCP
data <- con %>%
  tbl(from = in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

# Create parent uprn groups
data = data %>% 
  filter(UPRN_FLAG == 1) %>% 
  mutate(
    MATCH_SLA_STD = coalesce(MATCH_SLA_PARENT, MATCH_SLA_STD),
    UPRN = coalesce(PARENT_UPRN, UPRN)
  ) %>% 
  group_by(UPRN) %>% 
  mutate(MATCH_SLA_STD = max(MATCH_SLA_STD)) %>% 
  ungroup()

# Write the table back to DALP
data %>%
  compute(
    name = "INT646_MATCH_SLA",
    temporary = FALSE
  )

# Disconnect
DBI::dbDisconnect(con)