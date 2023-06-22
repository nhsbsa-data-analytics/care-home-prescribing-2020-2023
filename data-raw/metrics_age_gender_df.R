# Running time ~x min

library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |>
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))

# Add to data/
# usethis::use_data(
#   patients_by_fy_geo_age_gender_df,
#   overwrite = T
# )

# Disconnect from database
DBI::dbDisconnect(con)
