# Initial setup -----------------------------------------------------------

library(dplyr)

# Data prep ---------------------------------------------------------------

## Download and combine data ----------------------------------------------

# All BUC boundary files @ CRS 27700
map_df <- bind_rows(
  
  # Region
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/NHS_England_Regions_July_2022_EN_BUC_2022/FeatureServer/0/query?where=1%3D1&outFields=NHSER22CD,NHSER22NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "Region") %>%
    select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE = NHSER22CD,
      SUB_GEOGRAPHY_NAME = NHSER22NM,
      GEOMETRY = geometry
    ),
  
  # STP
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ICB_JUL_2022_EN_BUC_V3/FeatureServer/0/query?where=1%3D1&outFields=ICB22CD,ICB22NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "ICB") %>%
    select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE = ICB22CD,
      SUB_GEOGRAPHY_NAME = ICB22NM,
      GEOMETRY = geometry
    ),
  
  # Local Authority
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2021_GB_BUC_2022/FeatureServer/0/query?where=1%3D1&outFields=LAD21CD,LAD21NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "Local Authority") %>%
    select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE = LAD21CD,
      SUB_GEOGRAPHY_NAME = LAD21NM,
      GEOMETRY = geometry
    ) %>%
    filter(substr(SUB_GEOGRAPHY_CODE, 1, 1) == "E")
)

## Clean data -------------------------------------------------------------

# Replace non-ascii chars in CRS
sf::st_crs(map_df)$wkt <- sf::st_crs(map_df)$wkt %>%
  stringr::str_replace_all("°", "\u00B0")

## Save ------------------------------------------------------------------

# Add to data
usethis::use_data(map_df, overwrite = TRUE)
