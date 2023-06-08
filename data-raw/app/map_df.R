library(dplyr)

# All BUC boundary files @ CRS 27700
map_df <- bind_rows(
  
  # Region
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Regions_December_2022_EN_BUC/FeatureServer/0/query?where=1%3D1&outFields=RGN22CD,RGN22NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "Region") %>%
    select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_CODE = RGN22CD,
      SUB_GEOGRAPHY_NAME = RGN22NM,
      GEOMETRY = geometry
    ),
  
  # STP
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ICB_JUL_2022_EN_BUC_V3/FeatureServer/0/query?where=1%3D1&outFields=ICB22CD,ICB22NM&outSR=27700&f=json") %>%
    mutate(GEOGRAPHY = "STP/ICS") %>%
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

# Add to data
usethis::use_data(map_df, overwrite = TRUE)
