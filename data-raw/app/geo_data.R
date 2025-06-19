# Initial setup -----------------------------------------------------------

library(dplyr)
library(stringr)

# Data prep ---------------------------------------------------------------

## Setup ------------------------------------------------------------------

url <- glue::glue("
  https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/{{dataset}}/\\
  FeatureServer/0/query?where=1%3D1&outFields={{fields}}\\
  &outSR=27700&f=json
")

create_geo_data <- function(dataset, fields) {
  sf::read_sf(
    glue::glue(
      url,
      dataset = dataset,
      fields = fields
    ),
    quiet = TRUE
  ) %>%
    rename(
      SUB_GEOGRAPHY_CODE = 1,
      SUB_GEOGRAPHY_NAME = 2,
      GEOMETRY = 3
    ) %>%
    filter(substr(SUB_GEOGRAPHY_CODE, 1, 1) == "E") %>% 
    mutate(
      SUB_GEOGRAPHY_NAME = gsub("NHS | Integrated Care Board", "", SUB_GEOGRAPHY_NAME)
    ) %>% 
    geojsonsf::sf_geojson() %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
}

create_geo_data_validation <- function(dataset, fields) {
  sf::read_sf(
    glue::glue(
      url,
      dataset = dataset,
      fields = fields
    ),
    quiet = TRUE
  ) %>%
    rename(
      SUB_GEOGRAPHY_CODE = 1,
      SUB_GEOGRAPHY_NAME = 2
    ) %>%
    as_tibble() %>% 
    select(-geometry) %>% 
    filter(substr(SUB_GEOGRAPHY_CODE, 1, 1) == "E")
}

## Download and combine data ----------------------------------------------

# All BUC boundary files @ CRS 27700
geo_data <- list(
  Region = create_geo_data(
    "NHS_England_Regions_December_2023_EN_BSC",
    "NHSER23CD,NHSER23NM"
  ),
  ICS = create_geo_data(
    "Integrated_Care_Boards_April_2023_EN_BSC",
    "ICB23CD,ICB23NM"
  ),
  `Local Authority` = create_geo_data(
    "Local_Authority_Districts_December_2023_Boundaries_UK_BSC",
    "LAD23CD,LAD23NM"
  )
)

geo_data_validation <- list(
  Region = create_geo_data_validation(
    "NHS_England_Regions_December_2023_EN_BSC",
    "NHSER23CD,NHSER23NM"
  ),
  ICB = create_geo_data_validation(
    "Integrated_Care_Boards_April_2023_EN_BSC",
    "ICB23CD,ICB23NM"
  ) %>%
    # Some small differences in naming between our data and ArcGIS; the data used
    # in the app does not need corrected as only the code is used to join to our
    # data
    mutate(
      SUB_GEOGRAPHY_NAME = str_replace(
        SUB_GEOGRAPHY_NAME,
        "Integrated Care Board",
        "ICB"
      )
    ),
  `Local Authority` = create_geo_data_validation(
    "Local_Authority_Districts_December_2023_Boundaries_UK_BSC",
    "LAD23CD,LAD23NM"
  ) %>%
    # Some small differences in naming between our data and ArcGIS; the data used
    # in the app does not need corrected as only the code is used to join to our
    # data
    mutate(
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, ", County of", ""),
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, ", City of", "")
    )
)

# Save ------------------------------------------------------------------

# Add to data
usethis::use_data(geo_data, overwrite = TRUE)
usethis::use_data(geo_data_validation, internal = TRUE, overwrite = TRUE)
