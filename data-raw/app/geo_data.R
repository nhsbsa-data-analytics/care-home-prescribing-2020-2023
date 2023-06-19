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
    "NHS_England_Regions_July_2022_EN_BUC_2022",
    "NHSER22CD,NHSER22NM"
  ),
  ICB = create_geo_data(
    "ICB_JUL_2022_EN_BUC_V3",
    "ICB22CD,ICB22NM"
  ),
  `Local Authority` = create_geo_data(
    "Local_Authority_Districts_December_2021_GB_BUC_2022",
    "LAD21CD,LAD21NM"
  )
)

geo_data_validation <- list(
  Region = create_geo_data_validation(
    "NHS_England_Regions_July_2022_EN_BUC_2022",
    "NHSER22CD,NHSER22NM"
  ),
  ICB = create_geo_data_validation(
    "ICB_JUL_2022_EN_BUC_V3",
    "ICB22CD,ICB22NM"
  ) %>%
    # Some small differences in ICB data between ours and ArcGIS; the data used
    # in the app does not need corrected as only the code is used to join to our
    # data
    mutate(
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, "Integrated Care Board", "ICB"),
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, " the ", " The "),
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, " of ", " Of "),
    ),
  `Local Authority` = create_geo_data_validation(
    "Local_Authority_Districts_December_2021_GB_BUC_2022",
    "LAD21CD,LAD21NM"
  )
)

# Save ------------------------------------------------------------------

# Add to data
usethis::use_data(geo_data, overwrite = TRUE)
usethis::use_data(geo_data_validation, internal = TRUE, overwrite = TRUE)
