# Running time ~10 min

library(dplyr)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |>
  tbl(from = in_schema("DALL_REF", "INT646_BASE_20200401_20230331"))
 
# Filter to care home only and add a dummy overall column
base_db <- base_db |>
   filter(CH_FLAG == 1L) |>
   mutate(OVERALL = "Overall")
 
# Loop over each geography and aggregate using purrr's map function approach

patients_by_fy_geo_age_gender_fun <- function(geography_name) {
  
  # Identify geography cols
  geography_cols <-
    geographies[[geography_name]] %>%
    purrr::set_names(
      nm = stringr::str_replace(
        string = names(.),
        pattern = "BREAKDOWN",
        replacement = "GEOGRAPHY"
      )
    )
  
  # Overall total patients
  base_db |>
    group_by(
      FY,
      GEOGRAPHY = geography_name,
      across(all_of(unname(geography_cols))),
      GENDER,
      AGE_BAND
    ) |>
    rename(!!!geography_cols) |>
    mutate(
      # If the SUB_GEOGRAPHY_NAME is NA then set gender to NA
      GENDER = ifelse(is.na(SUB_GEOGRAPHY_NAME), NA, GENDER),
      # If either SUB_GEOGRAPHY_NAME is NA or GENDER is NA then set AGE_BAND to NA
      AGE_BAND = ifelse(
        test = is.na(SUB_GEOGRAPHY_NAME) | is.na(GENDER),
        yes = NA,
        AGE_BAND
      )
    ) |>
    summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) |>
    ungroup(GENDER, AGE_BAND, TOTAL_PATIENTS) |>
    mutate(PCT_PATIENTS = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) |>
    ungroup() |>
    nhsbsaR::collect_with_parallelism(32)
  
}

patients_by_fy_geo_age_gender_df <- purrr::map(
  names(geographies),
  patients_by_fy_geo_age_gender_fun
  ) |>
  purrr::list_rbind()

# Get all the possible combinations
patients_by_fy_geo_age_gender_df <-
  patients_by_fy_geo_age_gender_df |>
  tidyr::complete(
    # Only geographies that already exist
    tidyr::nesting(FY, GEOGRAPHY, SUB_GEOGRAPHY_CODE, SUB_GEOGRAPHY_NAME),
    # Only age band and gender combinations that exist (so we only have NA age
    # band for NA genders)
    tidyr::nesting(AGE_BAND, GENDER),
    fill = list(
      TOTAL_PATIENTS = 0L,
      PCT_PATIENTS = 0
    )
  )

# Apply SDC to total patients and percentage of patients
patients_by_fy_geo_age_gender_df <-
  patients_by_fy_geo_age_gender_df |>
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(TOTAL_PATIENTS, -1)),
    SDC_PCT_PATIENTS =
      ifelse(SDC == 1, NA_integer_, janitor::round_half_up(PCT_PATIENTS, 1))
  ) |>
  select(-SDC)

# Format factors etc and sort
patients_by_fy_geo_age_gender_df <-
  patients_by_fy_geo_age_gender_df |>
  format_data_raw(
    vars = c("GENDER", "AGE_BAND")
  )


# Clean some names
patients_by_fy_geo_age_gender_df <-
  patients_by_fy_geo_age_gender_df |>
    # mutate(SUB_GEOGRAPHY_NAME = stringr::str_remove(SUB_GEOGRAPHY_NAME, " \\(as of \\d{6}\\)$")) |>
    # mutate(SUB_GEOGRAPHY_NAME = stringr::str_remove(SUB_GEOGRAPHY_NAME, " PCN$")) |>
    mutate(SUB_GEOGRAPHY_NAME = stringr::str_remove(SUB_GEOGRAPHY_NAME, " ICB$")) |>
    mutate(SUB_GEOGRAPHY_NAME = stringr::str_remove(SUB_GEOGRAPHY_NAME, "^NHS "))
  

# Add to data/
usethis::use_data(
  patients_by_fy_geo_age_gender_df,
  overwrite = T
)

# Disconnect from database
DBI::dbDisconnect(con)
rm(list = ls()); gc()
