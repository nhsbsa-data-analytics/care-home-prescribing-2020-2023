
# Running time ~10 min
library(dplyr)
library(dbplyr)
library(assertr)
library(assertr.alt)
devtools::load_all()


base_table <- "INT646_BASE_20200401_20250331"
start_year <- substring(base_table, 13, 16)
end_year <- substring(base_table, 22, 25)
EXPECTED_YEARS <- as.integer(end_year) - as.integer(start_year)
EXPECTED_MONTHS <- 12 * EXPECTED_YEARS

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Item-level base table
base_db <- con |>
  tbl(from = in_schema("DALL_REF", base_table)) %>%
  verify(nrow.alt(distinct(., FY)) == EXPECTED_YEARS) %>%
  verify(nrow.alt(distinct(., YEAR_MONTH)) == EXPECTED_MONTHS)


# Row validation calculation ----------------------------------------------

distinct_counts <- base_db %>% 
  summarise(
    across(
      c(CH_FLAG, FY, (starts_with("PCD") & ends_with("CODE")), AGE_BAND, GENDER),
      n_distinct
    )
  ) %>%
  collect() %>% 
  rename_with(\(x) glue::glue("EXPECTED_{x}S")) %>% 
  as.list() %>% 
  purrr::iwalk(
    \(x, idx) assign(idx, x, envir = .GlobalEnv)
  )

EXPECTED_ROWS <- EXPECTED_YEARS *
  EXPECTED_CH_FLAGS * (
    1 +                                 # 1 for OVERALL
      (EXPECTED_PCD_REGION_CODES + 1) + # + 1 for NA
      (EXPECTED_PCD_ICB_CODES + 1) +    # + 1 for NA
      (EXPECTED_PCD_LAD_CODES + 1)      # + 1 for NA
  ) * (
    EXPECTED_AGE_BANDS *
      EXPECTED_GENDERS +
      1                                 # + 1 for AGE_BAND = NA, GENDER = Unknown
  )
  
 
# Add a dummy overall column
base_db <- base_db |>
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
      CH_FLAG,
      FY,
      GEOGRAPHY = geography_name,
      across(all_of(unname(geography_cols))),
      GENDER,
      AGE_BAND
    ) |>
    rename(!!!geography_cols) |>
    mutate(
      # If the SUB_GEOGRAPHY_NAME is NA then set gender to NA
      GENDER = ifelse(is.na(SUB_GEOGRAPHY_NAME), "Unknown", GENDER),
      # If either SUB_GEOGRAPHY_NAME is NA or GENDER is NA then set AGE_BAND to NA
      AGE_BAND = ifelse(
       is.na(SUB_GEOGRAPHY_NAME) | is.na(GENDER),
       NA,
       AGE_BAND
      )
    ) |>
    summarise(TOTAL_PATIENTS = n_distinct(NHS_NO)) |>
    ungroup(GENDER, AGE_BAND, TOTAL_PATIENTS) |>
    #mutate(PCT_PATIENTS = TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100) |> # Now calculated based on rounded values outside SQL
    ungroup() |>
    nhsbsaR::collect_with_parallelism(32)
  
}

# Map function
patients_by_fy_geo_age_gender_df <- purrr::map(
  names(geographies),
  patients_by_fy_geo_age_gender_fun
  ) |>
  purrr::list_rbind()


# Apply rounding
patients_by_fy_geo_age_gender_df <-
  patients_by_fy_geo_age_gender_df |>
  mutate(
    TOTAL_PATIENTS = bespoke_round(TOTAL_PATIENTS)
    #PCT_PATIENTS = janitor::round_half_up(PCT_PATIENTS, 1)
  )

# Calculate patient proportions
patients_by_fy_geo_age_gender_df <- patients_by_fy_geo_age_gender_df |>
  group_by(CH_FLAG, FY, GEOGRAPHY, SUB_GEOGRAPHY_CODE, SUB_GEOGRAPHY_NAME) |>
  mutate(
    PCT_PATIENTS = janitor::round_half_up(TOTAL_PATIENTS / sum(TOTAL_PATIENTS) * 100, 1)
  ) |>
  ungroup()

# Get all the possible combinations
patients_by_fy_geo_age_gender_df <-
  patients_by_fy_geo_age_gender_df |>
  tidyr::complete(
    CH_FLAG,
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

# Format factors etc and sort
patients_by_fy_geo_age_gender_df <-
  patients_by_fy_geo_age_gender_df |>
  format_data_raw(
    vars = c("GENDER", "AGE_BAND")
  )

# Clean some geographic names
patients_by_fy_geo_age_gender_df <-patients_by_fy_geo_age_gender_df |>
  mutate(SUB_GEOGRAPHY_NAME = stringr::str_remove(SUB_GEOGRAPHY_NAME, " ICB$")) |>
  mutate(SUB_GEOGRAPHY_NAME = stringr::str_remove(SUB_GEOGRAPHY_NAME, "^NHS ")) %>%
  verify(nrow.alt(.) == EXPECTED_ROWS) %>% 
  assert.alt(not_na.alt, TOTAL_PATIENTS, PCT_PATIENTS)

# Add to data/
usethis::use_data(
  patients_by_fy_geo_age_gender_df,
  overwrite = T
)

# Disconnect from database
DBI::dbDisconnect(con)

# Remove vars specific to script
remove_vars <- setdiff(ls(), keep_vars)

# Remove objects and clean environment
rm(list = remove_vars, remove_vars); gc()
