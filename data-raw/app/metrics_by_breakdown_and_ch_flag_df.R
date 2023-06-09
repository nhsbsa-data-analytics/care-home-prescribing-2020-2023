library(tidyverse)
library(glue)
library(dbplyr)
devtools::load_all()

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

#################


PCD <- con %>%
  tbl(from = "INT646_POSTCODE_LOOKUP") %>% 
  select(ends_with("CODE"), ends_with("NAME")) %>%
  distinct() %>%
  collect()

transform_PCD <- function(data, geography) {
  data %>%
    select(starts_with(glue("PCD_{geography}"))) %>%
    distinct() %>%
    rename_with(\(x) str_replace(x, glue("PCD_{geography}"), "SUB_GEOGRAPHY"))
}

PCD_list <- list(
  REGION = PCD %>% transform_PCD("REGION"),
  ICB    = PCD %>% transform_PCD("ICB"),
  LAD    = PCD %>% transform_PCD("LAD")
)

GIS_all <- map_df %>% as_tibble() %>% select(-GEOMETRY)

GIS_list <- list(
  REGION = GIS_all %>% filter(GEOGRAPHY == "Region"),
  ICB    = GIS_all %>%
    filter(GEOGRAPHY == "ICB") %>%
    mutate(
      SUB_GEOGRAPHY_NAME = str_replace(SUB_GEOGRAPHY_NAME, "Integrated Care Board", "ICB")
    ),
  LAD    = GIS_all %>% filter(GEOGRAPHY == "Local Authority")
)

# Check sub-geography codes

check_sub_geo_codes <- list(
  in_GIS_only = map2(
    GIS_list,
    PCD_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
  ),
  in_PCD_only = map2(
    PCD_list,
    GIS_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_CODE, y$SUB_GEOGRAPHY_CODE)
  )
)

# Check sub-geography names

check_sub_geo_names <- list(
  in_GIS_only = map2(
    GIS_list,
    PCD_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
  ),
  in_PCD_only = map2(
    PCD_list,
    GIS_list,
    \(x, y) setdiff(x$SUB_GEOGRAPHY_NAME, y$SUB_GEOGRAPHY_NAME)
  )
)



#################

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = "INT646_BASE_20210401_20220331")

# Add a dummy overall column
fact_db <- fact_db %>%
  mutate(OVERALL = "Overall") # dummy col

# Loop over each breakdown and aggregate
for (breakdown_name in names(breakdowns)) {
  
  # Extract the breakdown cols
  breakdown_cols <- breakdowns[[breakdown_name]]
  
  # Group the table
  tmp_db <- fact_db %>%
    group_by(
      YEAR_MONTH = as.character(YEAR_MONTH),
      BREAKDOWN = breakdown_name,
      across(all_of(unname(breakdown_cols))),
      CH_FLAG,
      NHS_NO
    ) %>%
    rename(!!!breakdown_cols)
  
  # Sum the items and cost per patient month
  tmp_db <- tmp_db %>%
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT),
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      UNIQUE_MEDICINES = n_distinct(
        ifelse(
          test = substr(BNF_CHEMICAL_SUBSTANCE, 1, 2) %in%
            c(01, 02, 03, 04, 06, 07, 08, 09, 10),
          yes = CHEMICAL_SUBSTANCE_BNF_DESCR,
          no = NA_character_
        )
      )
    )
  
  # Calculate the metrics per patient month
  tmp_db <- tmp_db %>%
    ungroup(YEAR_MONTH, NHS_NO) %>%
    summarise(
      # Items and cost
      TOTAL_PATIENTS = n_distinct(NHS_NO), # For SDC
      ITEMS_PER_PATIENT_MONTH = mean(TOTAL_ITEMS),
      COST_PER_PATIENT_MONTH = mean(TOTAL_COST),
      # Unique medicines
      TOTAL_PATIENTS_UNIQUE_MEDICINES = n_distinct(
        ifelse(
          test = UNIQUE_MEDICINES > 1,
          yes = NHS_NO,
          NA
        )
      ),
      UNIQUE_MEDICINES_PER_PATIENT_MONTH = mean(UNIQUE_MEDICINES),
      TOTAL_PATIENTS_TEN_OR_MORE = n_distinct(
        ifelse(
          test = UNIQUE_MEDICINES >= 10,
          yes = NHS_NO,
          NA
        )
      )
    ) %>%
    ungroup() %>%
    mutate(
      PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = ifelse(
        test = TOTAL_PATIENTS_UNIQUE_MEDICINES == 0,
        yes = NA_real_,
        no = TOTAL_PATIENTS_TEN_OR_MORE / TOTAL_PATIENTS_UNIQUE_MEDICINES * 100
      )
    )
  
  # Either create the table or append to it
  if (breakdown_name == "Overall") {
    # On the first iteration initialise the table
    
    metrics_by_breakdown_and_ch_flag_db <- tmp_db
  } else {
    # Union results to initialised table
    
    metrics_by_breakdown_and_ch_flag_db <- union_all(
      x = metrics_by_breakdown_and_ch_flag_db,
      y = tmp_db
    )
  }
}

# Collect
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_db %>%
  collect()

# Get all the possible combinations
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_df %>%
  tidyr::complete(
    # Only breakdowns that already exist
    tidyr::nesting(
      BREAKDOWN,
      SUB_BREAKDOWN_CODE,
      SUB_BREAKDOWN_NAME,
      GENDER,
      AGE_BAND,
      NURSING_HOME_FLAG,
      RESIDENTIAL_HOME_FLAG
    ),
    # Every CH flag
    CH_FLAG,
    fill = list(
      TOTAL_PATIENTS = 0L,
      ITEMS_PER_PATIENT_MONTH = NA_real_,
      COST_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_UNIQUE_MEDICINES = 0L,
      UNIQUE_MEDICINES_PER_PATIENT_MONTH = NA_real_,
      TOTAL_PATIENTS_TEN_OR_MORE = 0L,
      PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH = NA_real_
    )
  )

# Apply SDC to the metrics based on the total patients
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_df %>%
  mutate(
    SDC = ifelse(TOTAL_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS = ifelse(
      test = SDC == 1,
      yes = NA_integer_,
      no = round(TOTAL_PATIENTS, -1)
    ),
    SDC_ITEMS_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1,
        yes = NA_integer_,
        no = janitor::round_half_up(ITEMS_PER_PATIENT_MONTH, 1)
      ),
    SDC_COST_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1L,
        yes = NA_integer_,
        no = janitor::round_half_up(COST_PER_PATIENT_MONTH)
      ),
    SDC = ifelse(TOTAL_PATIENTS_UNIQUE_MEDICINES %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS_UNIQUE_MEDICINES = ifelse(
      test = SDC == 1,
      yes = NA_integer_,
      no = round(TOTAL_PATIENTS_UNIQUE_MEDICINES, -1)
    ),
    SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1,
        yes = NA_integer_,
        no = janitor::round_half_up(UNIQUE_MEDICINES_PER_PATIENT_MONTH, 1)
      ),
    SDC = ifelse(TOTAL_PATIENTS_TEN_OR_MORE %in% c(1, 2, 3, 4), 1, 0),
    SDC_TOTAL_PATIENTS_TEN_OR_MORE = ifelse(
      test = SDC == 1,
      yes = NA_integer_,
      no = round(TOTAL_PATIENTS_TEN_OR_MORE, -1)
    ),
    SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH =
      ifelse(
        test = SDC == 1,
        yes = NA_integer_,
        no = janitor::round_half_up(PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH, 1)
      )
  ) %>%
  select(-SDC)

# Format for highcharter
metrics_by_breakdown_and_ch_flag_df <- metrics_by_breakdown_and_ch_flag_df %>%
  format_data_raw(
    c(
      "CH_FLAG",
      "GENDER",
      "AGE_BAND",
      "NURSING_HOME_FLAG",
      "RESIDENTIAL_HOME_FLAG"
    )
  )

# Simulate 3 years of data
metrics_by_breakdown_and_ch_flag_df <- bind_rows(
    metrics_by_breakdown_and_ch_flag_df %>% mutate(YEAR = "2020-2021"),
    metrics_by_breakdown_and_ch_flag_df %>% mutate(YEAR = "2021-2022"),
    metrics_by_breakdown_and_ch_flag_df %>% mutate(YEAR = "2022-2023")
  )

# Add to data
usethis::use_data(metrics_by_breakdown_and_ch_flag_df, overwrite = TRUE)

# Disconnect from database
DBI::dbDisconnect(con)
