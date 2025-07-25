
# Library
library(dplyr)
library(dbplyr)

# Metadata
base_table <- "INT646_BASE_20200401_20250331"
start_year <- substring(base_table, 13, 16)
end_year <- substring(base_table, 22, 25)
EXPECTED_YEARS <- as.integer(end_year) - as.integer(start_year)
EXPECTED_MONTHS <- 12 * EXPECTED_YEARS

# Connect to dalp
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Create a lazy table from the item level base table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", base_table))


# Row validation calculation ---------------------------------------------------

# Distinct count of geography categories
distinct_counts <- fact_db %>% 
  summarise(
    across(
      starts_with("PCD") & ends_with("CODE"),
      n_distinct
    )
  ) %>%
  collect() %>% 
  rename_with(\(x) glue::glue("EXPECTED_{x}S")) %>% 
  as.list() %>% 
  purrr::iwalk(
    \(x, idx) assign(idx, x, envir = .GlobalEnv)
  )

# Expected rows for final table
EXPECTED_ROWS <- EXPECTED_YEARS *
  # All geography combinations
  (EXPECTED_PCD_REGION_CODES + EXPECTED_PCD_ICB_CODES + EXPECTED_PCD_LAD_CODES) * 
  # Top 50 per BNF part, but only 21 Chapters
  (50 + 50 + 50 + 21 ) *
  # 6 metrics
  6

# BNF columns
bnf_cols = c(
  "CHAPTER_DESCR",
  "SECTION_DESCR", 
  "PARAGRAPH_DESCR", 
  "CHEMICAL_SUBSTANCE_BNF_DESCR"
)

# Geography columns
geo_cols = c(
  "PCD_REGION_NAME", 
  "PCD_LAD_NAME", 
  "PCD_ICB_NAME"
)

# All level permutations
all_levels = cross_join(
  data.frame(BNF = bnf_cols),
  data.frame(GEO = geo_cols)
)

# BNF cols vector
bnf_cols = all_levels %>% 
  select(BNF) %>% 
  pull()

# Part One: Get distinct patient counts ----------------------------------------

# Function to generate data
get_pats = function(index){
  
  # Get vars names
  bnf = rlang::sym(all_levels[index,1])
  geo = rlang::sym(all_levels[index,2])
  
  # Initial care home filter
  ch_db = fact_db %>% 
    filter(CH_FLAG == 1)
  
  # Filter bnf element when paragraph == section  
  if(bnf_cols[index] == "PARAGRAPH_DESCR"){
    ch_db = ch_db %>% filter(PARAGRAPH_DESCR != SECTION_DESCR)
  }
  
  # Filter bnf element when chemical substance == paragraph  
  if(bnf_cols[index] == "CHEMICAL_SUBSTANCE_BNF_DESCR"){
    ch_db = ch_db %>% filter(CHEMICAL_SUBSTANCE_BNF_DESCR != PARAGRAPH_DESCR)
  }
  
  # Limit to top 50 bnf_child across all years & geographies, for *each metric*
  join_fact = ch_db %>% 
    group_by({{ bnf }}) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 50
    ) %>% 
    ungroup() %>% 
    select({{ bnf }})
  
  # Number of distinct patients per aggregation combination
  fact_db %>% 
    inner_join(join_fact) %>% 
    filter(
      CH_FLAG == 1,
      !is.na({{ geo }})
    ) %>% 
    group_by(FY, {{ geo }}, {{ bnf }}) %>%
    summarise(PATS = n_distinct(NHS_NO)) %>%
    ungroup() %>%
    nhsbsaR::collect_with_parallelism(., 16) %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = rlang::as_string(geo),
      GEOGRAPHY_CHILD := {{ geo }},
      BNF_PARENT = rlang::as_string(bnf),
      BNF_CHILD := {{ bnf }},
      PATS = ifelse(PATS <= 5, 5, PATS)
    )
}

# Get prop results: < 3 mins
pat_results = lapply(1:nrow(all_levels), get_pats) %>% bind_rows()

# Part Two: items and nic prop -------------------------------------------------

# Function to generate data
get_geo_bnf_prop = function(index){
  
  # Get vars names
  bnf = rlang::sym(all_levels[index,1])
  geo = rlang::sym(all_levels[index,2])
  
  # Initial care home filter
  ch_db = fact_db %>% 
    filter(CH_FLAG == 1)
  
  # Filter when section name equals paragraph name
  if(bnf_cols[index] == "PARAGRAPH_DESCR"){
    ch_db = ch_db %>% filter(PARAGRAPH_DESCR != SECTION_DESCR)
  }

  # Filter when chem_sub name equals paragraph name
  if(bnf_cols[index] == "CHEMICAL_SUBSTANCE_BNF_DESCR"){
    ch_db = ch_db %>% filter(CHEMICAL_SUBSTANCE_BNF_DESCR != PARAGRAPH_DESCR)
  }
  
  # Limit to top 50 bnf_child across all years & geographies, for *each metric*
  join_fact = ch_db %>% 
    group_by({{ bnf }}) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    ungroup() %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 50
    ) %>% 
    select({{ bnf }}) %>% 
    nhsbsaR::collect_with_parallelism(., 16)
  
  # Total metric generation plus join
  df = fact_db %>% 
    filter(
      CH_FLAG == 1,
      !is.na({{ geo }})
      ) %>% 
    group_by(FY, {{ geo }}, {{ bnf }}) %>%
    summarise(
      ITEMS = sum(ITEM_COUNT),
      NIC = sum(ITEM_PAY_DR_NIC) / 100
      ) %>%
    mutate(
      TOTAL_ITEMS = sum(ITEMS),
      TOTAL_NIC = sum(NIC),
      PROP_ITEMS = (ITEMS / TOTAL_ITEMS) * 100,
      PROP_NIC = (NIC / TOTAL_NIC) * 100
      ) %>%
    ungroup() %>%
    nhsbsaR::collect_with_parallelism(., 16) %>% 
    inner_join(join_fact) %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = rlang::as_string(geo),
      GEOGRAPHY_CHILD := {{ geo }},
      BNF_PARENT = rlang::as_string(bnf),
      BNF_CHILD := {{ bnf }},
      PROP_ITEMS = janitor::round_half_up(PROP_ITEMS, 3),
      PROP_NIC = janitor::round_half_up(PROP_NIC, 3),
      TOTAL_ITEMS = ITEMS,
      TOTAL_NIC = NIC
      ) %>% 
    tidyr::pivot_longer(
      c('PROP_ITEMS', 'PROP_NIC', 'TOTAL_ITEMS', 'TOTAL_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
      )
    
  # Expand grid an left_join for all permutations
  expand.grid(
    FY = unique(df$FY),
    GEOGRAPHY_PARENT = unique(df$GEOGRAPHY_PARENT),
    GEOGRAPHY_CHILD = unique(df$GEOGRAPHY_CHILD),
    BNF_PARENT = unique(df$BNF_PARENT),
    BNF_CHILD = unique(df$BNF_CHILD),
    METRIC = unique(df$METRIC)
    ) %>% 
    left_join(df) %>% 
    mutate(VALUE = ifelse(is.na(VALUE), 0, VALUE))
}

# Get prop results: < 5 mins
prop_results = lapply(1:nrow(all_levels), get_geo_bnf_prop) %>% bind_rows()

# Part Three: items ppm and nic ppm --------------------------------------------

# Function to generate data
get_geo_bnf_ppm = function(index){
  
  # Get vars names
  bnf = rlang::sym(all_levels[index,1])
  geo = rlang::sym(all_levels[index,2])
  
  # Initial care home filter
  ch_db = fact_db %>% 
    filter(CH_FLAG == 1)
  
  # Filter bnf element when paragraph == section  
  if(bnf_cols[index] == "PARAGRAPH_DESCR"){
    ch_db = ch_db %>% filter(PARAGRAPH_DESCR != SECTION_DESCR)
  }
  
  # Filter bnf element when chemical substance == paragraph  
  if(bnf_cols[index] == "CHEMICAL_SUBSTANCE_BNF_DESCR"){
    ch_db = ch_db %>% filter(CHEMICAL_SUBSTANCE_BNF_DESCR != PARAGRAPH_DESCR)
  }
  
  # Limit to top 50 bnf_child across all years & geographies, for *each metric*
  join_fact = ch_db %>% 
    filter(CH_FLAG == 1) %>% 
    group_by({{ bnf }}) %>%
    summarise(TOTAL_ITEMS = sum(ITEM_COUNT)) %>% 
    slice_max(
      TOTAL_ITEMS,
      n = 50
    ) %>% 
    ungroup() %>% 
    select({{ bnf }})
  
  # Distinct months of presc per nhs_no and FY
  pat_months = fact_db %>% 
    filter(
      CH_FLAG == 1,
      !is.na({{ geo }})
    ) %>% 
    group_by(FY, YEAR_MONTH, NHS_NO, {{ geo }}) %>% 
    summarise() %>% 
    ungroup() %>% 
    cross_join(join_fact)
  
  # Generate output
  pat_info = fact_db %>% 
    # There are 17 records without a postcode and no geographic information
    filter(
      CH_FLAG == 1,
      !is.na(IMD_DECILE)
      ) %>% 
    group_by(FY, YEAR_MONTH, NHS_NO, {{ geo }}, {{ bnf }}) %>% 
    summarise(
      ITEMS = sum(ITEM_COUNT),
      NIC = sum(ITEM_PAY_DR_NIC) / 100
    ) %>%
    ungroup()
  
  # Output
  pat_months %>% 
    left_join(pat_info) %>%
    # Data now annual patient-month level
    mutate(
      ITEMS = ifelse(is.na(ITEMS), 0, ITEMS),
      NIC = ifelse(is.na(NIC), 0, NIC)
    ) %>% 
    # Annual-level aggregation
    group_by(FY, {{ geo }}, {{ bnf }}) %>% 
    summarise(
      PPM_ITEMS = mean(ITEMS),
      PPM_NIC = mean(NIC)
    ) %>% 
    ungroup() %>% 
    nhsbsaR::collect_with_parallelism(., 16) %>% 
    transmute(
      FY,
      GEOGRAPHY_PARENT = rlang::as_string(geo),
      GEOGRAPHY_CHILD := {{ geo }},
      BNF_PARENT = rlang::as_string(bnf),
      BNF_CHILD := {{ bnf }},
      PPM_ITEMS = janitor::round_half_up(PPM_ITEMS, 3),
      PPM_NIC = janitor::round_half_up(PPM_NIC, 3)
    ) %>% 
    tidyr::pivot_longer(
      c('PPM_ITEMS', 'PPM_NIC'),
      names_to = "METRIC",
      values_to = "VALUE"
    )
}

# Get ppm results: ~30 mins
ppm_results = lapply(1:nrow(all_levels), get_geo_bnf_ppm) %>% bind_rows()

# Part Four: bind results and left-join pat count ------------------------------

# Bind both both outputs
output = rbind(prop_results, ppm_results) %>% 
  left_join(pat_results) %>% 
  mutate(
    GEOGRAPHY_PARENT = case_when(
      GEOGRAPHY_PARENT == "PCD_REGION_NAME" ~ "Region",
      GEOGRAPHY_PARENT == "PCD_ICB_NAME" ~ "ICS",
      GEOGRAPHY_PARENT == "PCD_LAD_NAME" ~ "Local Authority"
    ),
    BNF_PARENT = case_when(
      BNF_PARENT == "CHAPTER_DESCR" ~ "Chapter",
      BNF_PARENT == "PARAGRAPH_DESCR" ~ "Paragraph",
      BNF_PARENT == "SECTION_DESCR" ~ "Section",
      BNF_PARENT == "CHEMICAL_SUBSTANCE_BNF_DESCR" ~ "Chemical substance"
    ),
    METRIC = case_when(
      METRIC == "PPM_ITEMS" ~ "Mean prescription items PPM",
      METRIC == "PPM_NIC" ~ "Mean drug cost PPM",
      METRIC == "PROP_ITEMS" ~ "% of total annual number of prescription items",
      METRIC == "PROP_NIC" ~ "% of total annual drug cost",
      METRIC == "TOTAL_ITEMS" ~ "Total annual number of prescription items",
      METRIC == "TOTAL_NIC" ~ "Total annual drug cost"
    ),
    PATS = ifelse(is.na(PATS), 0, PATS)
  )

# Part Five: ensure all permutations present -----------------------------------

# Final year month value
end_year_month = substring(base_table, 22, 27)

# Get geo data
get_geo_data_mod8 = function(pcd_col, parent_col){
  
  con %>%
    tbl(from = in_schema("DALL_REF", base_table)) %>% 
    filter(YEAR_MONTH == end_year_month) %>% 
    select(starts_with(pcd_col)) %>% 
    distinct() %>% 
    collect() %>% 
    filter(!is.na(.[[pcd_col]])) %>% 
    rename("GEOGRAPHY_CHILD" = pcd_col) %>% 
    mutate(GEOGRAPHY_PARENT = parent_col)
}

# Get data for each geography in same format
distinct_geo <- do.call(rbind, list(
  get_geo_data_mod8("PCD_REGION_NAME", "Region"),
  get_geo_data_mod8("PCD_ICB_NAME", "ICS"),
  get_geo_data_mod8("PCD_LAD_NAME", "Local Authority")
  ))

# Distinct combinations of other fields
distinct_info = output %>% 
  select(FY, BNF_PARENT, BNF_CHILD, METRIC) %>% 
  distinct()

# Cross join and then left join
mod_geo_ch_flag_drug_df = cross_join(distinct_info, distinct_geo) %>% 
  left_join(output) %>% 
  mutate(
    VALUE = ifelse(is.na(VALUE), 0, VALUE),
    PATS = ifelse(is.na(PATS), 0, PATS),
    GEOGRAPHY_CHILD = case_when(
      GEOGRAPHY_PARENT == "PCD_ICB_NAME" ~ gsub("\\bNHS\\b ", "", GEOGRAPHY_CHILD),
      TRUE ~ GEOGRAPHY_CHILD
    ),
    GEOGRAPHY_CHILD = case_when(
      GEOGRAPHY_PARENT == "PCD_ICB_NAME" ~ gsub("\\bICB\\b", "", GEOGRAPHY_CHILD),
      TRUE ~ GEOGRAPHY_CHILD
    )
  ) %>% 
  select(names(output)) %>% 
  verify(nrow.alt(.) == EXPECTED_ROWS) %>% 
  assert.alt(not_na.alt, VALUE, PATS)


# Use this
usethis::use_data(mod_geo_ch_flag_drug_df, overwrite = TRUE)


# Part Five: pre-compute download data -----------------------------------------
# Computing this on the fly takes ~60s, so compute and save for instant download

# Additional functions
source("R/utils_helpers.R")

# Pivot data from tall to wide for mod data download
bnf_level_prescribing_estimates_in_care_homes_df <- mod_geo_ch_flag_drug_df %>%
  tidyr::pivot_wider(
    names_from = "METRIC",
    values_from = "VALUE"
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("Total"), bespoke_round
    )
  )

# Need to start a new chain to prevent dplyr trying to arrange the
# original longer vectors
bnf_level_prescribing_estimates_in_care_homes_df <- 
  bnf_level_prescribing_estimates_in_care_homes_df %>% 
  dplyr::filter(GEOGRAPHY_CHILD != "Isles of Scilly") %>% 
  dplyr::arrange(
    .data$FY,
    .data$GEOGRAPHY_PARENT,
    .data$GEOGRAPHY_CHILD,
    .data$BNF_PARENT,
    .data$BNF_CHILD
  ) %>%
  dplyr::rename(
    `Financial year` = "FY",
    Geography = "GEOGRAPHY_PARENT",
    `Sub-geography name` = "GEOGRAPHY_CHILD",
    `BNF level` = "BNF_PARENT",
    `BNF sub-level` = "BNF_CHILD",
    `Patient count` = "PATS"
  ) %>% 
  dplyr::mutate(`Patient count` = bespoke_round(`Patient count`))

# Use this
usethis::use_data(bnf_level_prescribing_estimates_in_care_homes_df, overwrite = TRUE)

# Disconnect
DBI::dbDisconnect(con); rm(list = ls()); gc()

#-------------------------------------------------------------------------------