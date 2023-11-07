library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)
#devtools::install_github('nhsbsa-data-analytics/nhsbsaR')
#source("data-raw/app/data_raw_helpers.R")

con <- nhsbsaR::con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("DALL_REF", "INT646_BASE_20200401_20230331")) |>
  filter(FY == "2022/23" & UPRN_FLAG==1)


get_metrics <- function(init_db,
                        first_grouping,
                        second_grouping,
                        num_parallel = 32) {
  # Collect data and calculate raw metrics
  init_db |> 
    group_by(across(all_of(first_grouping))) |>
    summarise(
      
      TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
      
      ANY_ACB = any(
        case_when(
          ACB_CAT > 0 ~ 1L,
          TRUE ~ 0L
        ),
        na.rm = TRUE
      ),
      ACB = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1]) >= 2 ~ 1L,
        TRUE ~ 0L
      ),
      ANY_DAMN = any(
        case_when(
          DAMN_CAT > 0 ~ 1L,
          TRUE ~ 0L
        ),
        na.rm = TRUE
      ),
      DAMN = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[DAMN_CAT == 1]) >= 2 ~ 1L,
        TRUE ~ 0L
      ),
      # More than 1 metric relating to one drug group will skew the overall picture
      # FALLS = case_when(
      #   n_distinct(BNF_CHEMICAL_SUBSTANCE[FALLS_CAT == 1]) >= 3 ~ 1L,
      #   TRUE ~ 0L
      # ),
      UNIQUE_MEDICINES_FALLS = n_distinct(
        case_when(
          FALLS_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      
      # Pure paracetamol not in combination with any other substances
      PARACETAMOL_ITEMS = sum(
        case_when(
          BNF_CHEMICAL_SUBSTANCE == "0407010H0" ~ 1,
          TRUE ~ 0
        )
      ),
      
      # Non-opioid analgesics excluding pure paracetamol
      NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS = sum(
        case_when(
          PARAGRAPH_DESCR == "Non-opioid analgesics and compound preparations" &
          BNF_CHEMICAL_SUBSTANCE != "0407010H0" ~ 1,
          TRUE ~ 0
        )
      ),
      
      # Opioid analgesics
      OP_ANALG_ITEMS = sum(
        case_when(
          PARAGRAPH_DESCR == "Opioid analgesics" ~ 1,
          TRUE ~ 0
        )
      ),
      
      # Tube feed, excludes many other types of oral nutrition
      ENTERAL_NUTRITION_ITEMS = sum(
        case_when(
          SECTION_DESCR == "Enteral nutrition" ~ 1,
          TRUE ~ 0
        )
      ),
      
      # Laxatives
     LAXATIVE_ITEMS = sum(
        case_when(
          SECTION_DESCR == "Laxatives" ~ 1,
          TRUE ~ 0
        )
      ),
      
      # Antibacterials, excluding UTI drugs
      ANTIBAC_EXCL_UTI_ITEMS = sum(
        case_when(
          SECTION_DESCR == "Antibacterial drugs" &
          PARAGRAPH_DESCR != "Urinary-tract infections" ~ 1,
          TRUE ~ 0
        )
      ),
      
      # Antibacterials for UTIs
      ANTIBAC_UTI_ITEMS = sum(
        case_when(
          SECTION_DESCR == "Antibacterial drugs" &
            PARAGRAPH_DESCR == "Urinary-tract infections" ~ 1,
          TRUE ~ 0
        )
      )
      
    ) |>
    ungroup() |> 
    group_by(across(all_of(second_grouping))) |>
    summarise(
      # Total patient months - use for denominators
      TOTAL_PM            = n(),
      TOTAL_PATIENTS      = n_distinct(NHS_NO),
      TOTAL_ITEMS         = sum(TOTAL_ITEMS, na.rm = TRUE),
      TOTAL_PM_ACB        = sum(ANY_ACB, na.rm = TRUE),
      TOTAL_PM_DAMN       = sum(ANY_DAMN, na.rm = TRUE),
      # ACB numerator
      RISK_PM_ACB         = sum(ACB, na.rm = TRUE),
      # DAMN numerator
      RISK_PM_DAMN        = sum(DAMN, na.rm = TRUE),
      # Falls unique medicines
      UNIQ_MEDS_FALLS_PPM = mean(UNIQUE_MEDICINES_FALLS, na.rm = TRUE),
      # Falls numerator
      # RISK_PM_FALLS       = sum(FALLS, na.rm = TRUE)
      
      # Various items PPM
      PARACETAMOL_ITEMS_PPM  = mean(PARACETAMOL_ITEMS),
      NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS_PPM = mean(NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS),
      OP_ANALG_ITEMS_PPM = mean(OP_ANALG_ITEMS),
      ENTERAL_NUTRITION_ITEMS_PPM  = mean(ENTERAL_NUTRITION_ITEMS),
      LAXATIVE_ITEMS_PPM = mean(LAXATIVE_ITEMS),
      ANTIBAC_EXCL_UTI_ITEMS_PPM = mean(ANTIBAC_EXCL_UTI_ITEMS),
      ANTIBAC_UTI_ITEMS_PPM = mean(ANTIBAC_UTI_ITEMS)
      
    ) |>
    ungroup() |>
    mutate(
      # Calculate % metrics - each denominator is restricted to patients on any
      # med that is also a condition to be included in numerator
      PCT_PM_ACB = 100 * case_when(
        TOTAL_PM_ACB == 0 ~ NA,
        TRUE ~ RISK_PM_ACB / TOTAL_PM_ACB
      ),
      PCT_PM_DAMN = 100 * case_when(
        TOTAL_PM_DAMN == 0 ~ NA,
        TRUE ~ RISK_PM_DAMN / TOTAL_PM_DAMN
      )#,
      # PCT_PM_FALLS = 100 * case_when(
      #   TOTAL_PM == 0 ~ NA,
      #   TRUE ~ RISK_PM_FALLS / TOTAL_PM
      # )
    ) |>
    nhsbsaR::collect_with_parallelism(num_parallel)
  
}

# Here we have 3 types of metric: 
# - mean distinct chem subs per PM
# - % of PMs exceeding a certain distinct chem sub threshold
# - mean items per PM (to be developed)
# These may need to be harmonised where possible, e.g. use mean falls-risk items instead of distinct falls-risk substances


# Running time <= 1 min
tictoc::tic()
df <- get_metrics(
  DB,
  first_grouping = c(
    "YEAR_MONTH",
    "NHS_NO",
    "UPRN",
    "MATCH_SLA_STD",
    "MATCH_TYPE"
  ),
  second_grouping = c(
    "UPRN",
    "MATCH_SLA_STD",
    "MATCH_TYPE"
  )
)
tictoc::toc()

cqc_df <- tbl(con, in_schema("MAMCP", "INT646_CQC_20230602")) |>
  group_by(UPRN) |>
  summarise(BEDS = max(NUMBER_OF_BEDS, na.rm = T)) |>
  nhsbsaR::collect_with_parallelism(24)

df <- df |> left_join(cqc_df, by="UPRN"); rm(cqc_df)


df |> filter(TOTAL_PATIENTS <= 100) |>
  ggplot(aes(TOTAL_PATIENTS)) + geom_histogram(binwidth = 5)

df |> group_by(TOTAL_PATIENTS) |>
  summarise(Carehomes = n()) |>
  arrange(TOTAL_PATIENTS) -> t

# ST LAURAS CARE HOME WD4 8BH has only 5 patients, but >60 beds (on its website)
# Check what is going out in this and other carhomes with small pat counts

#Exclude CHs with <5 patients...?
df <- df |> filter(TOTAL_PATIENTS >= 10)


norm_minmax <- function(x, ...) {
  return((x - min(x, ...)) /(max(x, ...) - min(x, ...)))
}


# Normalise metrics
df <- df |> mutate(across(
          UNIQ_MEDS_FALLS_PPM:ncol(df),
          ~ norm_minmax(.x, na.rm = T),
          .names = "{.col}_NORM"
        ))

# Remove non-UTF-8 characters in 3 addresses (interferes with highcharts)
df <- df |> mutate(MATCH_SLA_STD = iconv(MATCH_SLA_STD, to="UTF-8", sub=""))



# Overall outliers
overall_out <- df |>
  tidyr::pivot_longer(
    cols = ends_with("_NORM"),
    names_to = "metric"
  ) |>
  group_by(UPRN, MATCH_SLA_STD, TOTAL_PATIENTS, TOTAL_PM,
           UNIQ_MEDS_FALLS_PPM,
           PCT_PM_ACB,
           PCT_PM_DAMN,
           PARACETAMOL_ITEMS_PPM,
           NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS_PPM,
           OP_ANALG_ITEMS_PPM,
           ENTERAL_NUTRITION_ITEMS_PPM,
           LAXATIVE_ITEMS_PPM,
           ANTIBAC_EXCL_UTI_ITEMS_PPM,
           ANTIBAC_UTI_ITEMS_PPM) |>
  summarise(mean_normalised_value = mean(value, na.rm = T), .groups = "drop")

# Add normalised metrics
overall_out <- left_join(
  overall_out,
  select(df, UPRN, ends_with("_NORM")),
  by = "UPRN"
)


# myboxplotData <- data_to_boxplot(overall_out, mean_normalised_value, add_outliers  = T)
# 
# highchart() |>
#   hc_xAxis(type ="category") |>
#   hc_add_series_list(myboxplotData) |>
#   hc_chart(inverted = T)

highchart() |>
  hc_add_series(
    overall_out |> mutate(y = 0),
    "scatter",
    hcaes(mean_normalised_value, y)
  ) |>
  hc_tooltip(
    headerFormat = "",
    pointFormat = paste0(
      "{point.MATCH_SLA_STD}<br>",
      "<b>Mean normalised value:</b> {point.mean_normalised_value:.2f}<br>",
      "<b>Patients:</b> {point.TOTAL_PATIENTS}<br>",
      "<b>Patient-months:</b> {point.TOTAL_PM}<br>",
      "<b>Norm unique falls meds PPM:</b> {point.UNIQ_MEDS_FALLS_PPM_NORM:.2f}<br>",
      "<b>Norm prop PM 2+ ACB meds:</b> {point.PCT_PM_ACB_NORM:.2f}<br>",
      "<b>Norm prop PM 2+ DAMN meds:</b> {point.PCT_PM_DAMN_NORM:.2f}<br>",
      "<b>Norm paracetamol items PPM:</b> {point.PARACETAMOL_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm non-opioid analgesic (excl. paracetamol) items PPM:</b> {point.NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm opioid analgesic items PPM:</b> {point.OP_ANALG_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm enteral nutrition items PPM:</b> {point.ENTERAL_NUTRITION_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm laxative items PPM:</b> {point.LAXATIVE_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm antibacterial items (excl. UTI) PPM:</b> {point.ANTIBAC_EXCL_UTI_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm antibacterial UTI items PPM:</b> {point.ANTIBAC_UTI_ITEMS_PPM_NORM:.2f}"
      )
    ) |>
  hc_chart(zoomType = "x")


# Overall outliers + percentile in each dimension
top <- overall_out |>
  select(MATCH_SLA_STD, TOTAL_PATIENTS, TOTAL_PM, mean_normalised_value,
         ends_with("_NORM")) |>
  arrange(-mean_normalised_value) |>
  mutate(across(
    !c(1:3),
    ~ round(.x, 3)
  )) |> tidyr::pivot_longer(
  cols = ends_with("_NORM"),
  names_to = "metric") |>
  head(10 * 10) |> # 10 CHs x 10 metrics
  mutate(
    metric = case_when(
      metric == "UNIQ_MEDS_FALLS_PPM_NORM" ~ "Unique falls meds",
      metric == "PCT_PM_ACB_NORM" ~ "Prop ACB 2+",
      metric == "PCT_PM_DAMN_NORM" ~ "Prop DAMN 2+",
      metric == "PARACETAMOL_ITEMS_PPM_NORM" ~ "Paracetamol",
      metric == "NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS_PPM_NORM" ~ "Non-opioid analgesics (ex. paracet)",
      metric == "OP_ANALG_ITEMS_PPM_NORM" ~ "Opioid analgesics",
      metric == "ENTERAL_NUTRITION_ITEMS_PPM_NORM" ~ "Enteral nutrition",
      metric == "LAXATIVE_ITEMS_PPM_NORM" ~ "Laxatives",
      metric == "ANTIBAC_EXCL_UTI_ITEMS_PPM_NORM" ~ "Antibacterials (ex. UTI)",
      metric == "ANTIBAC_UTI_ITEMS_PPM_NORM" ~ "Antibacterials (UTI)",
      T ~ NA
    )
  )

ggplot(top, aes(metric, value)) + geom_col() +
  facet_wrap(~MATCH_SLA_STD, nrow = 2) +
  coord_flip()
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))


### 2d/3d outliers

# Painkillers

highchart() |>
  hc_add_series(
    overall_out,
    "scatter",
    hcaes(NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS_PPM_NORM, OP_ANALG_ITEMS_PPM_NORM, color = PARACETAMOL_ITEMS_PPM_NORM)
  ) |>
  hc_tooltip(
    headerFormat = "",
    pointFormat = paste0(
      "{point.MATCH_SLA_STD}<br>",
      "<b>Mean normalised value:</b> {point.mean_normalised_value:.2f}<br>",
      "<b>Patients:</b> {point.TOTAL_PATIENTS}<br>",
      "<b>Patient-months:</b> {point.TOTAL_PM}<br>",
      "<b>Norm paracetamol items PPM:</b> {point.PARACETAMOL_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm non-opioid analgesic (excl. paracetamol) items PPM:</b> {point.NON_OP_ANALG_EXCL_PARACETAMOL_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm opioid analgesic items PPM:</b> {point.OP_ANALG_ITEMS_PPM_NORM:.2f}"
    )
  ) |>
  hc_xAxis(
    title = list(text = "Norm non-opioid analgesic (excl. paracetamol) items PPM"),
    min = 0, max = 1
    ) |>
  hc_yAxis(
    title = list(text = "Norm opioid analgesic items PPM"),
    min = 0, max = 1
    )


# Nutrition

highchart() |>
  hc_add_series(
    overall_out,
    "scatter",
    hcaes(ENTERAL_NUTRITION_ITEMS_PPM_NORM, LAXATIVE_ITEMS_PPM_NORM)
  ) |>
  hc_tooltip(
    headerFormat = "",
    pointFormat = paste0(
      "{point.MATCH_SLA_STD}<br>",
      "<b>Mean normalised value:</b> {point.mean_normalised_value:.2f}<br>",
      "<b>Patients:</b> {point.TOTAL_PATIENTS}<br>",
      "<b>Patient-months:</b> {point.TOTAL_PM}<br>",
      "<b>Norm enteral nutrition items PPM:</b> {point.ENTERAL_NUTRITION_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm laxative items PPM:</b> {point.LAXATIVE_ITEMS_PPM_NORM:.2f}<br>"
    )
  ) |>
  hc_xAxis(
    title = list(text = "Norm enteral nutrition items PPM"),
    min = 0, max = 1
  ) |>
  hc_yAxis(
    title = list(text = "Norm laxative items PPM"),
    min = 0, max = 1
  )

# Opioids & constipation

# Note: Dowsvale Nursing Home, Dorking - top2 outlier on mean norm value

highchart() |>
  hc_add_series(
    overall_out,
    "scatter",
    hcaes(OP_ANALG_ITEMS_PPM_NORM, LAXATIVE_ITEMS_PPM_NORM),
    regression = TRUE
  ) |>
  hc_add_dependency("plugins/highcharts-regression.js") |>
  hc_tooltip(
    headerFormat = "",
    pointFormat = paste0(
      "{point.MATCH_SLA_STD}<br>",
      "<b>Mean normalised value:</b> {point.mean_normalised_value:.2f}<br>",
      "<b>Patients:</b> {point.TOTAL_PATIENTS}<br>",
      "<b>Patient-months:</b> {point.TOTAL_PM}<br>",
      "<b>Norm opioid analgesic items PPM:</b> {point.OP_ANALG_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm laxative items PPM:</b> {point.LAXATIVE_ITEMS_PPM_NORM:.2f}<br>"
    )
  ) |>
  hc_xAxis(
    title = list(text = "Norm opioid analgesic items PPM"),
    min = 0, max = 1
  ) |>
  hc_yAxis(
    title = list(text = "Norm laxative items PPM"),
    min = 0, max = 1
  )

# Antibacterials

highchart() |>
  hc_add_series(
    overall_out,
    "scatter",
    hcaes(ANTIBAC_EXCL_UTI_ITEMS_PPM_NORM, ANTIBAC_UTI_ITEMS_PPM_NORM)
  ) |>
  hc_tooltip(
    headerFormat = "",
    pointFormat = paste0(
      "{point.MATCH_SLA_STD}<br>",
      "<b>Mean normalised value:</b> {point.mean_normalised_value:.2f}<br>",
      "<b>Patients:</b> {point.TOTAL_PATIENTS}<br>",
      "<b>Patient-months:</b> {point.TOTAL_PM}<br>",
      "<b>Norm antibacterial items (excl. UTI) PPM:</b> {point.ANTIBAC_EXCL_UTI_ITEMS_PPM_NORM:.2f}<br>",
      "<b>Norm antibacterial UTI items PPM:</b> {point.ANTIBAC_UTI_ITEMS_PPM_NORM:.2f}"
    )
  ) |>
  hc_xAxis(
    title = list(text = "Norm antibacterial items (excl. UTI) PPM"),
    min = 0, max = 1
  ) |>
  hc_yAxis(
    title = list(text = "Norm antibacterial UTI items PPM"),
    min = 0, max = 1
  )


DBI::dbDisconnect(con)
