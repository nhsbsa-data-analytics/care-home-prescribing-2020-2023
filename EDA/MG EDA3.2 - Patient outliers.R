library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)
#devtools::install_github('nhsbsa-data-analytics/nhsbsaR')

con <- nhsbsaR::con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("DALL_REF", "INT646_BASE_20200401_20230331")) |>
  filter(FY == "2022/23" & CH_FLAG==1)

tictoc::tic()
df1 <- DB |> 
  group_by(NHS_NO, YEAR_MONTH) |>
  summarise(
    
    TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    
    UNIQUE_MEDICINES = n_distinct(
      case_when(
        CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
      )
    ),
    
    COST = sum(ITEM_PAY_DR_NIC)/100
    
  ) |>
  ungroup() |> 
  group_by(NHS_NO) |>
  summarise(
    TOTAL_ITEMS = sum(TOTAL_ITEMS),
    TOTAL_MONTHS = n_distinct(YEAR_MONTH),
    MEAN_MONTHLY_ITEMS = sum(TOTAL_ITEMS) / n_distinct(YEAR_MONTH),
    MEAN_MONTHLY_UNIQUE_MEDS = sum(UNIQUE_MEDICINES) / n_distinct(YEAR_MONTH),
    MEAN_MONTHLY_COST = sum(COST) / n_distinct(YEAR_MONTH)
  ) |>
  nhsbsaR::collect_with_parallelism(32)
tictoc::toc()
# Runnning time ~10 sec




# Break total items into all chapters
# Break distinct medicines into 1-4 + 6-10 chapters

# Check: 4408676667, 147 total items, 9 months, mean items = 147/9 = 16.3 pm;
#                    29 distinct meds, mean unique meds = mean of monthly numbers, which may count overlapping meds = 13.7

#df |> filter(NHS_NO==4408676667)



tictoc::tic()
df2 <- DB |> 
  #tbl(con, "TEMP") |> # Testing
  group_by(NHS_NO, AGE, YEAR_MONTH, SECTION_DESCR) |>
  summarise(
    
    TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    
    UNIQUE_MEDICINES = n_distinct(
      case_when(
        CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
      )
    ),
    
    COST = sum(ITEM_PAY_DR_NIC)/100
    
  ) |>
  ungroup() |>
  
  left_join(
    DB |> group_by(NHS_NO) |> summarise(TOTAL_MONTHS = n_distinct(YEAR_MONTH)),
    by = "NHS_NO"
  ) |>
  
  # mutate(
  #   MEAN_MONTHLY_UNIQUE_MEDS = sum(UNIQUE_MEDICINES) / TOTAL_MONTHS
  # ) |>
  
  group_by(NHS_NO, AGE, TOTAL_MONTHS, SECTION_DESCR) |>
  summarise(
    TOTAL_ITEMS = sum(TOTAL_ITEMS),
    SUM_MONTHLY_UNIQUE_MEDS = sum(UNIQUE_MEDICINES),
    TOTAL_COST = sum(COST)
  ) |>
  ungroup() |>

  mutate(
    MEAN_MONTHLY_ITEMS = TOTAL_ITEMS / TOTAL_MONTHS,
    MEAN_MONTHLY_UNIQUE_MEDS = SUM_MONTHLY_UNIQUE_MEDS / TOTAL_MONTHS,
    MEAN_MONTHLY_COST = TOTAL_COST / TOTAL_MONTHS
  ) |>
  nhsbsaR::collect_with_parallelism(24)
tictoc::toc()
# Running time ~1.5 min

DBI::dbDisconnect(con); rm(con, DB)

df1 |> ggplot(aes(MEAN_MONTHLY_ITEMS)) + geom_histogram(binwidth = 5)
df1 |> filter(MEAN_MONTHLY_ITEMS >= 50) |> ggplot(aes(MEAN_MONTHLY_ITEMS)) + geom_histogram(binwidth = 5)
df1 |> filter(MEAN_MONTHLY_ITEMS >= 100) |> ggplot(aes(MEAN_MONTHLY_ITEMS)) + geom_histogram(binwidth = 5)
df1 |> filter(MEAN_MONTHLY_ITEMS >= 100) |> summarise(PATIENTS = n_distinct(NHS_NO))


highchart() |>
  hc_add_series(
    df1 |> filter(MEAN_MONTHLY_ITEMS >= 100) |> arrange(-MEAN_MONTHLY_ITEMS),
    "column",
    hcaes(NHS_NO, MEAN_MONTHLY_ITEMS),
  )




df2 <- df2 |> group_by(NHS_NO) |>
  mutate(ALL_MEAN_MONTHLY_ITEMS = sum(MEAN_MONTHLY_ITEMS),
         MEAN_MONTHLY_COST_ALL_ITEMS = sum(MEAN_MONTHLY_COST)) |> # Window-rollup across all sections
  ungroup() |>
  arrange(-ALL_MEAN_MONTHLY_ITEMS)


highchart() |>
  hc_add_series(
    df2 |> filter(ALL_MEAN_MONTHLY_ITEMS >= 100),
    "column",
    hcaes(NHS_NO, MEAN_MONTHLY_ITEMS, group = SECTION_DESCR),
    stacking = "normal"
  ) |>
  hc_xAxis(
    type = "category"
    ) |>
  hc_yAxis(
    min = 0, max = 180
  ) |>
  hc_tooltip(
    headerFormat = "",
    pointFormat = "Age: {point.AGE}<br>{series.name}: {point.y}"
  ) |>
  hc_legend(enabled = F) 


# Mean monthly cost per patient distribution
df1 |>
  ggplot(aes(MEAN_MONTHLY_COST)) + geom_histogram(binwidth = 5)

highchart() |>
  hc_add_series(
    df2 |> filter(MEAN_MONTHLY_COST_ALL_ITEMS >= 2772), # Hacky way to get top5
    "column",
    hcaes(NHS_NO, MEAN_MONTHLY_COST, group = SECTION_DESCR),
    stacking = "normal"
  ) |>
  hc_xAxis(
    type = "category"
  ) |>
  hc_tooltip(
    headerFormat = "",
    pointFormat = "Age: {point.AGE}<br>{series.name}: {point.y}"
  ) |>
  hc_legend(enabled = F) 

