library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)
#devtools::install_github('nhsbsa-data-analytics/nhsbsaR')

con <- nhsbsaR::con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("DALL_REF", "INT646_BASE_20200401_20230331")) |>
  filter(FY == "2022/23" & CH_FLAG==1)


df <- DB |> 
  group_by(NHS_NO, YEAR_MONTH) |>
  summarise(
    
    TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    
    UNIQUE_MEDICINES = n_distinct(
      case_when(
        CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
        TRUE ~ NA
      )
    )
    
  ) |>
  ungroup() |> 
  group_by(NHS_NO) |>
  summarise(
    across(
      !YEAR_MONTH,
      mean
      )
  ) |>
  nhsbsaR::collect_with_parallelism(32)


# Break total items into all chapters
# Break distinct medicines into 1-4 + 6-10 chapters











DBI::dbDisconnect(con)