library(dplyr)
library(dbplyr)
#library(highcharter)
library(ggplot2)
#devtools::install_github('nhsbsa-data-analytics/nhsbsaR')
#source("data-raw/app/data_raw_helpers.R")

con <- nhsbsaR::con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("DALL_REF", "INT646_BASE_20200401_20230331")) |>
      filter(FY == "2022/23" &
             UPRN_FLAG==1)


get_metrics <- function(init_db,
                        first_grouping,
                        second_grouping,
                        num_parallel = 32) {
  # Collect data and calculate raw metrics
  init_db |> 
    group_by(across(all_of(first_grouping))) |>
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
      TOTAL_COST =  0.01 * sum(ITEM_PAY_DR_NIC, na.rm = TRUE),
      UNIQUE_MEDICINES = n_distinct(
        case_when(
          CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      GTE_SIX = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[CHAPTER_1_4_6_10_CAT == 1]) >= 6 ~ 1L,
        TRUE ~ 0L 
      ),
      GTE_TEN = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[CHAPTER_1_4_6_10_CAT == 1]) >= 10 ~ 1L,
        TRUE ~ 0L 
      ),
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
      FALLS = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[FALLS_CAT == 1]) >= 3 ~ 1L,
        TRUE ~ 0L
      ),
      UNIQUE_MEDICINES_FALLS = n_distinct(
        case_when(
          FALLS_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      )
    ) |>
    ungroup() |> 
    group_by(across(all_of(second_grouping))) |>
    summarise(
      # Total patient months - use for denominators
      TOTAL_PM            = n(),
      TOTAL_PATIENTS      = n_distinct(NHS_NO),
      TOTAL_PM_ACB        = sum(ANY_ACB, na.rm = TRUE),
      TOTAL_PM_DAMN       = sum(ANY_DAMN, na.rm = TRUE),
      # Items, cost and unique meds count
      ITEMS_PPM           = mean(TOTAL_ITEMS, na.rm = TRUE),
      COST_PPM            = mean(TOTAL_COST, na.rm = TRUE),
      UNIQ_MEDS_PPM       = mean(UNIQUE_MEDICINES, na.rm = TRUE),
      # Unique medicines numerators
      RISK_PM_GTE_SIX     = sum(GTE_SIX, na.rm = TRUE),
      RISK_PM_GTE_TEN     = sum(GTE_TEN, na.rm = TRUE),
      # ACB numerator
      RISK_PM_ACB         = sum(ACB, na.rm = TRUE),
      # DAMN numerator
      RISK_PM_DAMN        = sum(DAMN, na.rm = TRUE),
      # Falls unique medicines
      UNIQ_MEDS_FALLS_PPM = mean(UNIQUE_MEDICINES_FALLS, na.rm = TRUE),
      # Falls numerator
      RISK_PM_FALLS       = sum(FALLS, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      # Calculate % metrics - each denominator is restricted to patients on any
      # med that is also a condition to be included in numerator
      PCT_PM_GTE_SIX = 100 * case_when(
        TOTAL_PM == 0 ~ NA,
        TRUE ~ RISK_PM_GTE_SIX / TOTAL_PM
      ),
      PCT_PM_GTE_TEN = 100 * case_when(
        TOTAL_PM == 0 ~ NA,
        TRUE ~ RISK_PM_GTE_TEN / TOTAL_PM
      ),
      PCT_PM_ACB = 100 * case_when(
        TOTAL_PM_ACB == 0 ~ NA,
        TRUE ~ RISK_PM_ACB / TOTAL_PM_ACB
      ),
      PCT_PM_DAMN = 100 * case_when(
        TOTAL_PM_DAMN == 0 ~ NA,
        TRUE ~ RISK_PM_DAMN / TOTAL_PM_DAMN
      ),
      PCT_PM_FALLS = 100 * case_when(
        TOTAL_PM == 0 ~ NA,
        TRUE ~ RISK_PM_FALLS / TOTAL_PM
      )
    ) |>
    nhsbsaR::collect_with_parallelism(num_parallel)

}

# Running time <= 1 min
tictoc::tic()
df <- get_metrics(
  DB,
  first_grouping = c(
    "YEAR_MONTH",
    "NHS_NO",
    "UPRN",
    "MATCH_SLA_STD"
  ),
  second_grouping = c(
    "UPRN",
    "MATCH_SLA_STD"
  )
)
tictoc::toc()


CQC_DB <- tbl(con, in_schema("MAMCP", "INT646_CQC_20230602"))

cqc_df <- CQC_DB |>
          group_by(UPRN) |>
          summarise(BEDS = max(NUMBER_OF_BEDS, na.rm = T)) |>
          nhsbsaR::collect_with_parallelism(24)

df <- df |> left_join(cqc_df, by="UPRN")

# df |> group_by(is.na(BEDS)) |> summarise(n=n())# 11483 joined





 
df |> mutate(pat_bin = cut(TOTAL_PATIENTS, breaks = seq(1, max(TOTAL_PATIENTS), by=1), right = F)) |>
  group_by(pat_bin) |>
  summarise(N_CH = n_distinct(UPRN)) -> t

#Exclude CHs with <5 patients...?
df <- df |> filter(TOTAL_PATIENTS >= 5)


df |> ggplot(aes(BEDS)) + geom_histogram(binwidth = 1) +
  geom_vline(xintercept = median(df$BEDS, na.rm = T), lty = 2) # Median = 38 beds

# CH classification based on beds: S (1-24) | M (25-50) | L (50+) (Stavros)
df <- df |> mutate(
  CH_SIZE = case_when(between(BEDS, 1, 24) ~ "Small",
                      between(BEDS, 25, 50) ~ "Medium",
                      BEDS > 50 ~ "Large",
                      T ~ NA)
)

df |> group_by(CH_SIZE) |> summarise(n = n())

# N_CH and avg metrics by size
t <- df |> filter(!is.na(CH_SIZE)) |>
      group_by(CH_SIZE) |>
      summarise(
        N_CH = n(),
        MEDIAN_BEDS = median(BEDS, na.rm = T),
        ITEMS_PPM = mean(ITEMS_PPM),
        COST_PPM = mean(COST_PPM),
        UNIQ_MEDS_PPM = mean(UNIQ_MEDS_PPM),
        PCT_PM_GTE_SIX = mean(PCT_PM_GTE_SIX),
        PCT_PM_GTE_TEN = mean(PCT_PM_GTE_TEN),
        PCT_PM_ACB = mean(PCT_PM_ACB, na.rm = T),
        PCT_PM_DAMN = mean(PCT_PM_DAMN, na.rm = T),
        UNIQ_MEDS_FALLS_PPM = mean(UNIQ_MEDS_FALLS_PPM),
        PCT_PM_FALLS = mean(PCT_PM_FALLS)
        ) |>
  tidyr::pivot_longer(
    where(is.numeric),
    names_to = "variable"
  ) |>
  mutate(CH_SIZE = factor(CH_SIZE, levels = c("Small","Medium","Large")))


t |> ggplot(aes(CH_SIZE, value)) +
  geom_col() +
  facet_wrap(~variable, scales = "free_y")

# Larger CHs have higher cost (at same item vol, so more expensive items), 
# slightly lower acb/damn/falls metrics, same or slightly worse polypharmacy metrics

#https://www.typeerror.org/docs/scikit_learn/modules/clustering
# Choice of a clustering algorithm: OPTICS has best performance
# and can, by design, not assign outliers to clusters
library(dbscan)


df_cl <- df  |>
  filter(if_all(starts_with("PCT") | ends_with("PPM"), ~ !is.na(.)))

x <- df_cl |> select(ITEMS_PPM, COST_PPM, UNIQ_MEDS_PPM,
                     PCT_PM_GTE_SIX, PCT_PM_GTE_TEN, PCT_PM_ACB,
                     PCT_PM_DAMN, UNIQ_MEDS_FALLS_PPM, PCT_PM_FALLS) |>
  as.matrix()

res <- optics(x, minPts = 100)
plot(res)
res
res2 <- extractDBSCAN(res, eps_cl = 100)
res2
plot(res2)  ## black is noise
hullplot(x, res2)



df_cl <- mutate(df_cl, cluster = res2$cluster)

df <- left_join(
  df,
  select(df_cl, UPRN, cluster),
  by = "UPRN"
)


df |> 
  mutate(
    cluster = case_when(
      cluster == 1 ~ "1",
      cluster == 0 ~ "2",
      T ~ "NA"
    )
  ) |>
  filter(cluster != "NA") |>
  group_by(cluster) |>
  summarise(
    N_CH = n(),
    MEDIAN_BEDS = median(BEDS, na.rm = T),
    MEAN_PATIENTS = mean(TOTAL_PATIENTS),
    ITEMS_PPM = mean(ITEMS_PPM),
    COST_PPM = mean(COST_PPM),
    UNIQ_MEDS_PPM = mean(UNIQ_MEDS_PPM),
    PCT_PM_GTE_SIX = mean(PCT_PM_GTE_SIX),
    PCT_PM_GTE_TEN = mean(PCT_PM_GTE_TEN),
    PCT_PM_ACB = mean(PCT_PM_ACB, na.rm = T),
    PCT_PM_DAMN = mean(PCT_PM_DAMN, na.rm = T),
    UNIQ_MEDS_FALLS_PPM = mean(UNIQ_MEDS_FALLS_PPM),
    PCT_PM_FALLS = mean(PCT_PM_FALLS)
  ) |>
  tidyr::pivot_longer(
    where(is.numeric),
    names_to = "variable"
  ) |>
  ggplot(aes(cluster, value)) +
  geom_col() +
  facet_wrap(~variable, scales = "free_y")

t <- df |> filter(cluster==0)

# The 15 outlier CHs had few patients, but more items, cost, more expensive items,
# higher polypharm and acb/damn/falls metrics.

# When looking at their names, some include
# - neurological care centre
# - ward for neuro-disability
# - complex care home
# which explains why they were outliers and validates the method
# in other CHs on the list, complex care could not be inferred from the name,
# so these may warrant an investigation

## Possibly do this looking at items/cost by BNF in each chapter/section/para as a dimension



DBI::dbDisconnect(con)
