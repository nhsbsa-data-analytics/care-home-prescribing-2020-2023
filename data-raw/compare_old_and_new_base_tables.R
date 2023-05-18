# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

table_name = "INT646_BASE_20210401_20220331"

old_db <- con %>%
  tbl(from = in_schema("ADNSH", table_name))

new_db <- con %>%
  tbl(from = table_name)

row_count_old <- old_db %>% tally() %>% pull() # 1109760069
row_count_new <- new_db %>% tally() %>% pull() # 1109760069
row_count_old == row_count_new # TRUE

# group by year_month
# count distinct nhs_no
# sum items
# sum nic
ym_summary_OLD <- old_db %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(
    DISTINCT_NHS_NO = n_distinct(NHS_NO),
    SUM_ITEMS = sum(ITEM_COUNT),
    SUM_NIC = sum(ITEM_PAY_DR_NIC)
  ) %>%
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/ym_summary_OLD.rds")

ym_summary_NEW <- new_db %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(
    DISTINCT_NHS_NO = n_distinct(NHS_NO),
    SUM_ITEMS = sum(ITEM_COUNT),
    SUM_NIC = sum(ITEM_PAY_DR_NIC)
  ) %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/ym_summary_NEW.rds")

ym_summary_OLD <- readRDS("data-raw/temp_comparison_data/ym_summary_OLD.rds")
cols <- names(ym_summary_OLD %>% select(-YEAR_MONTH))

ym_summary_OLD <- ym_summary_OLD %>%
  arrange(YEAR_MONTH) %>% 
  rename_with(~ paste0(.x, "_OLD"))
ym_summary_NEW <- readRDS("data-raw/temp_comparison_data/ym_summary_NEW.rds") %>% 
  arrange(YEAR_MONTH) %>% 
  rename_with(~ paste0(.x, "_NEW"))

ym_summary <- bind_cols(ym_summary_OLD, ym_summary_NEW)

for(col in cols) {
  ym_summary <- ym_summary %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

ym_summary <- ym_summary %>%
  select(order(colnames(ym_summary))) %>% 
  select(YEAR_MONTH = YEAR_MONTH_OLD, everything(), -YEAR_MONTH_NEW)

saveRDS(ym_summary, "data-raw/temp_comparison_data/ym_summary.rds")

# group by match_type
# count
match_type_summary_OLD <- old_db %>%
  group_by(MATCH_TYPE) %>%
  summarise(
    COUNT = n()
  ) %>%
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/match_type_summary_OLD.rds")

match_type_summary_NEW <- new_db %>%
  group_by(MATCH_TYPE) %>%
  summarise(
    COUNT = n()
  ) %>%
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/match_type_summary_NEW.rds")

match_type_summary_OLD <- readRDS("data-raw/temp_comparison_data/match_type_summary_OLD.rds")
cols <- names(match_type_summary_OLD %>% select(-MATCH_TYPE))

match_type_summary_OLD <- match_type_summary_OLD %>%
  arrange(MATCH_TYPE) %>% 
  rename_with(~ paste0(.x, "_OLD"))
match_type_summary_NEW <- readRDS("data-raw/temp_comparison_data/match_type_summary_NEW.rds") %>% 
  arrange(MATCH_TYPE) %>% 
  rename_with(~ paste0(.x, "_NEW"))

match_type_summary <- bind_cols(match_type_summary_OLD, match_type_summary_NEW)

for(col in cols) {
  match_type_summary <- match_type_summary %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

match_type_summary <- match_type_summary %>%
  select(order(colnames(match_type_summary))) %>% 
  select(MATCH_TYPE = MATCH_TYPE_OLD, everything(), -MATCH_TYPE_NEW)

saveRDS(match_type_summary, "data-raw/temp_comparison_data/match_type_summary.rds")

# group by rounded match scores to 1 d.p.
# count
scores_summary_OLD <- old_db %>% 
  mutate(SCORE = round(SCORE, 1)) %>% 
  group_by(SCORE) %>% 
  summarise(
    COUNT = n()
  ) %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/scores_summary_OLD.rds")

scores_summary_NEW <- new_db %>% 
  mutate(SCORE = round(SCORE, 1)) %>% 
  group_by(SCORE) %>% 
  summarise(
    COUNT = n()
  ) %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/scores_summary_NEW.rds")

scores_summary_OLD <- readRDS("data-raw/temp_comparison_data/scores_summary_OLD.rds")
cols <- names(scores_summary_OLD %>% select(-SCORE))

scores_summary_OLD <- scores_summary_OLD %>%
  arrange(SCORE) %>% 
  rename_with(~ paste0(.x, "_OLD"))
scores_summary_NEW <- readRDS("data-raw/temp_comparison_data/scores_summary_NEW.rds") %>% 
  arrange(SCORE) %>% 
  rename_with(~ paste0(.x, "_NEW"))

scores_summary <- bind_cols(scores_summary_OLD, scores_summary_NEW)

for(col in cols) {
  scores_summary <- scores_summary %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

scores_summary <- scores_summary %>%
  select(order(colnames(scores_summary))) %>% 
  select(SCORE = SCORE_OLD, everything(), -SCORE_NEW)

saveRDS(scores_summary, "data-raw/temp_comparison_data/scores_summary.rds")

# group by ch_flag
# count
ch_flag_summary_OLD <- old_db %>% 
  group_by(CH_FLAG) %>% 
  summarise(
    COUNT = n()
  ) %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/ch_flag_summary_OLD.rds")

ch_flag_summary_NEW <- new_db %>% 
  group_by(CH_FLAG) %>% 
  summarise(
    COUNT = n()
  ) %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/ch_flag_summary_NEW.rds")

ch_flag_summary_OLD <- readRDS("data-raw/temp_comparison_data/ch_flag_summary_OLD.rds")
cols <- names(ch_flag_summary_OLD %>% select(-CH_FLAG))

ch_flag_summary_OLD <- ch_flag_summary_OLD %>%
  arrange(CH_FLAG) %>% 
  rename_with(~ paste0(.x, "_OLD"))
ch_flag_summary_NEW <- readRDS("data-raw/temp_comparison_data/ch_flag_summary_NEW.rds") %>% 
  arrange(CH_FLAG) %>% 
  rename_with(~ paste0(.x, "_NEW"))

ch_flag_summary <- bind_cols(ch_flag_summary_OLD, ch_flag_summary_NEW)

for(col in cols) {
  ch_flag_summary <- ch_flag_summary %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

ch_flag_summary <- ch_flag_summary %>%
  select(order(colnames(ch_flag_summary))) %>% 
  select(CH_FLAG = CH_FLAG_OLD, everything(), -CH_FLAG_NEW)

saveRDS(ch_flag_summary, "data-raw/temp_comparison_data/ch_flag_summary.rds")

# group by uprn_flag
# count
uprn_flag_summary_OLD <- old_db %>% 
  group_by(UPRN_FLAG) %>% 
  summarise(
    COUNT = n()
  ) %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/uprn_flag_summary_OLD.rds")

uprn_flag_summary_NEW <- new_db %>% 
  group_by(UPRN_FLAG) %>% 
  summarise(
    COUNT = n()
  ) %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/uprn_flag_summary_NEW.rds")

uprn_flag_summary_OLD <- readRDS("data-raw/temp_comparison_data/uprn_flag_summary_OLD.rds")
cols <- names(uprn_flag_summary_OLD %>% select(-UPRN_FLAG))

uprn_flag_summary_OLD <- uprn_flag_summary_OLD %>%
  arrange(UPRN_FLAG) %>% 
  rename_with(~ paste0(.x, "_OLD"))
uprn_flag_summary_NEW <- readRDS("data-raw/temp_comparison_data/uprn_flag_summary_NEW.rds") %>% 
  arrange(UPRN_FLAG) %>% 
  rename_with(~ paste0(.x, "_NEW"))

uprn_flag_summary <- bind_cols(uprn_flag_summary_OLD, uprn_flag_summary_NEW)

for(col in cols) {
  uprn_flag_summary <- uprn_flag_summary %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

uprn_flag_summary <- uprn_flag_summary %>%
  select(order(colnames(uprn_flag_summary))) %>% 
  select(UPRN_FLAG = UPRN_FLAG_OLD, everything(), -UPRN_FLAG_NEW)

saveRDS(uprn_flag_summary, "data-raw/temp_comparison_data/uprn_flag_summary.rds")

# group by ch_flag and uprn_flag
# items count
# nic count
ch_uprn_flag_summary_OLD <- old_db %>% 
  group_by(CH_FLAG, UPRN_FLAG) %>% 
  summarise(
    SUM_ITEMS = sum(ITEM_COUNT),
    SUM_NIC = sum(ITEM_PAY_DR_NIC)
  ) %>% 
  ungroup() %>% 
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/ch_uprn_flag_summary_OLD.rds")

ch_uprn_flag_summary_NEW <- new_db %>% 
  group_by(CH_FLAG, UPRN_FLAG) %>% 
  summarise(
    SUM_ITEMS = sum(ITEM_COUNT),
    SUM_NIC = sum(ITEM_PAY_DR_NIC)
  ) %>% 
  ungroup() %>%
  collect() %>% 
  readRDS("data-raw/temp_comparison_data/ch_uprn_flag_summary_NEW.rds")

ch_uprn_flag_summary_OLD <- readRDS("data-raw/temp_comparison_data/ch_uprn_flag_summary_OLD.rds")
cols <- names(ch_uprn_flag_summary_OLD %>% select(-c(CH_FLAG, UPRN_FLAG)))

ch_uprn_flag_summary_OLD <- ch_uprn_flag_summary_OLD %>%
  arrange(CH_FLAG, UPRN_FLAG) %>% 
  rename_with(~ paste0(.x, "_OLD"))
ch_uprn_flag_summary_NEW <- readRDS("data-raw/temp_comparison_data/ch_uprn_flag_summary_NEW.rds") %>% 
  arrange(CH_FLAG, UPRN_FLAG) %>% 
  rename_with(~ paste0(.x, "_NEW"))

ch_uprn_flag_summary <- bind_cols(ch_uprn_flag_summary_OLD, ch_uprn_flag_summary_NEW)

for(col in cols) {
  ch_uprn_flag_summary <- ch_uprn_flag_summary %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

ch_uprn_flag_summary <- ch_uprn_flag_summary %>%
  select(order(colnames(ch_uprn_flag_summary))) %>% 
  select(
    CH_FLAG = CH_FLAG_OLD,
    UPRN_FLAG = UPRN_FLAG_OLD,
    everything(),
    -c(CH_FLAG_NEW, UPRN_FLAG_NEW)
  )

saveRDS(ch_uprn_flag_summary, "data-raw/temp_comparison_data/ch_uprn_flag_summary.rds")

# sum total items and total nic
item_nic_summary_OLD <- readRDS("data-raw/temp_comparison_data/ch_uprn_flag_summary_OLD.rds") %>% 
  summarise(
    SUM_ITEMS = sum(SUM_ITEMS),
    SUM_NIC = sum(SUM_NIC)
  ) %>% 
  readRDS("data-raw/temp_comparison_data/item_nic_summary_OLD.rds")

item_nic_summary_NEW <- readRDS("data-raw/temp_comparison_data/ch_uprn_flag_summary_NEW.rds") %>% 
  summarise(
    SUM_ITEMS = sum(SUM_ITEMS),
    SUM_NIC = sum(SUM_NIC)
  ) %>% 
  readRDS("data-raw/temp_comparison_data/item_nic_summary_NEW.rds")

item_nic_summary_OLD <- readRDS("data-raw/temp_comparison_data/item_nic_summary_OLD.rds")
cols <- names(item_nic_summary_OLD)

item_nic_summary_OLD <- item_nic_summary_OLD %>%
  rename_with(~ paste0(.x, "_OLD"))
item_nic_summary_NEW <- readRDS("data-raw/temp_comparison_data/item_nic_summary_NEW.rds") %>% 
  rename_with(~ paste0(.x, "_NEW"))

item_nic_summary <- bind_cols(item_nic_summary_OLD, item_nic_summary_NEW)

for(col in cols) {
  item_nic_summary <- item_nic_summary %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

item_nic_summary <- item_nic_summary %>%
  select(order(colnames(item_nic_summary))) %>% 
  select(everything())

saveRDS(item_nic_summary, "data-raw/temp_comparison_data/item_nic_summary.rds")

# sum NAs per column

sym_to_char <- function(...){
  as.character(ensyms(...))
}

common_cols <- intersect(names(old_db), names(new_db))

cols <- sym_to_char(
  MATCH_TYPE,
  SCORE,
  AB_FLAG,
  UPRN_FLAG,
  CH_FLAG,
  UPRN,
  PARENT_UPRN,
  NURSING_HOME_FLAG,
  RESIDENTIAL_HOME_FLAG,
  NUMBER_OF_BEDS,
  CURRENT_RATING,
  ITEM_COUNT,
  ITEM_PAY_DR_NIC
)

# Will remove in final code, just checking this works on a simple case
# dummy_cols <- sym_to_char(
#   dummy
# )
# 
# nulls_cmp <- tibble(dummy_OLD = c(1,2), dummy_NEW = c(3,4))
# 
# for(col in dummy_cols) {
#   nulls_cmp <- nulls_cmp %>% 
#     mutate(
#       !!sym(glue("{col}_RATIO")) := !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
#     )
# }

nulls_both <- NULL

for (col in cols) {
  nulls <- old_db %>% 
    select(all_of(col)) %>% 
    filter(is.na(!!sym(col))) %>% 
    tally() %>% 
    rename(!!sym(glue("{col}_OLD")) := 1) %>% 
    collect()
  
  if (is.null(nulls_both)) nulls_both <- nulls
  else nulls_both <- bind_cols(nulls_both, nulls)
  
  nulls <- new_db %>% 
    select(all_of(col)) %>% 
    filter(is.na(!!sym(col))) %>% 
    tally() %>% 
    rename(!!sym(glue("{col}_NEW")) := 1) %>% 
    collect()
  
  nulls_both <- bind_cols(nulls_both, nulls)
}

saveRDS(nulls_both, "nulls_both.rds")

nulls_cmp <- readRDS("data-raw/temp_comparison_data/nulls_both.rds")

for(col in cols) {
  nulls_cmp <- nulls_cmp %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

nulls_cmp <- nulls_cmp %>% select(order(colnames(nulls_cmp)))
saveRDS(nulls_cmp, "data-raw/temp_comparison_data/nulls_summary.rds")

# Do same for rest of common cols, could take a long time
common_cols <- setdiff(intersect(names(old_db), names(new_db)), cols)

nulls_both_rest <- NULL

for (col in common_cols) {
  nulls <- old_db %>% 
    select(all_of(col)) %>% 
    filter(is.na(!!sym(col))) %>% 
    tally() %>% 
    rename(!!sym(glue("{col}_OLD")) := 1) %>% 
    collect()
  
  if (is.null(nulls_both_rest)) nulls_both_rest <- nulls
  else nulls_both_rest <- bind_cols(nulls_both_rest, nulls)
  
  nulls <- new_db %>% 
    select(all_of(col)) %>% 
    filter(is.na(!!sym(col))) %>% 
    tally() %>% 
    rename(!!sym(glue("{col}_NEW")) := 1) %>% 
    collect()
  
  nulls_both_rest <- bind_cols(nulls_both_rest, nulls)
}

saveRDS(nulls_both_rest, "data-raw/temp_comparison_data/nulls_both_rest.rds")

nulls_cmp_rest <- readRDS("data-raw/temp_comparison_data/nulls_both_rest.rds")

for(col in common_cols) {
  nulls_cmp_rest <- nulls_cmp_rest %>% 
    mutate(
      !!sym(glue("{col}_RATIO")) := case_when(
        (!!sym(glue("{col}_NEW")) == 0) ~ 0,
        TRUE ~ !!sym(glue("{col}_OLD")) / !!sym(glue("{col}_NEW"))
      )
    )
}

nulls_cmp_rest <- nulls_cmp_rest %>% select(order(colnames(nulls_cmp_rest)))
saveRDS(nulls_cmp_rest, "data-raw/temp_comparison_data/nulls_rest_summary.rds")
