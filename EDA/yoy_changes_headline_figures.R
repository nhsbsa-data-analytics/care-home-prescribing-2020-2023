library(dplyr)
library(dbplyr)
library(nhsbsaR)

base_table <- "INT646_BASE_20200401_20250331"

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Base table
data_db <- con %>%
  tbl(from = in_schema("DALL_REF", base_table))

key_findings_fy_ch_flag <- data_db %>%
  group_by(FY, CH_FLAG) %>%
  summarise(
    PATS = n_distinct(NHS_NO),
    ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
    NIC = sum(ITEM_PAY_DR_NIC, na.rm = TRUE) / 100
  ) %>%
  ungroup() %>%
  nhsbsaR::collect_with_parallelism(., 16)

# Patients ----------------------------------------------------------------

df_pats <- key_findings_fy_ch_flag %>%
  select(FY, CH_FLAG, PATS) %>%
  arrange(FY)

df_pats_ch <- filter(df_pats, CH_FLAG == 1)
df_pats_nch <- filter(df_pats, CH_FLAG == 0)

df_pats_ch <- df_pats_ch %>%
  mutate(
    PREV_PATS = lag(PATS),
    YOY_CHANGE_CH = (PATS - PREV_PATS) / PREV_PATS * 100,
    PATS_CH = PATS
  ) %>%
  select(FY, PATS_CH, YOY_CHANGE_CH)

df_pats_nch <- df_pats_nch %>%
  mutate(
    PREV_PATS = lag(PATS),
    YOY_CHANGE_NCH = (PATS - PREV_PATS) / PREV_PATS * 100,
    PATS_NCH = PATS
  ) %>%
  select(FY, PATS_NCH, YOY_CHANGE_NCH)

df_pats_yoy_change <- left_join(df_pats_ch, df_pats_nch)


# Items -------------------------------------------------------------------

df_items <- key_findings_fy_ch_flag %>%
  select(FY, CH_FLAG, ITEMS) %>%
  arrange(FY)

df_items_ch <- filter(df_items, CH_FLAG == 1)
df_items_nch <- filter(df_items, CH_FLAG == 0)

df_items_ch <- df_items_ch %>%
  mutate(
    PREV_ITEMS = lag(ITEMS),
    YOY_CHANGE_CH = (ITEMS - PREV_ITEMS) / PREV_ITEMS * 100,
    ITEMS_CH = ITEMS
  ) %>%
  select(FY, ITEMS_CH, YOY_CHANGE_CH)

df_items_nch <- df_items_nch %>%
  mutate(
    PREV_ITEMS = lag(ITEMS),
    YOY_CHANGE_NCH = (ITEMS - PREV_ITEMS) / PREV_ITEMS * 100,
    ITEMS_NCH = ITEMS
  ) %>%
  select(FY, ITEMS_NCH, YOY_CHANGE_NCH)

df_items_yoy_change <- left_join(df_items_ch, df_items_nch)


# NIC ---------------------------------------------------------------------

# Items -------------------------------------------------------------------

df_nic <- key_findings_fy_ch_flag %>%
  select(FY, CH_FLAG, NIC) %>%
  arrange(FY)

df_nic_ch <- filter(df_nic, CH_FLAG == 1)
df_nic_nch <- filter(df_nic, CH_FLAG == 0)

df_nic_ch <- df_nic_ch %>%
  mutate(
    PREV_NIC = lag(NIC),
    YOY_CHANGE_CH = (NIC - PREV_NIC) / PREV_NIC * 100,
    NIC_CH = NIC
  ) %>%
  select(FY, NIC_CH, YOY_CHANGE_CH)

df_nic_nch <- df_nic_nch %>%
  mutate(
    PREV_NIC = lag(NIC),
    YOY_CHANGE_NCH = (NIC - PREV_NIC) / PREV_NIC * 100,
    NIC_NCH = NIC
  ) %>%
  select(FY, NIC_NCH, YOY_CHANGE_NCH)

df_nic_yoy_change <- left_join(df_nic_ch, df_nic_nch)
