library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)
#devtools::install_github('nhsbsa-data-analytics/nhsbsaR')
library(nhsbsaR)


con <- con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

DB <- DB |> filter(UPRN_FLAG==1)

# PRESC to CH
P <- DB |>
     group_by(PRESC_ORG_NM, MATCH_SLA_STD, PRESC_ORG_CODE, PRESC_ORG_TYPE, PRESC_ORG_SUB_TYPE) |>
     summarise(
       NIC = sum(ITEM_PAY_DR_NIC/100),
       ITEMS = sum(ITEM_COUNT),
       .groups = "drop") |>
     rename(from = PRESC_ORG_NM, to = MATCH_SLA_STD) |>
     mutate(id = paste(from, to, sep = " -> "),
            # Highcharter's sankey diagram needs different PRESC/DISP names to draw them on different sides
            # If the same org (a dispensing doctor) both prescribed and dispensed, chart won't handle it well
            from = paste("PRESC", from)) |> 
     relocate(all_of(c("PRESC_ORG_CODE", "PRESC_ORG_TYPE", "PRESC_ORG_SUB_TYPE")), .after = id)

# CH to DISP
D <- DB |>
  group_by(MATCH_SLA_STD, DISP_NM, DISP_CODE, DISP_TYPE) |>
  summarise(
    NIC = sum(ITEM_PAY_DR_NIC/100),
    ITEMS = sum(ITEM_COUNT),
    .groups = "drop") |>
  rename(from = MATCH_SLA_STD, to = DISP_NM) |>
  mutate(id = paste(from, to, sep = " -> "),
         to = paste("DISP", to)) |>
  relocate(all_of(c("DISP_CODE", "DISP_TYPE")), .after = id)

LINKS <- union_all(P, D) |> collect_with_parallelism(12)

# 3 level Sankey diagram showing flow of NIC

# sample_of_uprns <- LINKS |> pull(UPRN) |> unique() |> sample(5)

top_CHs <- P |> group_by(to) |>
  summarise(ITEMS = sum(ITEMS)) |>
  slice_max(ITEMS, n = 5, with_ties = F) |>
  collect_with_parallelism(12) |>
  pull(to)

t <- LINKS |> 
  filter(to %in% top_CHs | from %in% top_CHs) |>
  rename(weight = ITEMS)

# Careful, when looking at a subset of CHs/UPRNs, sankey might falsely convey an impression that a given PRESC org prescribes entirely to one CH, whereas this is only because we kept activity to/from the focal top CHs
# Add all activity from involved presc and disp orgs to "other" non-top CHs...

t2 <- t |> bind_rows(
  
  # Presc activity from presc orgs that prescribed to top CHs to all other CHs
  LINKS |> filter(
    from %in% (LINKS |> filter(to %in% top_CHs) |> distinct(from) |> pull()), #PRESC ORGs involved in prescribing to top CHs
    !(to %in% top_CHs)) |> #... except the prescribing activity to the top CHs themselves
    group_by(from) |>
    summarise(weight = sum(ITEMS)) |>
    mutate(to = "OTHER")
) |>
  bind_rows(
    
    # Disp activity to disp orgs that dispensed to top CHs to all other CHs
    LINKS |> filter(
      to %in% (LINKS |> filter(from %in% top_CHs) |> distinct(to) |> pull()),
      !(from %in% top_CHs)) |>
      group_by(to) |>
      summarise(weight = sum(ITEMS)) |>
      mutate(from = "OTHER")
  )

highchart() |>
  hc_add_series(t,   # Showing prescribing activity from presc/disp orgs to focal CHs only 
                #t2, # Showing all presc activity from presc/disp orgs that had activity with focal CHs
                type = "sankey")

# Colour presc/disp by type?

# Distributions of % of items/nic prescribed/dispensed by a top org
# 3D graph: X = % prescribed by top presc org; Y = % dispensed by top disp org; Z (colour/size) = total items/nic


TOP_ORGS_PER_CH <- left_join( 
  
  LINKS |> filter(grepl("^PRESC", from)) |>
    group_by(to) |>
    mutate(ITEMS_PER_CH = sum(ITEMS),
           PROP_ITEMS_BY_PRESC_TO_CH = ITEMS/ITEMS_PER_CH) |>
    slice_max(PROP_ITEMS_BY_PRESC_TO_CH, n = 1, with_ties = F) |>
    ungroup() |>
    select(CH = to,
           ITEMS_PER_CH,
           PROP_ITEMS_BY_TOP_PRESC_TO_CH = PROP_ITEMS_BY_PRESC_TO_CH,
           PRESC = from),
  
  LINKS |> filter(grepl("^DISP", to)) |>
    group_by(from) |>
    mutate(ITEMS_PER_CH = sum(ITEMS),
           PROP_ITEMS_BY_DISP_FROM_CH = ITEMS/ITEMS_PER_CH) |>
    slice_max(PROP_ITEMS_BY_DISP_FROM_CH, n = 1, with_ties = F) |>
    ungroup() |>
    select(CH = from,
           PROP_ITEMS_BY_TOP_DISP_FROM_CH = PROP_ITEMS_BY_DISP_FROM_CH,
           DISP = to),
  
  by = "CH")

# 17,648 CHs

ggplot(TOP_ORGS_PER_CH, aes(ITEMS_PER_CH)) + 
  geom_histogram(bins = 150) +
  scale_x_continuous(limits = c(0,4000))

ecdf(TOP_ORGS_PER_CH$ITEMS_PER_CH)(300) # 40% of CHs (many of them likely 'faux') had <= 300 items

dom_fraction_threshold = 0.7 # What constitutes a dominant fraction of prescrbing/dispensing
low_activity_threshold = 300 # Exclude CHs with items below this threshold

ggplot(TOP_ORGS_PER_CH |> filter(ITEMS_PER_CH >= low_activity_threshold),
       aes(PROP_ITEMS_BY_TOP_PRESC_TO_CH, PROP_ITEMS_BY_TOP_DISP_FROM_CH, color = log(ITEMS_PER_CH))) +
  geom_point(alpha = 0.85) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  scale_color_distiller(palette = "RdYlBu") +
  geom_rect(ymin = dom_fraction_threshold, ymax = 1, xmin = dom_fraction_threshold, xmax = 1, fill = NA, color = "red", linewidth = 1.5) +
  xlab("Proportion of items prescribed by top presc org") +
  ylab("Proportion of items dispensed by top disp org") +
  annotate("text", x = dom_fraction_threshold+(1-dom_fraction_threshold)/2, y = dom_fraction_threshold-0.03, color = "red", size = 7,
           label = ((TOP_ORGS_PER_CH |> filter(ITEMS_PER_CH >= low_activity_threshold,
                                             PROP_ITEMS_BY_TOP_PRESC_TO_CH >= dom_fraction_threshold,
                                             PROP_ITEMS_BY_TOP_DISP_FROM_CH >= dom_fraction_threshold) |>
                                      nrow() / nrow(TOP_ORGS_PER_CH))*100) |>
             janitor::round_half_up() |> paste0("%"))

# Prop by dominant presc/disp org vs item volume

TOP_ORGS_PER_CH |>  filter(ITEMS_PER_CH >= low_activity_threshold,
                           ITEMS_PER_CH <= 10e3) |>
  tidyr::pivot_longer(starts_with("PROP"),
                      names_to = "PROP_TYPE",
                      values_to = "PROP") |>
  ggplot(aes(ITEMS_PER_CH, PROP)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PROP_TYPE)


# Group by Presc + Disp, count items/nic, n distinct CHs

# Reconnect to base table w/o UPRN_FLAG==1 filter, so we get all CH prescribing
DB <- tbl(con, in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

# All (CH and non-CH) activity between top-20 CH prescribing orgs and
# top-20 dispensers. Tooltips show the no. of CHs involved.

t3 <- DB |>
      filter(CH_FLAG==1) |>
      group_by(PRESC_ORG_NM, DISP_NM) |>
      summarise(
        NIC = sum(ITEM_PAY_DR_NIC/100),
        ITEMS = sum(ITEM_COUNT),
        CH_COUNT = n_distinct(MATCH_SLA_STD),
        .groups = "drop"
      ) |> collect_with_parallelism(8)

top_20_presc <- t3 |> group_by(PRESC_ORG_NM) |> 
  summarise(ITEMS = sum(ITEMS)) |>
  slice_max(ITEMS, n = 20, with_ties = F) |>
  pull(PRESC_ORG_NM)

top_20_disp <- t3 |> group_by(DISP_NM) |>
  summarise(ITEMS = sum(ITEMS)) |>
  slice_max(ITEMS, n = 20, with_ties = F) |>
  pull(DISP_NM)

t4 <- DB |> mutate(
  PRESC_ORG_NM = case_when(
    PRESC_ORG_NM %in% top_20_presc ~ PRESC_ORG_NM,
    T ~ "OTHER"
  ),
  DISP_NM = case_when(
    DISP_NM %in% top_20_disp ~ DISP_NM,
    T ~ "OTHER"
  )) |>
  filter(!(PRESC_ORG_NM=="OTHER" & DISP_NM=="OTHER")) |>
  group_by(PRESC_ORG_NM, DISP_NM) |>
  summarise(
    NIC = sum(ITEM_PAY_DR_NIC/100),
    ITEMS = sum(ITEM_COUNT),
    CH_COUNT = n_distinct(MATCH_SLA_STD),
    .groups = "drop"
  ) |> collect_with_parallelism(6)

t4 |> mutate(
  PRESC_ORG_NM = factor(PRESC_ORG_NM, levels = c(top_20_presc, "OTHER") |> rev()),
  DISP_NM = factor(DISP_NM, levels = c(top_20_disp, "OTHER"))
) |>
  hchart(type = "heatmap", hcaes(DISP_NM, PRESC_ORG_NM, value = log(ITEMS) |> janitor::round_half_up(2)),
             dataLabels = list(enabled = T,
                               format = "{point.value:.1f}",
                               #formatter = htmlwidgets::JS("function () { return Math.round(Math.exp(this.point.value)/1000 * 10) / 10 + 'k' ; }"),
                               #formatter = htmlwidgets::JS("function () { return (Math.round(this.point.ITEMS / 1000 * 10) / 10).toLocaleString() + 'k' ; }"),
                               style = list(fontSize = "8px"))) |>
  hc_colorAxis(
    minColor = "blue",
    maxColor = "red",
    tickAmount = 2,
    labels = list(enabled = F)
  ) |>
  hc_tooltip(
    formatter = htmlwidgets::JS(
      paste0(
        "
            function() {
                outHTML =
                  '<b>ITEMS: </b>' + this.point.ITEMS + '<br>' +
                  '<b>NIC: </b>' + '£' + Math.round(this.point.NIC).toLocaleString() + '<br>' +
                  '<b>Distinct carehomes: </b>' + this.point.CH_COUNT
                return outHTML;
            }
            "
  )))

# % CH prescribing and total volume by presc org

t5 <- DB |> group_by(PRESC_ORG_NM, CH_FLAG) |>
    summarise(ITEMS = sum(ITEM_COUNT),
              NIC = sum(ITEM_PAY_DR_NIC),
              .groups = "drop") |>
    group_by(PRESC_ORG_NM) |>
    mutate(
      TOTAL_ITEMS = sum(ITEMS),
      PROP_ITEMS_CH = ITEMS/TOTAL_ITEMS,
      TOTAL_NIC = sum(NIC),
      PROP_NIC_CH = NIC/TOTAL_NIC
      ) |>
    ungroup() |>
    arrange(PRESC_ORG_NM, CH_FLAG) |>
    collect_with_parallelism(18)

# Some presc orgs don't have any CH prescribing, eg (IRLAM) SALFORD CARE CTRS MEDICAL PRACTI

# Only relatively small practices specialise in CH prescribing

# PA angle? Identify presc/disp orgs that have a lot of CH activity and where CH activity is a high %
# of their total activity.

t5 |> filter(CH_FLAG==1, TOTAL_ITEMS>=100) |>
      mutate(CH_ITEMS = TOTAL_ITEMS*PROP_ITEMS_CH) |>
hchart(type = "scatter", hcaes(CH_ITEMS, PROP_ITEMS_CH)) |>
  hc_tooltip(
    formatter = htmlwidgets::JS(
      paste0(
        "
            function() {
                outHTML =
                  '<i>' + this.point.PRESC_ORG_NM + '</i><br>' + 
                  '<b>CH ITEMS: </b>' + this.point.CH_ITEMS + '<br>' +
                  '<b>PROPORTION OF ITEMS TO CHs: </b>' + this.point.PROP_ITEMS_CH + '<br>'
                return outHTML;
            }
            "
      )))


t6 <- DB |> group_by(DISP_NM, CH_FLAG) |>
  summarise(ITEMS = sum(ITEM_COUNT),
            NIC = sum(ITEM_PAY_DR_NIC),
            .groups = "drop") |>
  group_by(DISP_NM) |>
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP_ITEMS_CH = ITEMS/TOTAL_ITEMS,
    TOTAL_NIC = sum(NIC),
    PROP_NIC_CH = NIC/TOTAL_NIC
  ) |>
  ungroup() |>
  arrange(DISP_NM, CH_FLAG) |>
  collect_with_parallelism(18)

t6 |> filter(CH_FLAG==1, TOTAL_NIC<=1e9) |>
      mutate(CH_NIC = TOTAL_NIC*PROP_NIC_CH) |>
hchart(type = "scatter", hcaes(CH_NIC, PROP_NIC_CH)) |>
  #hc_xAxis(type = "logarithmic") |>
  hc_tooltip(
    formatter = htmlwidgets::JS(
      paste0(
        "
            function() {
                outHTML =
                  '<i>' + this.point.DISP_NM + '</i><br>' + 
                  '<b>CH NIC: </b>' + this.point.CH_NIC + '<br>' +
                  '<b>PROPORTION OF NIC TO CHs: </b>' + this.point.PROP_NIC_CH + '<br>'
                return outHTML;
            }
            "
      )))


# Zoom-in on dispensing doctors

DB |> group_by(PRESCRIBER_SUB_TYPE) |>
  summarise(n = n()) |>
  collect_with_parallelism(10) -> t

# Prop of self-dispensing among dispensing doctors

t7 <- DB |> filter(PRESCRIBER_SUB_TYPE=="DISPENSING ONLY DOCTOR") |>
  mutate(SELF_DISP = ifelse(PRESC_ORG_CODE==DISP_CODE, 1L, 0L)) |>
  group_by(PRESC_ORG_NM, SELF_DISP) |>
  summarise(ITEMS = sum(ITEM_COUNT),
            NIC = sum(ITEM_PAY_DR_NIC),
            .groups = "drop") |>
  group_by(PRESC_ORG_NM) |>
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP_ITEMS_SD = ITEMS/TOTAL_ITEMS,
    TOTAL_NIC = sum(NIC),
    PROP_NIC_SD = NIC/TOTAL_NIC
  ) |>
  ungroup() |>
  arrange(PRESC_ORG_NM, SELF_DISP) |>
  collect_with_parallelism(18)

t7 |> filter(SELF_DISP==1 & ITEMS>=100) |>
  tidyr::pivot_longer(starts_with("PROP"), names_to = "PROP_TYPE", values_to = "PROP") |>
  mutate(PROP_TYPE = case_when(
    PROP_TYPE == "PROP_ITEMS_SD" ~ "Proportion of self-dispensed items",
    PROP_TYPE == "PROP_NIC_SD" ~ "Proportion of self-dispensed NIC"
  )) |>
  ggplot(aes(PROP)) + 
  geom_histogram() +
  facet_wrap(~PROP_TYPE, ncol = 1)

# Bubble chart: prop of self-dispensed items/nic, prop of self-dispensed items/nic to CHs, size = total items/nic
  
t8 <- DB |> filter(PRESCRIBER_SUB_TYPE=="DISPENSING ONLY DOCTOR") |>
  mutate(SELF_DISP = ifelse(PRESC_ORG_CODE==DISP_CODE, 1L, 0L)) |>
  group_by(PRESC_ORG_NM, SELF_DISP, CH_FLAG) |>
  summarise(ITEMS = sum(ITEM_COUNT),
            NIC = sum(ITEM_PAY_DR_NIC),
            .groups = "drop") |>
  group_by(PRESC_ORG_NM) |>
  mutate(
    TOTAL_ITEMS = sum(ITEMS),
    PROP_ITEMS_SD_CH = ITEMS/TOTAL_ITEMS,
    TOTAL_NIC = sum(NIC),
    PROP_NIC_SD_CH = NIC/TOTAL_NIC
  ) |>
  ungroup() |>
  arrange(PRESC_ORG_NM, SELF_DISP, CH_FLAG) |>
  collect_with_parallelism(18)

t9 <- left_join(t7 |> filter(SELF_DISP==1),
                t8 |> filter(SELF_DISP==1 & CH_FLAG==1) |> select(PRESC_ORG_NM, PROP_ITEMS_SD_CH, PROP_NIC_SD_CH),
                by="PRESC_ORG_NM")

ggplot(t9, aes(PROP_ITEMS_SD, PROP_ITEMS_SD_CH)) + geom_point(aes(color = TOTAL_ITEMS)) +
  scale_color_distiller(palette = "RdYlBu")

DBI::dbDisconnect(con)
