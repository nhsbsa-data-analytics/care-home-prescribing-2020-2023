library(dplyr)
library(dbplyr)
library(highcharter)
library(ggplot2)

con <- nhsbsaR::con_nhsbsa(database = "DALP")

DB <- tbl(con, in_schema("ADNSH", "INT646_BASE_20210401_20220331"))

DB <- DB |> filter(UPRN_FLAG==1)

# PRESC to CH
P <- DB |>
     group_by(PRESC_ORG_NM, UPRN, PRESC_ORG_CODE, PRESC_ORG_TYPE, PRESC_ORG_SUB_TYPE) |>
     summarise(
       SLA = max(MATCH_SINGLE_LINE_ADDRESS),
       NIC = sum(ITEM_PAY_DR_NIC/100),
       ITEMS = sum(ITEM_COUNT),
       .groups = "drop") |>
     rename(from = PRESC_ORG_NM, to = SLA) |>
     mutate(id = paste(from, to, sep = " -> "),
            # Highcharter's sankey diagram needs different PRESC/DISP names to draw them on different sides
            # If the same org (a dispensing doctor) both prescribed and dispensed, chart won't handle it well
            from = paste("PRESC", from)) |> 
     relocate(all_of(c("PRESC_ORG_CODE", "PRESC_ORG_TYPE", "PRESC_ORG_SUB_TYPE")), .after = id) |>
     # We ensured there is only one max CH SLA per presc org, but different presc orgs could still have different variants, so:
     group_by(UPRN) |>
     mutate(to = max(to)) |>
     # Same SLA may still have different UPRNs
     group_by(to) |>
     mutate(UPRN = max(UPRN)) |>
     ungroup()

# Incorporate this 1:1 UPRN:SLA logic into base table code?

#DB |> group_by(DISP_TYPE, DISP_DIST_FLAG, DISP_LPS_FLAG, DISP_OOH_FLAG, DISP_PRIVATE_FLAG, DISP_APPLIANCE_FLAG) |> summarise(n = n())
# None of the dispensers have OOH or Private flags in this dataset, only pharmacy contractors have mutually exclusive dist/lps/appliance flags, therefore:
DB <- DB |> mutate(DISP_TYPE = case_when(
  DISP_DIST_FLAG == "Y" ~ "PHARMACY CONTRACTOR: DISTANCE SELLING",
  DISP_LPS_FLAG == "Y" ~ "PHARMACY CONTRACTOR: LPS",
  DISP_APPLIANCE_FLAG == "Y" ~ "PHARMACY CONTRACTOR: APPLIANCE",
  DISP_OOH_FLAG == "Y" ~ "PHARMACY CONTRACTOR: OOH",
  DISP_PRIVATE_FLAG == "Y" ~ "PHARMACY CONTRACTOR: PRIVATE",
  T ~ DISP_TYPE
))


# CH to DISP
D <- DB |>
  group_by(UPRN, DISP_NM, DISP_CODE, DISP_TYPE) |>
  summarise(
    SLA = max(MATCH_SINGLE_LINE_ADDRESS),
    NIC = sum(ITEM_PAY_DR_NIC/100),
    ITEMS = sum(ITEM_COUNT),
    .groups = "drop") |>
  rename(from = SLA, to = DISP_NM) |>
  mutate(id = paste(from, to, sep = " -> "),
         to = paste("DISP", to)) |>
  relocate(all_of(c("DISP_CODE", "DISP_TYPE")), .after = id) |>
  group_by(UPRN) |>
  mutate(from = max(from)) |>
  group_by(from) |>
  mutate(UPRN = max(UPRN)) |>
  ungroup()

LINKS <- union_all(P, D) |> collect()

# 3 level Sankey diagram showing flow of NIC

# sample_of_uprns <- LINKS |> pull(UPRN) |> unique() |> sample(5)

top_uprns <- P |> group_by(UPRN) |>
  summarise(ITEMS = sum(ITEMS)) |>
  slice_max(ITEMS, n = 5, with_ties = F) |>
  collect() |>
  pull(UPRN)

t <- LINKS |> 
  filter(UPRN %in% top_uprns) |>
  rename(weight = ITEMS)

# Careful, when looking at a subset of CHs/UPRNs, sankey might falsely convey an impression that a given PRESC org prescribes entirely to one CH, whereas this is only because we excluded most other CHs
# Add all activity from involved presc and disp orgs to "other" CHs...

t2 <- t |> bind_rows(
  
  # Presc activity from presc orgs that prescribed to top CHs to all other CHs
  LINKS |> filter(
    from %in% (LINKS |> filter(UPRN %in% top_uprns,
                               grepl("^PRESC", from)) |> distinct(from) |> pull()),
    !(UPRN %in% top_uprns)) |>
    group_by(from) |>
    summarise(weight = sum(ITEMS)) |>
    mutate(to = "OTHER")
) |>
  bind_rows(
    
    # Disp activity to disp orgs that dispensed to top CHs to all other CHs
    LINKS |> filter(
      to %in% (LINKS |> filter(UPRN %in% top_uprns,
                               grepl("^DISP", to)) |> distinct(to) |> pull()),
      !(UPRN %in% top_uprns)) |>
      group_by(to) |>
      summarise(weight = sum(ITEMS)) |>
      mutate(from = "OTHER")
  )

highchart() |>
  hc_add_series(t,   # Showing prescribing activity from presc/disp orgs to focal CHs only 
                #t2, # Showing all presc activity from presc/disp orgs that had activity with focal CHs
                type = "sankey")

# Colour presc/disp by type

# Distributions of % of items/nic prescribed/dispensed by a top org
# 3D graph: X = % prescribed by top presc org; Y = % dispensed by top disp org; Z (colour/size) = total items/nic

TOP_ORGS_PER_CH <- left_join( 
  
  LINKS |> filter(grepl("^PRESC", from)) |>
    group_by(to, UPRN) |>
    mutate(ITEMS_PER_CH = sum(ITEMS)) |>
    mutate(PROP_ITEMS_BY_PRESC_TO_CH = ITEMS/ITEMS_PER_CH) |>
    slice_max(PROP_ITEMS_BY_PRESC_TO_CH, n = 1, with_ties = F) |>
    ungroup() |>
    select(CH = to, UPRN,
           ITEMS_PER_CH,
           PROP_ITEMS_BY_TOP_PRESC_TO_CH = PROP_ITEMS_BY_PRESC_TO_CH,
           PRESC = from),
  
  LINKS |> filter(grepl("^DISP", to)) |>
    group_by(from, UPRN) |>
    mutate(ITEMS_PER_CH = sum(ITEMS)) |>
    mutate(PROP_ITEMS_BY_DISP_FROM_CH = ITEMS/ITEMS_PER_CH) |>
    slice_max(PROP_ITEMS_BY_DISP_FROM_CH, n = 1, with_ties = F) |>
    ungroup() |>
    select(CH = from, UPRN,
           PROP_ITEMS_BY_TOP_DISP_FROM_CH = PROP_ITEMS_BY_DISP_FROM_CH,
           DISP = to),
  
  by = c("CH", "UPRN"))

# 17,411 CHs

ggplot(TOP_ORGS_PER_CH, aes(ITEMS_PER_CH)) + 
  geom_histogram(bins = 150) +
  scale_x_continuous(limits = c(0,4000))

ecdf(TOP_ORGS_PER_CH$ITEMS_PER_CH)(300) # 39% of CHs (many of them likely 'faux') had <= 300 items

dom_fraction_threshold = 0.7 # What constitutes a dominant fraction of prescrbing/dispensing
low_activity_threshold = 300 # Exclude CHs with items below this threshold

ggplot(TOP_ORGS_PER_CH |> filter(ITEMS_PER_CH >= low_activity_threshold),
       aes(PROP_ITEMS_BY_TOP_PRESC_TO_CH, PROP_ITEMS_BY_TOP_DISP_FROM_CH, color = ITEMS_PER_CH^(1/3))) +
  geom_point(alpha = 0.85) +
  scale_color_distiller(palette = "RdYlBu") +
  geom_rect(ymin = dom_fraction_threshold, ymax = 1, xmin = dom_fraction_threshold, xmax = 1, fill = NA, color = "red", linewidth = 1.5) +
  xlab("Proportion of items prescribed by top presc org") +
  ylab("Proportion of items dispensed by top disp org") +
  annotate("text", x = dom_fraction_threshold+(1-dom_fraction_threshold)/2, y = dom_fraction_threshold-0.03, color = "red", size = 7,
           label = ((TOP_ORGS_PER_CH |> filter(ITEMS_PER_CH >= low_activity_threshold,
                                             PROP_ITEMS_BY_TOP_PRESC_TO_CH >= dom_fraction_threshold,
                                             PROP_ITEMS_BY_TOP_DISP_FROM_CH >= dom_fraction_threshold) |>
                                      nrow() / nrow(TOP_ORGS_PER_CH))*100) |>
                   round() |> paste0("%"))

# Prop by dominant presc/disp org vs item volume

TOP_ORGS_PER_CH |>  filter(ITEMS_PER_CH >= low_activity_threshold) |>
  tidyr::pivot_longer(starts_with("PROP"),
                      names_to = "PROP_TYPE",
                      values_to = "PROP") |>
  ggplot(aes(ITEMS_PER_CH, PROP)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~PROP_TYPE)



DBI::dbDisconnect(con)
