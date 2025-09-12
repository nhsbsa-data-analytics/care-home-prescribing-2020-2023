

# Library
library(dplyr)
library(highcharter)

# Parameters
metric_name = "% of total annual number of prescription items"
geography_parent = "ICS"
geography_child = "NHS Northamptonshire ICB"
bnf_parent = "Chemical substance"
bnf_child = "Codeine phosphate"

# Text
title_text = paste0(
  "Annual distribution of ",
  metric_name,
  " value for the chemical substance ",
  bnf_child,
  " with ",
  geography_child,
  " highlighted"
)

# Filtered data
df = mod_geo_ch_flag_drug_df %>% 
  filter(
    GEOGRAPHY_PARENT == geography_parent,
    BNF_PARENT == bnf_parent,
    METRIC == metric_name,
    BNF_CHILD == bnf_child
  )

# Box data
box_data <- df %>%
  group_by(FY) %>%
  summarise(
    low = min(VALUE),
    q1 = quantile(VALUE, 0.25),
    median = median(VALUE),
    q3 = quantile(VALUE, 0.75),
    high = max(VALUE)
  ) %>% 
  select(FY, low, q1, median, q3, high) %>% 
  arrange(FY) %>% 
  list_parse2()

# Point data
point_data = df %>%
  filter(GEOGRAPHY_CHILD == "NHS Northamptonshire ICB") %>% 
  select(FY, VALUE) %>% 
  #mutate(x = match(x, unique(x)) - 1) %>% 
  arrange(FY) %>% 
  list_parse2()

# Chart
highchart() %>%
  hc_chart(
    type = "boxplot", 
    inverted = T
  ) %>%
  hc_title(text = title_text) %>%
  hc_xAxis(
    categories = sort(unique(df$FY)),
    title = list(text = "<b>Financial year</b>")
  ) %>%
  hc_add_series(
    data = box_data, 
    type = "boxplot", 
    color = nhsbsaR::palette_nhsbsa()[1],
    tooltip = list(
      pointFormat = paste0(
        'Min: {point.low}<br/>',
        'Q1: {point.q1}<br/>',
        'Median: {point.median}<br/>',
        'Q3: {point.q3}<br/>',
        'Max: {point.high}<br/>'
      )
    )
  ) %>% 
  hc_add_series(
    data = point_data, 
    type = "scatter", 
    color = nhsbsaR::palette_nhsbsa()[2], 
    marker = list(
      symbol = "square", 
      radius = 7
    ),
    tooltip = list(
      pointFormat = 'Point: {point.name}<br/>Value: {point.y}<br/>'
    )
  ) %>% 
  hc_plotOptions(
    series = list(
      states = list(
        inactive = list(
          enabled = FALSE
        ),
        hover = list(
          enabled = FALSE
        )
      )
    )
  ) %>% 
  hc_legend(enabled = FALSE) %>% 
  hc_tooltip(
    shared = TRUE,
    headerFormat = '<b>Financial Year: {point.key}</b><br/>'
  )
