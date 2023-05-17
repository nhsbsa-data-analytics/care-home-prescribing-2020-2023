
# Get data
source("R/analysis_packages.R")
source("R/workflow_helpers.R")

# Other libraries
library(reactable)

# Set up connection to DALP
con <- nhsbsaR::con_nhsbsa(database = "DALP")

# Care part 2 analysis plan ----------------------------------------------------

# Create lazy table
year <- con %>%
  tbl(from = in_schema("DIM", "YEAR_MONTH_DIM"))

# Mod 1.1
year %>% 
  filter(FINANCIAL_YEAR %in% c("2020/2021", "2021/2022", "2022/2023")) %>% 
  select(YEAR_MONTH) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(
    PATS = round(runif(n = 36, min = 250000, max = 350000)),
    YEAR_MONTH = as.factor(YEAR_MONTH)
    ) %>% 
  arrange(YEAR_MONTH) %>% 
  hchart(., "line", hcaes(YEAR_MONTH, PATS)) %>% 
  hc_yAxis(min = 0)

# Mod 1.2
year %>% 
 filter(FINANCIAL_YEAR %in% c("2020/2021", "2021/2022", "2022/2023")) %>% 
 select(FINANCIAL_YEAR) %>% 
 distinct() %>% 
 collect() %>% 
 mutate(
   PATS = round(runif(n = 3, min = 250000, max = 350000)),
   FINANCIAL_YEAR = as.character(as.factor(FINANCIAL_YEAR))
 ) %>% 
 arrange(FINANCIAL_YEAR) %>% 
 hchart(., "column", hcaes(FINANCIAL_YEAR, PATS))

# Mod 2.1
cross_join(
  x = data.frame(AGE_GROUP = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
  y = data.frame(YEAR = c("2020/21", "2021/22", "20222/23"))
  ) %>% 
  mutate(
    MALE = round(runif(n = 18, min = 25000, max = 50000)),
    FEMALE = round(runif(n = 18, min = 60000, max = 125000))
  ) %>% 
  tidyr::pivot_longer(!c('AGE_GROUP', "YEAR")) %>% 
  mutate(GROUP = paste0(YEAR, ' - ', name)) %>% 
  hchart(., "line", hcaes(AGE_GROUP, value, group = GROUP))


# Mod 2.2

# List for facet
charts = list()

# Facet 1
charts[[1]] = cross_join(
  x = data.frame(AGE_GROUP = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
  y = data.frame(YEAR = c("2020/21"))
  ) %>% 
  mutate(
    MALE = round(runif(n = 6, min = 25000, max = 50000)),
    FEMALE = round(runif(n = 6, min = 90000, max = 110000))
  ) %>% 
  tidyr::pivot_longer(!c('AGE_GROUP', "YEAR")) %>% 
  mutate(GROUP = paste0(YEAR, ' - ', name)) %>% 
  hchart(., "line", hcaes(AGE_GROUP, value, group = GROUP))

# Facet 2
charts[[2]] = cross_join(
  x = data.frame(AGE_GROUP = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
  y = data.frame(YEAR = c("2021/22"))
  ) %>% 
  mutate(
    MALE = round(runif(n = 6, min = 25000, max = 50000)),
    FEMALE = round(runif(n = 6, min = 90000, max = 110000))
  ) %>% 
  tidyr::pivot_longer(!c('AGE_GROUP', "YEAR")) %>% 
  mutate(GROUP = paste0(YEAR, ' - ', name)) %>% 
  hchart(., "line", hcaes(AGE_GROUP, value, group = GROUP))

# Facet 3
charts[[3]] = cross_join(
  x = data.frame(AGE_GROUP = c("65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
  y = data.frame(YEAR = c("2021/22"))
  ) %>% 
  mutate(
    MALE = round(runif(n = 6, min = 25000, max = 50000)),
    FEMALE = round(runif(n = 6, min = 90000, max = 110000))
  ) %>% 
  tidyr::pivot_longer(!c('AGE_GROUP', "YEAR")) %>% 
  mutate(GROUP = paste0(YEAR, ' - ', name)) %>% 
  hchart(., "line", hcaes(AGE_GROUP, value, group = GROUP))

# Grid plot
if (interactive()) {
  hw_grid(charts, rowheight = 300)
}

# Mod 3
# IMD Chart - copied and pasted from elsewhere

# Mod 4: National metrics
metric_df = cross_join(
  x = data.frame(`Metric` = c("Drug Cost", "No. of Items", "Unique Meds", "Unique Meds 10+", "Poly Metric One", "Poly Metric Two")),
  y = data.frame(`Year` = c("20/21", "21/22", "22/23"))
  ) %>% 
  mutate(
    `Care home` = round(runif(n = 18, min = 90, max = 110)),
    `Residential home` = round(runif(n = 18, min = 100, max = 130)),
    `Nursing home` = round(runif(n = 18, min = 70, max = 90)),
    `Non-Care home` = round(runif(n = 18, min = 40, max = 60))
  )

# Max value for plot
max_val = metric_df %>% 
  filter(Metric == "Drug Cost") %>% 
  select(-Metric) %>% 
  pivot_longer(!Year) %>% 
  filter(value == max(value)) %>% 
  select(value) %>% 
  distinct() %>% 
  pull()

# List plus 4 facets
charts = list()
charts[[1]] = hchart(metric_df %>% filter(Metric == "Drug Cost"), "column", hcaes(Year, `Care home`)) %>% hc_yAxis(min = 0, max = max_val)
charts[[2]] = hchart(metric_df %>% filter(Metric == "Drug Cost"), "column", hcaes(Year, `Residential home`)) %>% hc_yAxis(min = 0, max = max_val)
charts[[3]] = hchart(metric_df %>% filter(Metric == "Drug Cost"), "column", hcaes(Year, `Nursing home`)) %>% hc_yAxis(min = 0, max = max_val)
charts[[4]] = hchart(metric_df %>% filter(Metric == "Drug Cost"), "column", hcaes(Year, `Non-Care home`)) %>% hc_yAxis(min = 0, max = max_val)

# Grid plots as 2 x 2 box
if (interactive()) {
  hw_grid(charts, rowheight = 300, ncol = 2)
}

# Mod 5: geographic metrics table

# Create lazy table
la <- con %>%
  tbl(from = in_schema("DALL_REF", "ONS_AREA_CLASSIFICATION_LA"))

# Generate ls data
la_df = la %>% 
  select(LA_NAME) %>% 
  distinct() %>% 
  arrange(LA_NAME) %>% 
  collect() %>% 
  mutate(
    `Care home 2020/21` = round(runif(n = 326, min = 90, max = 110)),
    `Care home 2021/22` = round(runif(n = 326, min = 90, max = 110)),
    `Care home 2022/23` = round(runif(n = 326, min = 90, max = 110)),
    `Non-Care home 2020/21` = round(runif(n = 326, min = 40, max = 60)),
    `Non-Care home 2021/22` = round(runif(n = 326, min = 40, max = 60)),
    `Non-Care home 2022/23` = round(runif(n = 326, min = 40, max = 60))
    ) %>% 
  rename_at("LA_NAME", ~"Local Authority")

# Display as reactable table
reactable(la_df ,
          style = list (fontFamily = 'Arial', fontSize = '14px'),
          filterable = T,
          pagination = F,
          columns = list(
            `Care home 2020/21` = colDef(name = "20/21"),
            `Care home 2021/22` = colDef(name = "21/22"),
            `Care home 2022/23` = colDef(name = "22/23"),
            `Non-Care home 2020/21` = colDef(name = "20/21"),
            `Non-Care home 2021/22` = colDef(name = "21/22"),
            `Non-Care home 2022/23` = colDef(name = "22/23")
          ),
          columnGroups = list(
            colGroup(name = "Care Home", columns = c("Care home 2020/21", "Care home 2021/22", "Care home 2022/23")),
            colGroup(name = "Non-Care Home", columns = c("Non-Care home 2020/21", "Non-Care home 2021/22", "Non-Care home 2022/23"))
          ))

# Mod 7: drugs

# Drug info
drug = read.csv("../../Desktop/metrics_by_bnf_and_ch_flag_df (1).csv") %>% 
  clean_names()

# Line plus dots
highchart() %>% 
  hc_add_series(drug, "point", hcaes(bnf_description, non_care_home_percentage), color = "orange") %>% 
  hc_add_series(drug, "line", hcaes(bnf_description, care_home_percentage), color = "darkblue", pointWidth = 0.5) %>% 
  hc_xAxis(categories = drug %>% select(bnf_description) %>% pull()) %>% 
  hc_chart(inverted = T) %>% 
  hc_plotOptions(bar = list(pointWidth = 0.1))

# Line plus spline
highchart() %>% 
  hc_add_series(drug, "spline", hcaes(bnf_description, non_care_home_percentage), color = "orange", dashStyle = "Dot") %>% 
  hc_add_series(drug, "line", hcaes(bnf_description, care_home_percentage), color = "darkblue", pointWidth = 0.5) %>% 
  hc_xAxis(categories = drug %>% select(bnf_description) %>% pull()) %>% 
  hc_chart(inverted = T)

# Heatmap prep
a = drug %>% 
  filter(bnf_description != "") %>% 
  select(care_home_percentage, non_care_home_percentage) %>% 
  mutate(care_home_percentage = ifelse(is.na(care_home_percentage), 0, care_home_percentage)) %>% 
  mutate(
    col = 0,
    row = row_number() - 1,
    id = row
    ) %>% 
  select(id, col, row, care_home_percentage, non_care_home_percentage)

# Care home heatmap series
b = a %>% 
  group_by(id) %>% 
  do(item = list(
    x = .$col,
    y = .$row,
    value = .$care_home_percentage)
    )%>% 
  .$item

# Non care home heatmap series
c = a %>% 
  group_by(id) %>% 
  do(item = list(
    x = .$col + 1,
    y = .$row,
    value = .$non_care_home_percentage)
  )%>% 
  .$item

# Heatmap
highchart() %>% 
  hc_chart(type = "heatmap") %>% 
  hc_yAxis(categories = drug %>% select(bnf_description) %>% pull()) %>% 
  hc_xAxis(categories = c("Care home", "Non Care home")) %>% 
  hc_add_series(data = b, borderWidth = 0.3, borderColor = "black", dataLabels = list(enabled = T)) %>% 
  hc_add_series(data = c, borderWidth = 0.3, borderColor = "black", dataLabels = list(enabled = T)) %>% 
  hc_colorAxis(minColor = "lightblue", maxColor = "darkblue") %>% 
  hc_chart(marginLeft = 550)

# Difference highlight chart

# Results item table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Join and process tables then collect aggregated data
fact_df <- fact_db %>%
  group_by(CHAPTER_DESCR, CH_FLAG) %>%
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  group_by(CH_FLAG) %>% 
  mutate(PROP = ITEMS / sum(ITEMS)) %>% 
  ungroup() %>% 
  select(CHAPTER_DESCR, CH_FLAG, PROP) %>% 
  collect()

# Process results ready for plot
chapter_data <- fact_df %>% 
  pivot_wider(names_from = "CH_FLAG", values_from = "PROP") %>% 
  mutate(
    DIFF = round(`Care home` - `Non-care home`, 4) * 100,
    GREATER = case_when(
      DIFF > 0 ~ "Care homes",
      DIFF < 0 ~ "Non-care homes",
      T ~ "Neither"),
    `Care home` = round(`Care home`, 4) * 100,
    `Non care home` = round(`Non-care home`, 4) * 100,
  ) %>% 
  arrange(desc(DIFF))

# Plot
hchart(chapter_data, "spline", hcaes(CHAPTER_DESCR, DIFF), color = "green") %>% 
  hc_yAxis(plotBands = list(
    list(
      color = "#ffc0cb",
      from = 0,
      to = -30
    ),
    list(
      color = "lightblue",
      from = 0,
      to = 30
    ))) %>% 
  hc_yAxis(min = -20.5, max = 10.7, inverted = T) %>% 
  hc_plotOptions(spline = list(marker = list(radius = 0))) %>% 
  hc_xAxis(title = list(text = "BNF Chapter Description")) %>% 
  hc_yAxis(title = list(
    text = "Difference between Care home and <br> non-care home proportions")
  ) %>% 
  hc_title(
    text = "<b>The Difference in BNF Chapter-level Prescribing 
    in Care Home and non-Care Homes<b/>"
  ) %>% 
  hc_subtitle(
    text = "Proportions calculated by looking at chapter prescribing against 
    all prescribing<br>either on a care home or non-care home-level"
  ) %>% 
  hc_tooltip(
    headerFormat = "",
    pointFormat = "<b>BNF Chapter Description:</b> {point.CHAPTER_DESCR}<br>
    <b>Proportion of Care Home Prescribing:</b> {point.Care home} %<br>
    <b>Proportion of Non-Care Home Prescribing:</b> {point.Non care home} %<br>
      <b>Proportion Difference:</b> {point.DIFF} %<br>
      <b>Proportion Greater in:</b> {point.GREATER}"
  ) %>% 
  hc_chart(inverted=T) %>% 
  hc_annotations(
    list(
      labels = list(
        list(
          point = list(x = 350, y = 180),
          text = "Greater proportion of <br>items in non-care homes"
        ),
        list(
          point = list(x = 350, y = 530),
          text = "Greater proportion of <br>items in care homes"
        )
      )
    )
  )

# Difference and Volume scatter

# Points for straight line
points = data.frame(
  x = c(0, 26),
  y = c(0,26)
)

# Generate difference groups
drug_diff = drug %>% 
  mutate(rnk = abs(care_home_percentage - non_care_home_percentage)) %>% 
  mutate(`Difference Type` = case_when(
    rnk <= 1 ~ "small",
    rnk <= 5 ~ "Medium",
    T ~ "Large"
    ))

# Scattter with size and colour
highchart() %>% 
  hc_add_series(drug_diff %>% filter(`Difference Type` == "small"), "scatter", hcaes(care_home_percentage, non_care_home_percentage, size = 3), maxSize = "1%", dataLabels = list(enabled = TRUE, format = "{point.bnf_description}"), name = "Small prescribing difference") %>% 
  hc_add_series(drug_diff %>% filter(`Difference Type` == "Medium"), "scatter", hcaes(care_home_percentage, non_care_home_percentage, size = 3), maxSize = "3%", dataLabels = list(enabled = TRUE, format = "{point.bnf_description}"), name = "Medium prescribing difference") %>% 
  hc_add_series(drug_diff %>% filter(`Difference Type` == "Large"), "scatter", hcaes(care_home_percentage, non_care_home_percentage, size = 3), maxSize = "6%", dataLabels = list(enabled = TRUE, format = "{point.bnf_description}"), name = "Large prescribing difference") %>% 
  hc_add_series(points, "line", hcaes(x,y), showInLgend = F) %>% 
  hc_yAxis(
    min = 0,
    max = 26,
    title = list(text = "Percentage of Non Care Home Prescribing")
    ) %>% 
  hc_yAxis(
    min = 0,
    max = 26,
    title = list(text = "Percentage of Care Home Prescribing")
  ) %>% 
  hc_chart(zoomType = "xy")

# Scatter with color
highchart() %>% 
  hc_add_series(drug_diff %>% filter(`Difference Type` == "small"), "scatter", hcaes(care_home_percentage, non_care_home_percentage), maxSize = "1%", dataLabels = list(enabled = TRUE, format = "{point.bnf_description}"), name = "Small prescribing difference") %>% 
  hc_add_series(drug_diff %>% filter(`Difference Type` == "Medium"), "scatter", hcaes(care_home_percentage, non_care_home_percentage), maxSize = "3%", dataLabels = list(enabled = TRUE, format = "{point.bnf_description}"), name = "Medium prescribing difference") %>% 
  hc_add_series(drug_diff %>% filter(`Difference Type` == "Large"), "scatter", hcaes(care_home_percentage, non_care_home_percentage), maxSize = "6%", dataLabels = list(enabled = TRUE, format = "{point.bnf_description}"), name = "Large prescribing difference") %>% 
  hc_xAxis(
    min = 0,
    max = 26,
    title = list(text = "Percentage of Non Care Home Prescribing")
  ) %>% 
  hc_yAxis(
    min = 0,
    max = 26,
    title = list(text = "Percentage of Care Home Prescribing")
  ) %>% 
  hc_chart(zoomType = "xy")

# Scatter plain (no size or colour)
drug_diff %>% 
  filter(!is.na(care_home_percentage)) %>% 
  hchart(., "scatter", hcaes(care_home_percentage, non_care_home_percentage), dataLabels = list(enabled = TRUE, format = "{point.bnf_description}")) %>% 
  hc_add_series(points, "line", hcaes(x,y), showInLgend = F, color = "orange") %>% 
  hc_xAxis(
    min = 0,
    max = 26,
    title = list(text = "Care Home", size = 20)
  ) %>% 
  hc_yAxis(
    min = 0,
    max = 26,
    title = list(text = "Non Care Home", size = 20)
  ) %>% 
  hc_chart(zoomType = "xy")

# 'Simple' barchart
fact_db %>% 
  filter(
    CH_FLAG == "Care home",
    CHAPTER_DESCR == "Appliances"
    ) %>% 
  group_by(PCD_LAD_NAME) %>% 
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  collect() %>% 
  arrange(ITEMS) %>% 
  hchart(., "area", hcaes(PCD_LAD_NAME, ITEMS))

# Fact table
fact_db <- con %>%
  tbl(from = in_schema("DALL_REF", "INT615_ITEM_LEVEL_BASE"))

# Most Recent base data
form_db <- con %>%
  tbl(from = "INT646_BASE_20210401_20220331")

# Join and process tables then collect aggregated data
scatter_one <- fact_db %>%
  filter(CHAPTER_DESCR == 'Appliances') %>% 
  group_by(YEAR_MONTH, CHEMICAL_SUBSTANCE_BNF_DESCR, CH_FLAG) %>%
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  group_by(YEAR_MONTH, CH_FLAG) %>% 
  mutate(PROP = ITEMS / sum(ITEMS)) %>% 
  ungroup() %>% 
  select(YEAR_MONTH, CHEMICAL_SUBSTANCE_BNF_DESCR, CH_FLAG, PROP) %>% 
  collect()

# Additional scatter data
scatter_two = form_db %>%
  filter(CHAPTER_DESCR == 'Appliances') %>% 
  group_by(YEAR_MONTH, CHEMICAL_SUBSTANCE_BNF_DESCR, CH_FLAG) %>%
  summarise(ITEMS = sum(ITEM_COUNT)) %>% 
  ungroup() %>% 
  group_by(YEAR_MONTH, CH_FLAG) %>% 
  mutate(PROP = ITEMS / sum(ITEMS)) %>% 
  ungroup() %>% 
  select(YEAR_MONTH, CHEMICAL_SUBSTANCE_BNF_DESCR, CH_FLAG, PROP) %>% 
  collect()

# Bind data
scatter = scatter_two %>% 
  mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non-care home")) %>% 
  rbind(scatter_one) %>% 
  pivot_wider(names_from = "CH_FLAG", values_from = "PROP") %>% 
  clean_names() %>% 
  arrange(year_month, chemical_substance_bnf_descr)

# List of frames
ds = scatter %>% 
  group_by(chemical_substance_bnf_descr) %>% 
  do(item = list(
    sequence = .$year_month,
    x = .$care_home,
    y = .$non_care_home
  )) %>% 
  .$item

# Max scale values
max_value <- scatter %>%
  select(year_month, care_home, non_care_home) %>% 
  pivot_longer(!year_month) %>% 
  dplyr::summarise(max(value)) %>%
  dplyr::pull()

# Year vector
year_vec = scatter %>% 
  dplyr::select(year_month) %>% 
  dplyr::distinct() %>% 
  arrange(year_month) %>% 
  dplyr::pull()

# Initial frame
plot_start = scatter %>%
  dplyr::filter(year_month == min(year_month))

# Other frames
plot_seq = scatter %>%
  dplyr::group_by(chemical_substance_bnf_descr) %>%
  dplyr::do(sequence = highcharter::list_parse(select(., x = care_home, y = non_care_home)))

# Join prior to plot
plot_total = dplyr::left_join(plot_start, plot_seq)

# Final chart
highcharter::hchart(
  plot_total, 
  "scatter", 
  hcaes(care_home, non_care_home, group = chemical_substance_bnf_descr), 
  color = "#003087", 
  showInLegend = F, 
  dataLabels = list(enabled = TRUE, format = "{point.chemical_substance_bnf_descr}")
  ) %>% 
  highcharter::hc_motion(
    enabled = T,
    labels = year_vec,
    series = c(0:15)
  ) %>% 
  highcharter::hc_xAxis(max = max_value) %>% 
  highcharter::hc_yAxis(max = max_value) %>% 
  highcharter::hc_yAxis(
    gridLineWidth = 0,
    lineWidth = 1,
    max = ceiling(max_value / 5) * 5
    ) %>% 
  highcharter::hc_tooltip(
    useHTML = TRUE,
    shared = FALSE,
    headerFormat = ""
    #pointFormat = "<b>IMD rank</b>: {point.x} <br> <b>Local authority:</b> {point.area_name} <br> <b>Take-up (%):<b> {point.value:.1f} %"
  ) %>%
  highcharter::hc_colors(colors = c("#003087")) %>%
  highcharter::hc_chart(marginBottom = 125) %>% 
  highcharter::hc_credits(enabled = TRUE) %>% 
  hc_chart(zoomType = "xy") %>% 
  highcharter::hc_plotOptions(series = list(marker = list(radius = 3),
                                            stickyTracking = FALSE,
                                            events = list(
                                              mouseOver = htmlwidgets::JS("function() { if(this.options.color !== 'orange') {this.update({color: 'orange'})} }"),
                                              mouseOut = htmlwidgets::JS("function() { if(this.options.color === 'orange') {this.update({color: '#003087'})} }")
                                            ),
                                            states = list(
                                              hover = list(
                                                enabled = TRUE,
                                                lineWidth = 10
                                              )
                                            )))

# Mod 8: more ideas ------------------------------------------------------------

# List for plots
charts = list()

# Chart 1
charts[[1]] = hchart(bar_one %>% arrange(FY2021), "spline", hcaes(PCD_LAD_NAME, FY2021)) %>% 
  hc_xAxis(type = 'category',
           categories = rep("", 308),
           plotLines = list(
             list(
               label = list(text = "<b>Rank: Blaby 232</b>"),
               color = "orange",
               value = 233,
               width = 5
             )
           )) %>% 
  hc_yAxis(max = 4.5)

# Chart 2
charts[[2]] = hchart(bar_one %>% arrange(FY2022), "spline", hcaes(PCD_LAD_NAME, FY2022)) %>% 
  hc_xAxis(type = 'category',
           categories = rep("", 308),
           plotLines = list(
             list(
               label = list(text = "<b>Rank: Blaby 266</b>"),
               color = "orange",
               value = 266,
               width = 5
             )
           )) %>% 
  hc_yAxis(max = 4.5)

# Chart 3
charts[[3]] = hchart(bar_one %>% arrange(FY2023), "spline", hcaes(PCD_LAD_NAME, FY2023)) %>% 
  hc_xAxis(type = 'category',
           categories = rep("", 308),
           plotLines = list(
             list(
               label = list(text = "<b>Rank: Blaby 248</b>"),
               color = "orange",
               value = 248,
               width = 5
             )
           )) %>% 
  hc_yAxis(max = 4.5)

# Plot as grid
if (interactive()) {
  hw_grid(charts, rowheight = 240, ncol = 1)
}

# Colours for reactable table
get_colors = function(vars){
  
  vec = bar_one %>% 
    select({{vars}}) %>% 
    pull()
  
  normalized <- (vec - min(vec)) / (max(vec) - min(vec))
  
  color_ramp <-  rgb(colorRamp(c("white", "#ff9f1a"))(normalized), maxColorValue = 255)
}

# Generate colours
a = get_colors(FY2021)
b = get_colors(FY2022)
c = get_colors(FY2023)

# Reactable table
reactable(
  bar_one %>% select(PCD_LAD_NAME, FY2021, FY2022, FY2023) %>% rename(`Local Authority` = PCD_LAD_NAME),
  filterable = T,
  pagination = F,
  selection = "single",
  columns = list(
    
    FY2021 = colDef(
      style = JS("function(rowInfo, column, state) {
        const { showColors, fy1_colors } = state.meta
        if (showColors) {
          return { backgroundColor: fy1_colors[rowInfo.index] }
        }
      }")
    ),
    
    FY2022 = colDef(
      style = JS("function(rowInfo, column, state) {
        const { showColors, fy2_colors } = state.meta
        if (showColors) {
          return { backgroundColor: fy2_colors[rowInfo.index] }
        }
      }")
    ),
    
    FY2023 = colDef(
      style = JS("function(rowInfo, column, state) {
        const { showColors, fy3_colors } = state.meta
        if (showColors) {
          return { backgroundColor: fy3_colors[rowInfo.index] }
        }
      }")
    )
  ),
  meta = list(
    fy1_colors = a,
    fy2_colors = b,
    fy3_colors = c,
    showColors = TRUE
  )
)

# Remove and clean
DBI::dbDisconnect(con); rm(list = ls()); gc()