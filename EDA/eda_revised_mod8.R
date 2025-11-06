cols = nhsbsaR::palette_nhsbsa()[1:7]

mod_geo_ch_flag_drug_df

df_col = data.frame(
  GEOGRAPHY_CHILD = unique()
)

df = mod_geo_ch_flag_drug_df %>% 
  dplyr::filter(
    GEOGRAPHY_PARENT == "Region",
    METRIC == "% of total annual number of prescription items",
    BNF_PARENT == "Chapter",
    BNF_CHILD == "Anaesthesia"
    ) %>% 
  dplyr::inner_join(df_region_col) %>% 
  dplyr::arrange(FY, GEOGRAPHY_CHILD)


highcharter::hchart(df, "line", highcharter::hcaes(FY, VALUE, group = GEOGRAPHY_CHILD))


cols[(df$GEOGRAPHY_PARENT)]                                         

pal_func <- colorRampPalette(nhsbsaR::palette_nhsbsa())

df_region_col = mod_geo_ch_flag_drug_df %>% 
  dplyr::filter(GEOGRAPHY_PARENT == "Region") %>% 
  dplyr::distinct(GEOGRAPHY_CHILD) %>% 
  dplyr::arrange(GEOGRAPHY_CHILD) %>% 
  dplyr::mutate(COL = nhsbsaR::palette_nhsbsa()[1:nrow(.)])


df_icb_col = mod_geo_ch_flag_drug_df %>% 
  dplyr::filter(GEOGRAPHY_PARENT == "ICS") %>% 
  dplyr::distinct(GEOGRAPHY_CHILD) %>% 
  dplyr::arrange(GEOGRAPHY_CHILD) %>% 
  dplyr::mutate(COL = pal_func(nrow(.)))

# region_line_total = reactive({
#   
#   df = dplyr::union_all(
#     region_line_mean_df(),
#     region_line_df()
#     ) %>% 
#     dplyr::arrange(FY, GEOGRAPHY_CHILD)
#   
#   print(df)
#   df
# })

# Region colour df
# region_col = reactive({
#   
#   # Colour per geography child
#   df = region_filter() %>% 
#     dplyr::distinct(GEOGRAPHY_CHILD) %>% 
#     dplyr::arrange(GEOGRAPHY_CHILD) %>% 
#     dplyr::mutate(COL = nhsbsaR::palette_nhsbsa()[1:nrow(.)])
#   df
# })

label_comma_integer = function (x) scales::label_comma(accuracy = 1, scale_cut = append(scales::cut_long_scale(), 1, 1))(janitor::round_half_up(x, 0))
label_comma_decimal = function (x) scales::label_comma(accuracy = 0.1, scale_cut = append(scales::cut_long_scale(), 1, 1))(janitor::round_half_up(x, 1))

label_comma_integer(0.675)
label_comma_decimal(0.675)

label_comma_integer = function (x, acc) scales::label_comma(accuracy = acc, scale_cut = append(scales::cut_long_scale(), 1, 1))(janitor::round_half_up(x, 1))
label_comma_integer(23434345.45, 1)


highcharter::highchart() %>%
  highcharter::hc_xAxis(categories = sort(unique(region_line_mean_df()$FY))) %>%
  add_geo_series(
    geo_df = region_line_mean_df(),
    col = nhsbsaR::palette_nhsbsa()[2],
    geo_name = "Region",
    geo_bnf_parent = input$input_region_bnf_parent,
    geo_bnf_child = input$input_region_bnf_child,
    geo_metric = input$input_region_metric
  ) %>%
  add_geo_series(
    geo_df = region_line_df(),
    col = nhsbsaR::palette_nhsbsa()[1],
    geo_name = "Region",
    geo_bnf_parent = input$input_region_bnf_parent,
    geo_bnf_child = input$input_region_bnf_child,
    geo_metric = input$input_region_metric
  ) %>%
  # highcharter::hc_add_series(
  #   data = region_line_mean_df() %>% dplyr::mutate(GEOGRAPHY_CHILD = "National Mean"),
  #   type = "line",
  #   highcharter::hcaes(x = FY, y = VALUE, group = GEOGRAPHY_CHILD),
  #   name = "National Mean",
  #   color = nhsbsaR::palette_nhsbsa()[2],
  #   marker = list(enabled = FALSE),
  #   tooltip = list(
  #     headerFormat = "",
  #     pointFormat = paste0(
  #       "<b>Year: </b> {point.FY}<br>",
  #       "<b>Region: </b> {point.GEOGRAPHY_CHILD}<br>",
  #       "<b>BNF ", input$input_region_bnf_parent, ": </b> ", input$input_region_bnf_child, "<br>",
  #       switch(
  #         input$input_region_metric,
  #         "% of total annual drug cost" = "<b>% of total annual drug cost: </b> {point.VALUE_FORMAT:,.1f}%",
  #         "% of total annual number of prescription items" = "<b>% of total annual number of prescription items: </b> {point.VALUE_FORMAT:,.1f}%",
  #         "Mean drug cost PPM" = "<b>Mean drug cost PPM: </b> £{point.VALUE_FORMAT:,.1f}",
  #         "Total annual drug cost" = "<b>Total annual drug cost: </b> £{point.VALUE_FORMAT:,.0f}",
  #         "Mean prescription items PPM" = "<b>Mean prescription items PPM: </b> {point.VALUE_FORMAT:,.1f}",
  #         "Total annual number of prescription items" = "<b>Total annual number of prescription items: </b> {point.VALUE_FORMAT:,.0f}"
  #       )
  #     )
  #   )
  # ) %>%
  # highcharter::hc_add_series(
  #   data = region_line_df(),
  #   type = "line",
  #   highcharter::hcaes(x = FY, y = VALUE, group = GEOGRAPHY_CHILD),
  #   color = nhsbsaR::palette_nhsbsa()[1],
  #   marker = list(enabled = FALSE),
  #   tooltip = list(
  #     headerFormat = "",
  #     pointFormat = paste0(
  #       "<b>Year: </b> {point.FY}<br>",
  #       "<b>Region: </b> {point.GEOGRAPHY_CHILD}<br>",
  #       "<b>BNF ", input$input_region_bnf_parent, ": </b> ", input$input_region_bnf_child, "<br>",
  #       switch(
  #         input$input_region_metric,
  #         "% of total annual drug cost" = "<b>% of total annual drug cost: </b> {point.VALUE_FORMAT:,.1f}%",
  #         "% of total annual number of prescription items" = "<b>% of total annual number of prescription items: </b> {point.VALUE_FORMAT:,.1f}%",
  #         "Mean drug cost PPM" = "<b>Mean drug cost PPM: </b> £{point.VALUE_FORMAT:,.1f}",
  #         "Total annual drug cost" = "<b>Total annual drug cost: </b> £{point.VALUE_FORMAT:,.1f}",
  #         "Mean prescription items PPM" = "<b>Mean prescription items PPM: </b> {point.VALUE_FORMAT:,.1f}",
  #         "Total annual number of prescription items" = "<b>Total annual number of prescription items: </b> {point.VALUE_FORMAT:,.0f}"
  #       )
  #     )
  #   )
  # ) %>%
  # highcharter::hc_title(
  #   text = "<b>Annual metric values per selected Region</b>",
  #   style = list(
  #     fontSize = "16px",
  #     fontWeight = "bold",
  #     family = "Frutiger W01"
  #     )
  #   ) %>% 
  # highcharter::hc_plotOptions(
  #   series = list(
  #     states = list(
  #       inactive = list(enabled = FALSE)
  #     )
  #   )
  # ) %>% 
  nhsbsaR::theme_nhsbsa_highchart(stack = NULL) %>%
  highcharter::hc_yAxis(
    #min = 0,
    min = region_filter()$MIN[1] * 0.95,
    max = region_filter()$MAX[1] * 1.05,
    title = list(text = input$input_region_metric)
  ) %>% 
  highcharter::hc_legend(
    layout = "proximate", 
    align = "right"
  ) 


split_middle <- function(x, max_len = 35) {
  
  sapply(x, function(str) {
    # If short, return as-is
    if (nchar(str) <= max_len) return(str)
    
    # Find spaces
    spaces <- gregexpr(" ", str)[[1]]
    if (all(spaces == -1)) return(str)  # no spaces, return original
    
    # Find space closest to middle
    mid <- nchar(str) / 2
    split_pos <- spaces[which.min(abs(spaces - mid))]
    
    # Insert line break
    paste0(substr(str, 1, split_pos - 1),
           "<br>",
           substr(str, split_pos + 1, nchar(str)))
  }, USE.NAMES = FALSE)
}

split_middle("NHS Stoke, horchester and Middleingstongworth and Eppingdaleton ICB")                                                                                                                                
