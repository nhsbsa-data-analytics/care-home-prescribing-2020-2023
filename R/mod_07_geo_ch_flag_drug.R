
#' mod 07 geographic care home drug analysis
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_07_geo_ch_flag_drug_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2(
      "Demographic estimates for care home patients aged 65 years or over ",
      "receiving prescriptions"
    ),
    
    # Overall nhs card
    nhs_card(
      
      # Overall Mod heading
      heading = "... ABC ...",
      
      # 4 Tabs for differing geographies
      tabsetPanel(
        type = "tabs",
        
        # Tab 1: Region
        tabPanel(
          title = "Region Comparison",
          br(),
          h4_tabstop("... XYZ ..."),
          
          # 3 select-inputs per tab
          nhs_grid_3_col(
            
            # Input 1: Metric
            nhs_selectInput(
              inputId = ns("input_region_metric"),
              label = "Metric",
              choices = c(
                "Proportion of Items" = "PROP_ITEMS",
                "Proportion of Cost" = "PROP_NIC",
                "Items PPM" = "PPM_ITEMS",
                "Cost PPM" = "PPM_COST"
              ),
              full_width = TRUE
            ),
            
            # Input 2: BNF Parent
            nhs_selectInput(
              inputId = ns("input_region_bnf_parent"),
              label = "BNF Level",
              choices = c(
                "Chapter" = "CHAPTER_DESCR",
                "Section" = "SECTION_DESCR",
                "Paragraph" = "PARAGRAPH_DESCR",
                "Chemical Substance" = "CHEMICAL_SUBSTANCE_BNF_DESCR"
                ),
              full_width = TRUE
              ),
            
            # Input 3: BNF Child
            nhs_selectInput(
              inputId = ns("input_region_bnf_child"),
              label = "BNF Type",
              choices = NULL,
              full_width = TRUE
            )
              
        ),
        
        # First column with 1 long table
        fluidPage(
          # LHS: Single table
          column(
            7,
            reactable::reactableOutput(
              outputId = ns("region_table"),
              height = "600px"
              )
            ),
          # RHS: 3 charts
          column(
            5,
            highcharter::highchartOutput(
              outputId = ns("region_chart_one"), 
              height = "200px"
            ),
            highcharter::highchartOutput(
              outputId = ns("region_chart_two"), 
              height = "200px"
            ),
            highcharter::highchartOutput(
              outputId = ns("region_chart_three"), 
              height = "200px"
            )
            )
          )
        )
      )
    )
  )
}



library(highcharter)
#' 02_demographics Server Functions
#'
#' @noRd
mod_07_geo_ch_flag_drug_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # The mod df if filtered by select input 1
    input_region_filter = reactive({
      carehomes2::mod_geo_ch_flag_drug_df %>% 
        dplyr::filter(
          GEOGRAPHY_PARENT == "PCD_REGION_NAME",
          BNF_PARENT == input$input_region_bnf_parent,
          METRIC == input$input_region_metric
        )
    })
    
    # Observe the filtered mod df
    observeEvent(input_region_filter(), {
      choices = unique(input_region_filter()$BNF_CHILD)
      updateSelectInput(inputId = "input_region_bnf_child", choices = choices)
    })
    
    # Parent df to create 3 spline charts
    region_df = reactive({
      
      # Ensure select input required
      req(input$input_region_bnf_child)
      
      # Filter by selected bnf child and arrange values
      input_region_filter() %>% 
        dplyr::filter(BNF_CHILD == input$input_region_bnf_child) %>% 
        dplyr::arrange(VALUE)
    })
    
    # LHS: Table ---------------------------------------------------------------
    
    # Pivot table wide for table presentation
    region_table_wide = reactive({
      
      region_df() %>% 
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>% 
        dplyr::select(Region = GEOGRAPHY_CHILD, `2020/21`, `2021/22`, `2022/23`)
    })
      
    # Render table with FYs pivoted to columns
    output$region_table = reactable::renderReactable({
      
      reactable::reactable(
        region_table_wide(),
        selection = "single",
        onClick = "select",
        defaultSelected = 1,
        searchable = TRUE,
        pagination = FALSE,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        theme = reactable::reactableTheme(
          borderColor = "#dfe2e5",
          stripedColor = "#f6f8fa",
          highlightColor = "#f0f5f9",
          searchInputStyle = list(width = "100%"),
          style = list(fontFamily = "Arial")
        ),
        columns = list(
          `2020/21` = reactable::colDef(maxWidth = 75),
          `2021/22` = reactable::colDef(maxWidth = 75),
          `2022/23` = reactable::colDef(maxWidth = 75)
        )
        
      )
    })
    
    # Get reactive row index from row click
    region_table_click = reactive(
      reactable::getReactableState("region_table", "selected")
      )
    

    # Chart 1: FY 2020/21
    observeEvent(region_table_click(),{
      
      # Render table with FYs pivoted to columns
      output$region_table = reactable::renderReactable({
        
        # Ensure select input required
        req(region_table_click())
        
        # Reactable table
        reactable::reactable(
          region_table_wide(),
          selection = "single",
          onClick = "select",
          defaultSelected = region_table_click(),
          searchable = TRUE,
          pagination = FALSE,
          striped = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          theme = reactable::reactableTheme(
            borderColor = "#dfe2e5",
            stripedColor = "#f6f8fa",
            highlightColor = "#f0f5f9",
            searchInputStyle = list(width = "100%"),
            style = list(fontFamily = "Arial")
          ),
          columns = list(
            `2020/21` = reactable::colDef(maxWidth = 75),
            `2021/22` = reactable::colDef(maxWidth = 75),
            `2022/23` = reactable::colDef(maxWidth = 75)
          )
          
        )
      })
    })
    
    # details = function(index, name){
    #   request_id <- data()[index, "Request_ID"]
    #   htmltools::div(
    #     reactable(random_samples[random_samples$Request_ID == request_id, ])
    #   )
    # }
    
    # RHS: 3 charts ------------------------------------------------------------
    
    # Base table for spline charts
    region_spline = reactive({
      
      region_df() %>%
        dplyr::filter(FY == "2020/21") %>% 
        dplyr::arrange(VALUE) %>% 
        dplyr::mutate(index = dplyr::row_number()) %>%
        dplyr::arrange(desc(VALUE)) %>% 
        dplyr::mutate(rank = dplyr::row_number()) %>% 
        dplyr::mutate(col =  "#f7a35c") %>% 
        dplyr::mutate(
          label = dplyr::case_when(
            rank == 1 ~ as.character(VALUE),
            index == 1 ~ as.character(VALUE),
            rank == region_table_click() ~ as.character(VALUE),
            TRUE ~ ""
          )
        )
      })
    
    # Single point series data
    region_point = reactive({
      
      region_spline() %>% 
        dplyr::filter(dplyr::row_number() == region_table_click())
      })
    
    # Text for spline chart titles
    region_chart_one_text = reactive({
      
      paste0(
        "<b>Ranked ",
        region_point()$rank,
        " / ",
        nrow(region_spline()),
        " in ",
        region_point()$FY
      )
    })
    
    # Chart 1: FY 2020/21
    observeEvent(region_table_click(),{
      
      # Ensure select input required
      req(region_table_click())
      
      # Chart 1: FY 2020/21
      output$region_chart_one = highcharter::renderHighchart({
        
        highcharter::highchart() %>%
          highcharter::hc_add_series(
            region_spline(), 
            "spline", 
            highcharter::hcaes(index, VALUE), 
            showInLegend = FALSE,
            dataLabels = list(
              enabled = TRUE,
              format = "{point.label}",
              style = list(fontSize = "17px", fontFamily = "arial")
            )
          ) %>% 
          highcharter::hc_add_series(
            region_point(),
            "scatter",
            highcharter::hcaes(index, VALUE, color = col),
            showInLegend = FALSE
          ) %>% 
          highcharter::hc_yAxis(min = 0) %>%
          highcharter::hc_xAxis(categories = rep("", nrow(region_spline())+1)) %>%
          highcharter::hc_plotOptions(
            spline = list(
              marker = list(
                enabled = FALSE
              ),
              states = list(
                inactive = list(opacity = 1)
              )
            ),
            scatter = list(
              marker = list(
                radius = 5,
                symbol = "circle"
              ),
              states = list(
                inactive = list(opacity = 1)
              )
            ),
            series = list(
              states = list(
                hover = list(
                  enabled = FALSE
                ),
                select = list(
                  enabled = FALSE
                )
              )
            )
          ) %>%
          highcharter::hc_add_theme(hc_theme_null()) %>%
          highcharter::hc_tooltip(enabled = FALSE) %>%
          hc_title(
            text = region_chart_one_text(),
            style = list(
              textAlign = "center",
              fontSize = "17px",
              fontFamily = "arial"
            )
          )
      })
      
    })

    
    
    # Chart 2: FY 2020/21
    output$region_chart_two = highcharter::renderHighchart({
      region_df() %>% 
        dplyr::filter(FY == "2021/22") %>% 
        highcharter::hchart(., "spline", highcharter::hcaes(GEOGRAPHY_CHILD, VALUE)) %>% 
        highcharter::hc_xAxis(categories = rep("", nrow(region_df()))) %>% 
        highcharter::hc_yAxis(min = 0)
    })
    
    # Chart 3: FY 2020/21
    output$region_chart_three = highcharter::renderHighchart({
      region_df() %>% 
        dplyr::filter(FY == "2022/23") %>% 
        highcharter::hchart(., "spline", highcharter::hcaes(GEOGRAPHY_CHILD, VALUE)) %>% 
        highcharter::hc_xAxis(categories = rep("", nrow(region_df()))) %>% 
        highcharter::hc_yAxis(min = 0)
    })
    
  })
}



# reactable::reactable(
#   df_two %>% dplyr::inner_join(df_bar),
#   selection = "single",
#   onClick = "select",
#   searchable = TRUE,
#   pagination = FALSE,
#   striped = TRUE,
#   highlight = TRUE,
#   bordered = TRUE,
#   theme = reactable::reactableTheme(
#     borderColor = "#dfe2e5",
#     stripedColor = "#f6f8fa",
#     highlightColor = "#f0f5f9",
#     searchInputStyle = list(width = "100%"),
#     style = list(fontFamily = "Arial")
#   ),
#   columns = list(
#     Trend = reactable::colDef(cell = function(value, index){
#       sparkline::sparkline(
#         df_bar$Trend[[index]],
#         #chartRangeMin = min_val,
#         spotRadius = 0
#       )
#     }),
#     `2020/21` = reactable::colDef(maxWidth = 75),
#     `2021/22` = reactable::colDef(maxWidth = 75),
#     `2022/23` = reactable::colDef(maxWidth = 75),
#     Trend = reactable::colDef(maxWidth = 40)
#   )
# )




point = df_two %>%
  dplyr::select(Region, `2020/21`) %>%
  dplyr::arrange(`2020/21`) %>%
  dplyr::mutate(index = dplyr::row_number()) %>%
  dplyr::arrange(desc(`2020/21`)) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::mutate(col =  "#f7a35c") %>% 
  dplyr::mutate(
    label = dplyr::case_when(
      dplyr::row_number() == 1 ~ as.character(`2020/21`),
      dplyr::row_number() == nrow(.) ~ as.character(`2020/21`),
      Region == "South East" ~ as.character(`2020/21`),
      TRUE ~ ""
    )
  )

sub_point = point %>%
  dplyr::filter(Region == "South East")





text = paste0(
  "<br><br><br><b>Ranked ",
  sub_point$rank,
  " / ",
  nrow(point),
  " ",
  names(point)[1],
  "s in ",
  names(point[2]),
  "</b>"
)

highchart() %>%
  hc_add_series(
    point2,
    "spline",
    hcaes(index, `2020/21`),
    showInLegend = FALSE,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.label}",
      style = list(fontSize = "30px", fontFamily = "arial")
      )
    ) %>%
  hc_add_series(
    sub_point,
    "scatter",
    hcaes(index, `2020/21`, color = col),
    showInLegend = FALSE
    ) %>% 
  hc_yAxis(min = 0) %>%
  hc_xAxis(categories = rep("", nrow(point)+1)) %>% 
  hc_plotOptions(
      spline = list(
        marker = list(
          enabled = FALSE
        ),
        states = list(
          inactive = list(opacity = 1)
        )
      ),
      scatter = list(
        marker = list(
          radius = 15,
          symbol = "circle"
        ),
        states = list(
          inactive = list(opacity = 1)
        )
      ),
      series = list(
        states = list(
          hover = list(
            enabled = FALSE
          ),
          select = list(
            enabled = FALSE
            )
          )
        )
      ) %>%
  hc_tooltip(enabled = FALSE) %>% 
  hc_title(
    text = text,
    style = list(
      textAlign = "center",
      fontSize = "30px",
      fontFamily = "arial"
    )
  )




  



# Colours for reactable table
# get_colors = function(df){
#   
#   # Vector of metric values
#   vec = df %>% dplyr::select(VALUE) %>% dplyr::pull()
#   
#   # Normalised values
#   normalized = (vec - min(vec)) / (max(vec) - min(vec))
#   
#   # Colour scale
#   color_ramp <-  rgb(colorRamp(
#     c("white", nhsbsaR::palette_nhsbsa()[1]))(normalized), maxColorValue=255
#   )
# }

## To be copied in the UI
# mod_07_geo_ch_flag_drug_ui("geo_ch_flag_drug")

## To be copied in the server
# mod_07_geo_ch_flag_drug_server("geo_ch_flag_drug")
  