
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
            6,
            highcharter::highchartOutput(
              outputId = "region_table",
              height = "750px"
              )
            ),
          # RHS: 3 charts
          column(
            6,
            highcharter::highchartOutput(
              outputId = ns("region_chart_one"), 
              height = "250px"
            ),
            highcharter::highchartOutput(
              outputId = ns("region_chart_two"), 
              height = "250px"
            ),
            highcharter::highchartOutput(
              outputId = ns("region_chart_three"), 
              height = "250px"
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
    
    # Colours for reactable table
    get_colors = function(df){
      
      # Vector of metric values
      vec = df %>% select(VALUE) %>% pull()
      
      # Normalised values
      normalized = (vec - min(vec)) / (max(vec) - min(vec))
      
      # Colour scale
      color_ramp <-  rgb(colorRamp(
        c("white", nhsbsaR::palette_nhsbsa()[1]))(normalized), maxColorValue=255
        )
    }
    
    # Generate colours
    region_col = get_colors(region_df())

    
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
    
    
    # RHS: 3 charts ------------------------------------------------------------
    
    # Chart 1: FY 2020/21
    output$region_chart_one = highcharter::renderHighchart({
      region_df() %>% 
        dplyr::filter(FY == "2021/22") %>% 
        highcharter::hchart(., "spline", highcharter::hcaes(GEOGRAPHY_CHILD, VALUE))
    })
    
    # Chart 2: FY 2020/21
    output$region_chart_two = highcharter::renderHighchart({
      region_df() %>% 
        dplyr::filter(FY == "2021/22") %>% 
        highcharter::hchart(., "spline", highcharter::hcaes(GEOGRAPHY_CHILD, VALUE))
    })
    
    # Chart 3: FY 2020/21
    output$region_chart_three = highcharter::renderHighchart({
      region_df() %>% 
        dplyr::filter(FY == "2022/23") %>% 
        highcharter::hchart(., "spline", highcharter::hcaes(GEOGRAPHY_CHILD, VALUE))
    })
    
  })
}


df = carehomes2::mod_geo_ch_flag_drug_df %>% 
  dplyr::filter(
    GEOGRAPHY_PARENT == "PCD_REGION_NAME",
    BNF_PARENT == "CHAPTER_DESCR",
    METRIC == "PROP_ITEMS",
    BNF_CHILD == "Appliances"
  )

df_two = df %>% 
  tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>% 
  dplyr::select(Region = GEOGRAPHY_CHILD, `2020/21`, `2021/22`, `2022/23`)

df_bar = df %>% 
  dplyr::arrange(FY) %>% 
  dplyr::group_by(Region = GEOGRAPHY_CHILD) %>% 
  dplyr::summarise(Trend = list(VALUE))

min_val = min(df$VALUE)

reactable::reactable(
  df_two %>% dplyr::inner_join(df_bar),
  selection = "single",
  onClick = "select",
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
    Trend = reactable::colDef(cell = function(value, index){
      sparkline::sparkline(
        df_bar$Trend[[index]],
        chartRangeMin = min_val,
        spotRadius = 0
        )
    })
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