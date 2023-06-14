
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
                "% of Total Annual Number of Prescription Items" = "PROP_ITEMS",
                "% of Total Annual Drug Cost" = "PROP_NIC",
                "Number of Prescription Items (PPM)" = "PPM_ITEMS",
                "Drug Cost (PPM)" = "PPM_COST"
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
            DT::DTOutput(
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
    
    # Helper functions ---------------------------------------------------------
    
    # One: get df
    spline_chart_data = function(df, fy){
      
      df %>%
        dplyr::rename_at(fy, ~"VALUE") %>%
        dplyr::arrange(VALUE) %>%
        dplyr::mutate(
          index = dplyr::row_number(),
          rank = rev(dplyr::row_number()),
          total = max(rank),
          col =  "#f7a35c",
          label = dplyr::case_when(
            GEOGRAPHY_CHILD == selected_region() ~ paste0(fy, ":  ", VALUE),
            TRUE ~ ""
          )
        )
    }
    
    # Two: spline chart
    spline_chart_plot = function(df, top_plot = FALSE){
      
      hc = highcharter::highchart() %>%
        highcharter::hc_add_series(
          df,
          "spline",
          highcharter::hcaes(index, VALUE),
          showInLegend = FALSE,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.label}",
            style = list(fontSize = "16px", fontFamily = "arial")
          )
        ) %>% 
        highcharter::hc_add_series(
          df %>%  dplyr::filter(GEOGRAPHY_CHILD == selected_region()),
          "scatter",
          highcharter::hcaes(index, VALUE, color = col),
          showInLegend = FALSE
        ) %>%
        highcharter::hc_yAxis(min = 0, max = max(df$VALUE)) %>%
        highcharter::hc_xAxis(categories = c(rep("", nrow(df)), nrow(df))) %>%
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
            animation = FALSE,
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
        highcharter::hc_tooltip(enabled = FALSE) %>% 
        highcharter::hc_chart(borderColor = "grey", borderWidth = 0.3)
      
      # Add a title if required
      if(top_plot){
        hc = hc %>% 
          highcharter::hc_title(
            text = paste0("<b>", df$GEOGRAPHY_CHILD[1], "</b>"),
            style = list(
              textAlign = "center",
              fontSize = "18px",
              fontFamily = "arial"
            )
          )
      }
      
      hc
        
    }
    
    # Select Inputs ------------------------------------------------------------
    
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
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
        dplyr::select(
          GEOGRAPHY_CHILD,
          `20/21` = `2020/21`, 
          `21/22` = `2021/22`, 
          `22/23` = `2022/23`
          ) %>%
        dplyr::arrange(GEOGRAPHY_CHILD) 
    })
    
    # LHS: Table ---------------------------------------------------------------
    
    # Initial table
    output$region_table = DT::renderDT({
      
      DT::datatable(
        data = region_df() %>% dplyr::rename_at("GEOGRAPHY_CHILD", ~"Region"),
        escape = FALSE,
        rownames = FALSE,
        #selection = list(mode = "single"),
        selection = list(mode = "single", target = "row", selected = 1, nrow = 1, ncol = 1),
        options = list(
          scrollCollapse = TRUE,
          paging = FALSE,
          scrollY = "350px",
          overflow = "scroll"
          )
        )%>%
        DT::formatStyle(columns = 1:4, `font-size` = "14px", `width` = "6px")
    })
    
    # now we can get the row/rowname as follows:
    selected_region <- reactive({
      region_df() %>% 
        dplyr::filter(dplyr::row_number() == input$region_table_rows_selected) %>% 
        dplyr::select(GEOGRAPHY_CHILD) %>% 
        dplyr::pull()
    }) 
    
    # Chart 1: FY 2020/21
    # observeEvent(region_table_click(),{
    #   
    #   # Render table with FYs pivoted to columns
    #   output$region_table = reactable::renderReactable({
    #     
    #     # Ensure select input required
    #     req(region_table_click())
    #     
    #     # Reactable table
    #     region_df() %>% 
    #       tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>% 
    #       dplyr::select(Region = GEOGRAPHY_CHILD, `2020/21`, `2021/22`, `2022/23`) %>% 
    #       dplyr::arrange(Region) %>% 
    #       reactable::reactable(
    #         selection = "single",
    #         onClick = "select",
    #         defaultSelected = region_table_click(),
    #         searchable = TRUE,
    #         pagination = FALSE,
    #         striped = TRUE,
    #         highlight = TRUE,
    #         bordered = TRUE,
    #         theme = reactable::reactableTheme(
    #           borderColor = "#dfe2e5",
    #           stripedColor = "#f6f8fa",
    #           highlightColor = "#f0f5f9",
    #           searchInputStyle = list(width = "100%"),
    #           style = list(fontFamily = "Arial")
    #         ),
    #         columns = list(
    #           `2020/21` = reactable::colDef(maxWidth = 75),
    #           `2021/22` = reactable::colDef(maxWidth = 75),
    #           `2022/23` = reactable::colDef(maxWidth = 75)
    #       )
    #     )
    #   })
    # })
    
    # RHS: 3 charts ------------------------------------------------------------
    
    # Region spline chart data
    region_spline_one = reactive({
      spline_chart_data(region_df(), "20/21")
      })
    
    region_spline_two = reactive({
      spline_chart_data(region_df(), "21/22")
    })
    
    region_spline_three = reactive({
      spline_chart_data(region_df(), "22/23")
    })

    # Region spline chart plots
    output$region_chart_one = highcharter::renderHighchart({
      spline_chart_plot(region_spline_one(), top_plot = TRUE)
    })
    
    output$region_chart_two = highcharter::renderHighchart({
      spline_chart_plot(region_spline_two())
      
    })
    
    output$region_chart_three = highcharter::renderHighchart({
      spline_chart_plot(region_spline_three())
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




#---------------------------------

# a = carehomes2::mod_geo_ch_flag_drug_df %>% 
#   dplyr::filter(
#     GEOGRAPHY_PARENT == "PCD_REGION_NAME",
#     BNF_PARENT == "CHAPTER_DESCR",
#     METRIC == "PROP_ITEMS"
#   )
# 
#   
#   # Filter by selected bnf child and arrange values
# b = a %>% 
#     dplyr::filter(BNF_CHILD == "Appliances") %>% 
#     tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
#     dplyr::select(
#       GEOGRAPHY_CHILD,
#       `20/21` = `2020/21`, 
#       `21/22` = `2021/22`, 
#       `22/23` = `2022/23`
#     ) %>%
#     dplyr::arrange(GEOGRAPHY_CHILD)
# 
# c = b %>% 
#   dplyr::rename(VALUE = `20/21`) %>%
#   dplyr::arrange(VALUE) %>%
#   dplyr::mutate(
#     index = dplyr::row_number(),
#     rank = rev(dplyr::row_number()),
#     total = max(rank),
#     col =  "#f7a35c",
#     label = dplyr::case_when(
#       rank == 1 ~ as.character(VALUE),
#       index == 1 ~ as.character(VALUE),
#       GEOGRAPHY_CHILD == "Midlands" ~ paste0(GEOGRAPHY_CHILD, ": ", VALUE),
#       TRUE ~ ""
#     ),
#     text = paste0("Ranked ", index, " / ", rank, " in 20/21")
#   )






# library(shiny)
# library(reactable)
# 
# ui <- fluidPage(
#   reactableOutput("table")
# )
# 
# server <- function(input, output, session) {
#   # Reactive values
#   selectedRow <- reactiveVal(NULL)
#   
#   # Render the reactable component
#   output$table <- renderReactable({
#     reactable(
#       iris,
#       selection = "single",
#       defaultSorted = "Sepal.Length",
#       defaultSortOrder = "asc",
#       defaultPageSize = 10,
#       highlight = TRUE,
#       defaultSelected = selectedRow()
#     )
#   })
#   
#   # Update the selected row
#   observeEvent(input$table_selected, {
#     selectedRow(input$table_selected)
#   })
#   
#   # Update the reactable when sorting or filtering occurs
#   observeEvent(input$table_sorting, {
#     selectedRowIndex <- which(reactableData()$data$Sepal.Length == selectedRow())
#     updateReactable(session, "table", selected = selectedRowIndex)
#   })
#   
#   observeEvent(input$table_filtering, {
#     selectedRowIndex <- which(reactableData()$data$Sepal.Length == selectedRow())
#     updateReactable(session, "table", selected = selectedRowIndex)
#   })
# }
# 
# shinyApp(ui, server)

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



# server <- function(input, output,session) {
#   
#   # the reactive where we filter/sort/modify the data
#   reactive_df <- reactive({
#     mtcars[order(mtcars$cyl),]
#   })
#   
#   # This datatable uses the reactive directly, so no more modifications
#   output$table <- DT::renderDataTable({
#     DT::datatable(reactive_df())
#   })
#   
#   # now we can get the row/rowname as follows:
#   output$selectedcar <- renderText({
#     paste0(rownames(reactive_df())[input$table_rows_selected], collapse = ", ")
#   }) 
# } 


  