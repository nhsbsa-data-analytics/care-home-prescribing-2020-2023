#' Short longstay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_17_short_longstay_ui <- function(id){
  ns <- NS(id)

  tagList(
    #include_dynamic_md("inst/markdown/17_short_longstay.md"),
    nhs_card(
      heading = "Care home prescribing metric values depending on length of continuous care home stay",
      nhs_grid_3_col(
        nhs_selectInput(inputId = ns("metric"),
                        label = "Metric",
                        choices = c(
                          "Mean drug cost PPM" = "COST_PPM",
                          "Mean prescription items PPM" = "ITEMS_PPM",
                          "Mean unique medicines PPM" = "UNIQ_MEDS_PPM",
                          "% of patient-months with 6+ unique medicines" = "PCT_PM_GTE_SIX",
                          "% of patient-months with 10+ unique medicines" = "PCT_PM_GTE_TEN",
                          "% of patient-months with 2+ ACB medicines" = "PCT_PM_ACB",
                          "% of patient-months with 2+ DAMN medicines" = "PCT_PM_DAMN",
                          "% of patient-months with 2+ ACAP medicines" = "PCT_ACAP_DAMN",
                          "Mean unique falls risk medicines PPM" = "UNIQ_MEDS_FALLS_PPM",
                          "% of patient-months with 3+ falls risk medicines" = "PCT_PM_FALLS"
                        ),
                        selected = dplyr::first(unique(mod_short_longstay_df$METRIC)),
                        full_width = T),
        nhs_selectInput(inputId = ns("geography"),
                        label = "Geography",
                        choices = sort(unique(mod_short_longstay_df$GEO_TYPE)),
                        selected = dplyr::first(sort(unique(mod_short_longstay_df$GEO_TYPE))),
                        full_width = T),
        nhs_selectInput(inputId = ns("sub_geography"),
                        label = "Sub Geography",
                        choices = NULL, # dynamically generated
                        full_width = T)
      ),
      highcharter::highchartOutput(outputId = ns("short_longstay_chart"), height = "350px"),
      shiny::htmlOutput(outputId = ns("caption"))
    )
  )
}

#' patients_age_gender Server Functions
#'
#' @noRd 
mod_17_short_longstay_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Filter the data based on the FY and the geography
    initial_filter_df <- reactive({
      req(input$geography)
      
      mod_short_longstay_df %>%
        dplyr::filter(
          METRIC == input$metric,
          GEO_TYPE == input$geography
        )
    })
    
    # Update the list of choices for sub geography from the non NA rows in the
    # geography dataframe
    observeEvent(
      eventExpr = initial_filter_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_geography")
        updateSelectInput(
          inputId = "sub_geography",
          choices =
            initial_filter_df()$GEO %>%
            stats::na.omit() %>%
            unique()
        )
      }
    )
    
    # Filter the data based on the sub geography
    second_filter_df <- reactive({
      req(input$metric)
      req(input$geography)
      req(input$sub_geography)
      
      initial_filter_df() %>%
        dplyr::filter(
          GEO == input$sub_geography
        )
    })
    
    # Pyramid plot for age band and gender
    output$short_longstay_chart <-
      highcharter::renderHighchart({
        req(input$metric)
        req(input$geography)
        req(input$sub_geography)
        
        # Create the chart
        highcharter::hchart(second_filter_df(), "column", highcharter::hcaes(SEQ_GROUP, VALUE))

      })
    
  })
}

## To be copied in the UI
#mod_17_short_longstay_ui("short_longstay")

## To be copied in the server
#mod_17_short_longstay_server("short_longstay")