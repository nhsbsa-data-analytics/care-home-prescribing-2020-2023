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
    include_dynamic_md("inst/markdown/17_short_longstay.md"),
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
                          "% of patient-months with 2+ ACAP medicines" = "PCT_PM_ACAP",
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
      br(),
      
      # Chart caption
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "The first complete month that patient receives prescribing at a care home is deemed as month-1.",
        tags$br(),
        "Month-2 and month-3 and so on are then the successive months where a patient recieves care home prescribing.",
        tags$br(),
        "When a patient receives non-care prescribing, this counter resets to zero.",
        tags$br(),
        "The next complete month (if there is one) where patient receives prescribing is deemed as month-1 once again.",
        tags$br(),
        "As can be seen, the prescribing of the same patient can be allocated to the same month values multiple times.",
        tags$br(),
        "The prescribing of the same patient is also allocated to every monthly value, until they stop receiving care home prescribing."
      )
    )
  )
}

#' patients_age_gender Server Functions
#'
#' @noRd 
mod_17_short_longstay_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Metric and UI mappings----------------------------------------------------
    
    # Map metric column names to UI metric names
    ui_metric_names <- c(
      COST_PPM            = "Mean drug cost PPM",
      ITEMS_PPM           = "Mean prescription items PPM",
      UNIQ_MEDS_PPM       = "Mean unique medicines PPM",
      PCT_PM_GTE_SIX      = "% of patient-months with 6+ unique medicines",
      PCT_PM_GTE_TEN      = "% of patient-months with 10+ unique medicines",
      PCT_PM_ACB          = "% of patient-months with 2+ ACB medicines",
      PCT_PM_DAMN         = "% of patient-months with 2+ DAMN medicines",
      PCT_PM_ACAP         = "% of patient-months with 2+ ACAP medicines",
      UNIQ_MEDS_FALLS_PPM = "Mean unique falls risk medicines PPM",
      PCT_PM_FALLS        = "% of patient-months with 3+ falls risk medicines"
    )
    
    # Map metric column names to tooltip metric names
    # metric_tooltips <- c(
    #   COST_PPM            = "<b>Mean drug cost PPM:</b> \u00A3{point.y}",
    #   ITEMS_PPM           = "<b>Mean prescription items PPM:</b> {point.y:.1f}",
    #   UNIQ_MEDS_PPM       = "<b>Mean unique medicines PPM:</b> {point.y:.1f}",
    #   PCT_PM_GTE_SIX      = "<b>% of patient-months with 6+ unique medicines:</b> {point.y:.1f}%",
    #   PCT_PM_GTE_TEN      = "<b>% of patient-months with 10+ unique medicines:</b> {point.y:.1f}%",
    #   PCT_PM_ACB          = "<b>% of patient-months with 2+ ACB medicines:</b> {point.y:.1f}%",
    #   PCT_PM_DAMN         = "<b>% of patient-months with 2+ DAMN medicines:</b> {point.y:.1f}%",
    #   PCT_PM_ACAP         = "<b>% of patient-months with 2+ ACAP medicines:</b> {point.y:.1f}%",
    #   UNIQ_MEDS_FALLS_PPM = "<b>Mean unique falls risk medicines PPM</b> {point.y:.1f}",
    #   PCT_PM_FALLS        = "<b>% of patient-months with 3+ falls risk medicines</b> {point.y:.1f}%"
    # )
    
    # Map all column names to download data names
    dl_col_names <- c(
      rlang::set_names(names(ui_metric_names), unname(ui_metric_names)),
      "Care home length of stay"    = "SEQ_GROUP",
      "Geography"                   = "GEO_TYPE",
      "Sub-geography"               = "GEO"
    )
    
    # Reactive processing ------------------------------------------------------
    
    # Filter the data based on the FY and the geography
    initial_filter_df <- reactive({
      req(input$geography)
      
      mod_short_longstay_df %>%
        dplyr::filter(
          METRIC == input$metric,
          GEO_TYPE == input$geography
        )
    })
    
    # Generate max value in context of metric & parent-geography
    max_val = reactive({
      initial_filter_df() %>% 
        dplyr::summarise(VALUE = max(VALUE)) %>% 
        dplyr::pull()
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
            unique() %>% 
            sort()
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
    
    # Chart --------------------------------------------------------------------
    
    # Pyramid plot for age band and gender
    output$short_longstay_chart <-
      highcharter::renderHighchart({
        req(input$metric)
        req(input$geography)
        req(input$sub_geography)
        
        # Create the chart
        highcharter::hchart(
          second_filter_df(), 
          "column", 
          highcharter::hcaes(SEQ_GROUP, VALUE, group = CH_TYPE)
          ) %>% 
          highcharter::hc_yAxis(
            title = list(text = ui_metric_names[[input$metric]]),
            min = 0,
            max = max_val()
            ) %>% 
          highcharter::hc_xAxis(
            title = list(text = "Length of care home stay")
            ) %>% 
          nhsbsaR::theme_nhsbsa_highchart(stack = "") %>% 
          hc_colors(c("#003087", "#0072CE", "#00A9CE")) %>% 
          highcharter::hc_tooltip(
            shared = T,
            useHTML = T,
            valueDecimals = switch(input$metric,
                                   "COST_PPM" = 0,
                                   "ITEMS_PPM" = 1,
                                   "UNIQ_MEDS_PPM" = 1,
                                   "PCT_PM_GTE_SIX" = 1,
                                   "PCT_PM_GTE_TEN" = 1,
                                   "PCT_PM_ACB" = 1,
                                   "PCT_PM_DAMN" = 1,
                                   "PCT_ACAP_TWO" = 1,
                                   "UNIQ_MEDS_FALLS_PPM" = 1,
                                   "PCT_PM_FALLS" = 1),
            valueSuffix = switch(input$metric,
                                 "PCT_PM_GTE_SIX" = "%",
                                 "PCT_PM_GTE_TEN" = "%",
                                 "PCT_PM_ACB" = "%",
                                 "PCT_PM_DAMN" = "%",
                                 "PCT_ACAP_TWO" = "%",
                                 "PCT_PM_FALLS" = "%"),
            valuePrefix = switch(input$metric,
                                 "COST_PPM" = "£")
          )

      })
    
    # Download -----------------------------------------------------------------
    
    # Create download data
    create_download_data <- function(data) {
      data %>%
        tidyr::pivot_wider(
          names_from = METRIC,             
          values_from = where(is.numeric)  
        ) %>% 
        dplyr::rename(dl_col_names)
    }
    
    # Pivot wide to create download data
    mod_nhs_download_server(
      id = "download_data",
      filename = "Selected prescribing metrics by care home length of stay.xlsx",
      export_data = create_download_data(mod_short_longstay_df)
      )
    
    # Observe download
    observeEvent(
      mod_short_longstay_df,
      once = TRUE, {
        req(mod_short_longstay_df)

        insertUI(
          selector = ".nhsuk-card__description:eq(5)",
          where = "beforeEnd",
          ui = mod_nhs_download_ui(ns("download_data"))
        )
      })
    
  })
}

## To be copied in the UI
#mod_17_short_longstay_ui("short_longstay")

## To be copied in the server
#mod_17_short_longstay_server("short_longstay")