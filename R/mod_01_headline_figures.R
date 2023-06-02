#' mod 01 headline figures function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_headline_figures_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2(
      "Demographic estimates for care home patients aged 65 years or over ",
      "receiving prescriptions"
    ),
    # Chart One
    
    nhs_card(
      heading = "Annual and monthly total care home items prescribed, total cost of care home prescribing and distinct patients that received care home prescribing",

      # Metric select input
      nhs_selectInput(
        inputId = ns("metric"),
        label = "Metric",
        choices = c(
          "Distinct Patients" = "PATS",
          "Total Items" = "ITEMS",
          "Total Cost (£)" = "NIC"
        ),
        full_width = FALSE
      ),
      
      # Require unequal column widths
      fluidRow(
        
        # First chart
        column(
          4,
          highcharter::highchartOutput(
            outputId = ns("headline_annual_chart"),
            height = "350px"
            )
          ),
        
        # Second chart       
        column(
          8,
          highcharter::highchartOutput(
            outputId = ns("headline_monthly_chart"),
            height = "350px"
            )
          )
        ),
      
      # Chart caption
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Distinct patient counts are rounded to the nearest 100, while total items and total cost (£) are rounded to the nearest 1,000."
      ),
      
      # Data download option
      mod_nhs_download_ui(
        id = ns("download_headline_chart")
      )
    )
  )
}

#' 02_demographics Server Functions
#'
#' @noRd
mod_01_headline_figures_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter the data based on the breakdown
    headline_figures_df <- reactive({
      
      # Ensure select input required
      req(input$metric)
      
      # Select and rename chosen column
      carehomes2::mod_headline_figures_df %>%
        dplyr::select('TYPE', 'TIME', input$metric) %>% 
        dplyr::rename_at(input$metric, ~"METRIC")
    })
    
    # Annual Chart
    output$headline_annual_chart <- highcharter::renderHighchart({
      headline_figures_df() %>% 
        dplyr::filter(TYPE == "ANNUAL") %>% 
        highcharter::hchart(., "column", highcharter::hcaes(TIME, METRIC, color = nhsbsaR::palette_nhsbsa()[1])) %>% 
        highcharter::hc_xAxis(title = list(text = "<b> Financial Year</b>")) %>%  
        highcharter::hc_yAxis(
          min = 0,
          title = list(
            text = paste(
              switch(
                input$metric,
                "PATS" = "<b>Annual Distinct Patients</b>",
                "ITEMS" = "<b>Annual Total Items</b>",
                "NIC" = "<b>Annual Total Cost (£)</b>"
              )
            )
          )
        ) %>% 
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Year: </b> {point.TIME}<br>",
            switch(
              input$metric,
              "PATS" = "<b>Distinct Patients: </b> {point.METRIC:,.0f}",
              "ITEMS" = "<b>Total Items: </b> {point.METRIC:,.0f}",
              "NIC" = "<b>Total Cost: </b> £{point.METRIC:,.0f}"
            )
          )
        )
    })
    
    # Monthly Chart
    output$headline_monthly_chart <- highcharter::renderHighchart({
      headline_figures_df() %>% 
        dplyr::filter(TYPE == "MONTHLY") %>% 
        highcharter::hchart(., "line", highcharter::hcaes(TIME, METRIC, color = nhsbsaR::palette_nhsbsa()[1])) %>% 
        highcharter::hc_xAxis(title = list(text = "<b>Month</b>")) %>% 
        highcharter::hc_yAxis(
          min = 0,
          title = list(
            text = paste(
              switch(
                input$metric,
                "PATS" = "<b>Monthly Distinct Patients</b>",
                "ITEMS" = "<b>Monthly Total Items</b>",
                "NIC" = "<b>Monthly Total Cost (£)</b>"
                )
              )
            )
          ) %>% 
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Month: </b> {point.TIME}<br>",
            switch(
              input$metric,
              "PATS" = "<b>Distinct Patients: </b> {point.METRIC:,.0f}",
              "ITEMS" = "<b>Total Items: </b> {point.METRIC:,.0f}",
              "NIC" = "<b>Total Cost: </b> £{point.METRIC:,.0f}"
              )
            )
          )
    })
    
    # Add a download button
    mod_nhs_download_server(
      id = "download_headline_chart",
      filename = "headline_chart.csv",
      export_data = carehomes2::mod_headline_figures_df %>% 
        dplyr::rename(
          `Time Period` = TIME,
          `Distinct Patients` = PATS,
          `Total Items` = ITEMS,
          `Total Cost (Pounds)` = NIC,
          `Metric Type` = TYPE
        )
    )
  })
}

## To be copied in the UI
# mod_02_demographics_ui("02_demographics_ui_1")

## To be copied in the server
# mod_02_demographics_server("02_demographics_ui_1")