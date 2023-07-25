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
      "Demographic estimates for care home patients aged 65 years and over receiving prescriptions"
    ),
    # Chart One
    
    nhs_card(
      heading = "Annual and monthly totals of patient numbers, prescription 
                 items and drug cost for care home patients aged 65 years and
                 over in England",

      # Metric select input
      nhs_selectInput(
        inputId = ns("metric"),
        label = "Metric",
        choices = c(
          "Total patient count" = "PATS",
          "Total prescription items" = "ITEMS",
          "Total drug cost (£)" = "NIC"
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
            height = "317px"
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
        "Distinct patient counts are rounded to the nearest 100, total prescription items are rounded to the nearest 1,000 and total drug cost (£) is rounded to the nearest 10,000."
      ),
      
      # Data download option
      mod_nhs_download_ui(
        id = ns("download_data")
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
        highcharter::hc_xAxis(title = list(text = "<b> Financial year</b>")) %>%  
        highcharter::hc_yAxis(
          min = 0,
          title = list(
            text = paste(
              switch(
                input$metric,
                "PATS" = "<b>Annual distinct patients</b>",
                "ITEMS" = "<b>Annual total items</b>",
                "NIC" = "<b>Annual total cost (£)</b>"
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
              "PATS" = "<b>Distinct patients: </b> {point.METRIC:,.0f}",
              "ITEMS" = "<b>Total items: </b> {point.METRIC:,.0f}",
              "NIC" = "<b>Total cost: </b> £{point.METRIC:,.0f}"
            )
          )
        ) %>% 
        nhsbsaR::theme_nhsbsa_highchart()
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
                "PATS" = "<b>Monthly distinct patients</b>",
                "ITEMS" = "<b>Monthly total items</b>",
                "NIC" = "<b>Monthly total cost (£)</b>"
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
              "PATS" = "<b>Distinct patients: </b> {point.METRIC:,.0f}",
              "ITEMS" = "<b>Total items: </b> {point.METRIC:,.0f}",
              "NIC" = "<b>Total cost: </b> £{point.METRIC:,.0f}"
              )
            )
          ) %>% 
        nhsbsaR::theme_nhsbsa_highchart()
    })
    
    # Create download data
    create_download_data <- function(data) {
      data %>%
        dplyr::arrange(.data$TIME) %>%
        dplyr::rename(
          `Time period` = TIME,
          `Total patient count` = PATS,
          `Total prescription items` = ITEMS,
          `Total drug cost` = NIC,
          `Metric type` = TYPE
        )
    }
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "Headline figures for care home prescribing.xlsx",
      export_data = create_download_data(carehomes2::mod_headline_figures_df)
    )
  })
}

## To be copied in the UI
# mod_02_demographics_ui("02_demographics_ui_1")

## To be copied in the server
# mod_02_demographics_server("02_demographics_ui_1")