#' mod 01 headline figures function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_patients_imd_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2(
      "... IMD ..."
    ),
    # Chart One
    
    nhs_card(
      heading = "Annual and monthly total care home items prescribed, total cost of care home prescribing and distinct patients that received care home prescribing",
      
      # Metric select input
      nhs_selectInput(
        inputId = ns("financial_year"),
        label = "Financial Year",
        choices = c("Financial Year" = "Financial Year"),
        full_width = FALSE
      ),
      
      # Require unequal column widths
      fluidRow(
        
        # Chart
        column(
          12,
          highcharter::highchartOutput(
            outputId = ns("patients_imd_chart"),
            height = "350px"
          )
        )
      ),
      
      # Chart caption
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "... IMD ..."
      ),
      
      # Data download option
      mod_nhs_download_ui(
        id = ns("download_patients_imd_chart")
      )
    )
  )
}

#' 02_demographics Server Functions
#'
#' @noRd
mod_03_patients_imd_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter the FY
    imd_df <- reactive({
      
      # Ensure select input required
      req(input$financial_year)
      
      # Select and rename chosen column
      carehomes2::mod_patients_imd_df %>% 
        dplyr::filter(`Financial Year` = input$financial_year)
    })
    
    # Annual Chart
    output$patients_imd_chart <- highcharter::renderHighchart({
      
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
    
    # Add a download button
    mod_nhs_download_server(
      id = "download_patients_imd_chart",
      filename = "patient_imd.csv",
      export_data = carehomes2::mod_patient_imd_df
    )
  })
}

## To be copied in the UI
# mod_02_demographics_ui("02_demographics_ui_1")

## To be copied in the server
# mod_02_demographics_server("02_demographics_ui_1")