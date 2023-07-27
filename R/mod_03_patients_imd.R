#' mod 03 patients imd function
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
    includeMarkdown(app_sys("app", "www", "markdown", "03_patients_imd.rmd")),
    nhs_card(
      heading = "Deprivation decile of care home patients aged 65 years and over in England",
      
      # Metric select input
      nhs_selectInput(
        inputId = ns("financial_year"),
        label = "Financial Year",
        choices = unique(carehomes2::mod_patients_by_imd_df$`Financial Year`),
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
        paste0(
          "IMD deciles were attributed to care home address based on their postcode. ",
          "Every distinct patient that recieved a prescription form from a care homw off given IMD decile contributed to their total. ",
          "If patients moved between care homes they could potentially be counted in multiple IMD decile total distinct patient counts. ",
          "Only 10 prescription forms across the three financial years could not be attributed an IMD Decile."
        )
      ),
      
      # Data download option
      mod_nhs_download_ui(
        id = ns("download_patients_imd_chart")
      )
    )
  )
}

#' mod 03 patienst imd Server Functions
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
      carehomes2::mod_patients_by_imd_df %>% 
        dplyr::filter(`Financial Year` == input$financial_year)
    })
    
    # Annual Chart
    output$patients_imd_chart <- highcharter::renderHighchart({
      
      nhsbsaR::chart_hc_imd_bar(
        df = imd_df(),
        imd_col = "IMD Decile",
        value_col = "Number of Patients",
        metric_def = "Number of Patients",
        highlight_core20 = TRUE,
        value_dp = 0
      )
    })
    
    # Add a download button
    mod_nhs_download_server(
      id = "download_patients_imd_chart",
      filename = "patient_imd.csv",
      export_data = carehomes2::mod_patients_by_imd_df
    )
  })
}

## To be copied in the UI
# mod_03_patients_imd_ui("03_patients_imd_ui_1")

## To be copied in the server
# mod_03_patients_imd_server("03_patients_imd_ui_1")