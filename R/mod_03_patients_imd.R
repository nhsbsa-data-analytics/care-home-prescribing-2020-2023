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
    includeMarkdown("inst/markdown/03_patients_imd.md"),
    nhs_card(
      heading = "Deprivation decile of care home patients aged 65 years and over in England",
      
      # Metric select input
      nhs_selectInput(
        inputId = ns("financial_year"),
        label = "Financial year",
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
            height = "300px"
          )
        )
      ),
      
      # Chart caption
      tags$p(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "IMD deciles were attributed to care homes based on their address. If 
           a patient moved between care homes they could potentially be double 
           counted across multiple IMD deciles. Only 6 patients across the three
           financial years could not be attributed an IMD decile. Decile patient
           counts were rounded to the nearest 10."
      ),
      
      # Data download option
      mod_nhs_download_ui(
        id = ns("download_data")
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
        dplyr::filter(`Financial Year` == input$financial_year) %>% 
        dplyr::rename(`Number of patients` = `Number of Patients`)
    })
    
    # Annual Chart
    output$patients_imd_chart <- highcharter::renderHighchart({
      
      nhsbsaR::chart_hc_imd_bar(
        df = imd_df(),
        imd_col = "IMD Decile",
        value_col = "Number of patients",
        metric_def = "Number of patients",
        highlight_core20 = TRUE,
        value_dp = 0
        ) %>%
        highcharter::hc_yAxis(max = 60000)
        
    })
    
    # Create download data
    create_download_data <- function(data) {
      data %>%
        dplyr::arrange(
          .data[["Financial Year"]],
          .data[["IMD Decile"]]
        ) %>%
        dplyr::rename(
          `Financial year` = .data[["Financial Year"]],
          `IMD decile` = .data[["IMD Decile"]],
          `Total patients` = .data[["Number of Patients"]],
          `% of patients` = .data[["Percentage of Patients"]]
        )
    }
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "IMD deciles for care home patients with prescribing.xlsx",
      export_data = create_download_data(carehomes2::mod_patients_by_imd_df),
      number_xl_fmt_str = "#,##0"
    )
  })
}

## To be copied in the UI
# mod_03_patients_imd_ui("03_patients_imd_ui_1")

## To be copied in the server
# mod_03_patients_imd_server("03_patients_imd_ui_1")