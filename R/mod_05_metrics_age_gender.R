mod_05_metrics_age_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2_tabstop("Title"),
    h3_tabstop("Subtitle"),
    p("Paragraph text…"),
    nhs_card(
      heading = "Estimated average prescribing metrics per patient month for care home and non-care home patients aged 65 years or over in England by age band and gender (2020/21)",
      nhs_grid_2_col(
        nhs_selectInput(inputId = ns("fy"),
                        label = "Financial year",
                        choices = levels(patients_by_fy_geo_age_gender_df$FY),
                        full_width = T),
        nhs_selectInput(inputId = ns("gender_and_age_band_and_ch_flag_metric"),
                        label = "Metric",
                        choices = c(
                          "Drug cost (PPM)" = "SDC_COST_PER_PATIENT_MONTH"
                          # One metric for now during development
                          #"Number of prescription items (PPM)" = "SDC_ITEMS_PER_PATIENT_MONTH",
                          #"Number of unique medicines (PPM)" = "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH",
                          #"Patients on ten or more unique medicines (PPM)" = "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH"
                          ),
                        full_width = T)
        ),
      highcharter::highchartOutput(outputId = ns("metrics_by_gender_and_age_band_and_ch_flag_chart"), height = "350px"),
      #shiny::htmlOutput(outputId = ns("pct_excluded_patients")),
      #mod_nhs_download_ui(id = ns("download_metrics_by_gender_and_age_band_and_ch_flag_chart"))
    ),
    tags$div(style = "margin-top: 25vh") # Some buffer space after the chart
    
  )
}

#' metrics_age_gender Server Functions
#'
#' @noRd 
mod_05_metrics_age_gender_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$metrics_by_gender_and_age_band_and_ch_flag_chart <-
      highcharter::renderHighchart({

    highcharter::hchart(cars, type="scatter", highcharter::hcaes(speed, dist))
    
      })

    
  })
}

## To be copied in the UI
# mod_05_metrics_age_gender_ui("patients_age_gender")

## To be copied in the server
# mod_05_metrics_age_gender_server("patients_age_gender")