#' patients_age_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patients_age_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2_tabstop("Title"),
    h3_tabstop("Subtitle"),
    p("Text…"),
    nhs_card(
      heading = p("Chart heading"),
      nhs_grid_3_col(
        nhs_selectInput(inputId = ns("my_input1"),
                        label = "Year",
                        choices = c("1","2","3"),
                        full_width = T),
        nhs_selectInput(inputId = ns("geography"),
                        label = "Geography",
                        choices = names(careHomePrescribing2023::geographys),
                        full_width = T),
        nhs_selectInput(inputId = ns("sub_geography"),
                        label = "Sub Geography",
                        choices = NULL, # dynamically generated
                        full_width = T)
      ),
      highcharter::highchartOutput(outputId = ns("my_chart"), height = "400px"),
      mod_nhs_download_ui(id = ns("download_my_chart"))
    ),
    tags$div(style = "margin-top: 25vh") # Some buffer space after the chart
    
  )
}
    
#' patients_age_gender Server Functions
#'
#' @noRd 
mod_patients_age_gender_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_patients_age_gender_ui("patients_age_gender_1")
    
## To be copied in the server
# mod_patients_age_gender_server("patients_age_gender_1")
