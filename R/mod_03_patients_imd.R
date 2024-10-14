#' mod 03 patients imd function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_03_patients_ch_type_ui <- function(id) {
  tagList(
    includeMarkdown("inst/markdown/03_patients_ch_type.md"),
    tags$div(style = "margin-top: 25vh") # Some buffer space after the chart
  )
}

#' mod 03 patienst imd Server Functions
#'
#' @noRd
mod_03_patients_imd_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    
  })
}
