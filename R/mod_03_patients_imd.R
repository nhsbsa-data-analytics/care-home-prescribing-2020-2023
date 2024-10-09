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
  includeMarkdown("inst/markdown/03_patients_ch_type.md")
}

#' mod 03 patienst imd Server Functions
#'
#' @noRd
mod_03_patients_imd_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    
  })
}
