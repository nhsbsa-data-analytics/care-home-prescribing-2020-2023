#' 12_address_matching UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_12_address_matching_ui <- function(id){
  include_dynamic_md("inst/markdown/12_address_matching.md")
}
    
#' 12_address_matching Server Function
#'
#' @noRd 
mod_12_address_matching_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_12_address_matching_ui("12_address_matching_1")
    
## To be copied in the server
# mod_12_address_matching_server("12_address_matching_1")
