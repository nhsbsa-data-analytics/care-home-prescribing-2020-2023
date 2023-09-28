#' Address Matching UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_11_address_matching_ui <- function(id){
  includeMarkdown("inst/markdown/11_data_linkage.md")
}
    
#' Address Matching Server Function
#'
#' @noRd 
mod_11_address_matching_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_11_address_matching_ui("11_data_linkage_1")
    
## To be copied in the server
# mod_11_address_matching_server("11_data_linkage_1")
