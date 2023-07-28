#' mod_11_methodology UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_11_methodology_ui <- function(id){
  includeMarkdown("inst/markdown/11_methodology.md")
}
    
#' methodology Server Functions
#'
#' @noRd 
mod_11_methodology_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_11_methodology_ui("11_methodology_1")
    
## To be copied in the server
# mod_11_methodology_server("11_methodology_1")
