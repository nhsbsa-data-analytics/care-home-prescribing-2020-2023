#' 15_updates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_15_updates_ui <- function(id){
  includeMarkdown("inst/markdown/15_updates.md")
}
    
#' 14_updates Server Functions
#'
#' @noRd 
mod_15_updates_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_15_updates_ui("15_updates_1")
    
## To be copied in the server
# mod_15_updates_server("15_updates_1")
