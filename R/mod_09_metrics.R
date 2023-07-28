#' 09_definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_09_metrics_ui <- function(id){
  includeMarkdown("inst/markdown/09_metrics.md")
}
    
#' 09_metrics Server Functions
#'
#' @noRd 
mod_09_metrics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_09_metrics_ui("09_metrics_1")
    
## To be copied in the server
# mod_09_metrics_server("09_metrics_1")
