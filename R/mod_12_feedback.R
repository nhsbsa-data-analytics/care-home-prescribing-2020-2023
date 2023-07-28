#' 12_feedback UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_12_feedback_ui <- function(id){
  includeMarkdown("inst/markdown/12_feedback.md")
}
    
#' 12_feedback Server Functions
#'
#' @noRd 
mod_12_feedback_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_12_feedback_ui("12_feedback_1")
    
## To be copied in the server
# mod_12_feedback_server("12_feedback_1")
