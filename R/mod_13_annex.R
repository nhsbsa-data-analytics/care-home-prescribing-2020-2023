#' 13_annex UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_13_annex_ui <- function(id){
  includeMarkdown("inst/markdown/13_annex.md")
}
    
#' 13_annex Server Functions
#'
#' @noRd 
mod_13_annex_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_13_annex_ui("12_feedback_1")
    
## To be copied in the server
# mod_13_annex_server("12_feedback_1")
