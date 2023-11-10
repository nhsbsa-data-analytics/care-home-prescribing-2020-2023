#' 14_annex UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_14_annex_ui <- function(id){
  includeMarkdown("inst/markdown/14_annex.md")
}
    
#' 14_annex Server Functions
#'
#' @noRd 
mod_14_annex_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_14_annex_ui("14_annex_1")
    
## To be copied in the server
# mod_14_annex_server("14_annex_1")
