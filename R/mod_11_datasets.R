#' 11_datasets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_11_datasets_ui <- function(id){
  includeMarkdown("inst/markdown/11_datasets.md")
}
    
#' 11_datasets Server Functions
#'
#' @noRd 
mod_11_datasets_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_11_datasets_ui("11_datasets_1")
    
## To be copied in the server
# mod_11_datasets_server("11_datasets_1")
