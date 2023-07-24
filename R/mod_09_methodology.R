#' mod_09_methodology UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_09_methodology_ui <- function(id){
  includeMarkdown(app_sys("app", "www", "09_methodology.rmd"))
}
    
#' methodology Server Functions
#'
#' @noRd 
mod_09_methodology_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_09_methodology_ui("methodology_1")
    
## To be copied in the server
# mod_09_methodology_server("methodology_1")
