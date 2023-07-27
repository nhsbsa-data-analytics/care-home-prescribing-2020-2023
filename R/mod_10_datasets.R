#' 10_datasets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_10_datasets_ui <- function(id){
  includeMarkdown(app_sys("app", "www", "markdown", "10_datasets.rmd"))
}
    
#' 10_datasets Server Functions
#'
#' @noRd 
mod_10_datasets_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_10_datasets_ui("10_datasets_1")
    
## To be copied in the server
# mod_10_datasets_server("10_datasets_1")
