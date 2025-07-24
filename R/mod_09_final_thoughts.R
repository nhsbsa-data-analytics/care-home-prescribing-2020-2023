#' 09_final_thoughts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_09_final_thoughts_ui <- function(id){
  include_dynamic_md("inst/markdown/09_final_thoughts.md")
}
    
#' 09_metrics Server Functions
#'
#' @noRd 
mod_09_final_thoughts_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
  })
}
    
## To be copied in the UI
# mod_09_final_thoughts_ui("09_final_thoughts")
    
## To be copied in the server
# mod_09_final_thoughts_server("09_final_thoughts")
