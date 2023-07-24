library(shiny)
library(shinyWidgets)

ui <- shinyUI(
  fluidPage(
    uiOutput("test")
  )
)

server <- function(input, output) {
  output$test <- renderUI({
    includeHTML(rmarkdown::render("test_rmd.Rmd"))
  })
}

shinyApp(ui, server) 