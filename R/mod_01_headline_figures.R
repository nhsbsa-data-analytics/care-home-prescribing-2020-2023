#' mod 01 headline figures function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_headline_figures_ui <- function(id) {
  ns <- NS(id)
  tagList(
    include_dynamic_md("inst/markdown/01_headline_figures_1.md"),
    tags$div(style = "margin-top: 25vh"),
    includeMarkdown("inst/markdown/01_headline_figures_2.md"),
    nhs_card(
      heading = "Estimated number of patients, prescription items and drug cost 
                for care home patients aged 65 years and over in England",

      # Metric select input
      nhs_selectInput(
        inputId = ns("metric"),
        label = "Metric",
        choices = c(
          "Patient count" = "PATS",
          "Total prescription items" = "ITEMS",
          "Total drug cost (£)" = "NIC"
        ),
        full_width = FALSE
      ),
      
      # Require unequal column widths
      fluidRow(
        
        # First chart
        column(
          4,
          highcharter::highchartOutput(
            outputId = ns("headline_annual_chart"),
            height = "240px"
            )
          ),
        
        # Second chart       
        column(
          8,
          highcharter::highchartOutput(
            outputId = ns("headline_monthly_chart"),
            height = "250px"
            )
          )
        ),
      
      # Chart caption
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Patient counts are rounded to the nearest 100, while total prescription
        items and total cost (£) are rounded to the nearest 1,000."
      )
    ),
    tags$div(style = "margin-top: 25vh")
  )
}

#' mod 01 headline figures Server Functions
#'
#' @noRd
mod_01_headline_figures_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter the data based on the breakdown
    headline_figures_df <- reactive({
      
      # Ensure select input required
      req(input$metric)
      
      # Select and rename chosen column
      carehomes2::mod_headline_figures_df %>%
        dplyr::select('TYPE', 'TIME', input$metric) %>% 
        dplyr::rename_at(input$metric, ~"METRIC")
    })
    
    # Annual Chart
    output$headline_annual_chart <- highcharter::renderHighchart({
      headline_figures_df() %>% 
        dplyr::filter(TYPE == "Annual monthly mean") %>% 
        highcharter::hchart(., "column", highcharter::hcaes(TIME, METRIC, color = nhsbsaR::palette_nhsbsa()[1])) %>% 
        highcharter::hc_xAxis(title = list(text = "")) %>%  
        highcharter::hc_yAxis(
          min = 0,
          title = list(
            text = paste(
              switch(
                input$metric,
                "PATS" = "<b>Mean monthly patient count</b>",
                "ITEMS" = "<b>Mean monthly items</b>",
                "NIC" = "<b>Mean monthly cost (£)</b>"
              )
            )
          )
        ) %>% 
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Year: </b> {point.TIME}<br>",
            switch(
              input$metric,
              "PATS" = "<b>Mean monthly patients: </b> {point.METRIC:,.0f}",
              "ITEMS" = "<b>Mean monthly items: </b> {point.METRIC:,.0f}",
              "NIC" = "<b>Mean monthly cost: </b> £{point.METRIC:,.0f}"
            )
          )
        ) %>% 
        nhsbsaR::theme_nhsbsa_highchart()
    })
    
    # Monthly Chart
    output$headline_monthly_chart <- highcharter::renderHighchart({
      headline_figures_df() %>% 
        dplyr::filter(TYPE == "Monthly sum") %>% 
        highcharter::hchart(., "line", highcharter::hcaes(TIME, METRIC, color = nhsbsaR::palette_nhsbsa()[1])) %>% 
        highcharter::hc_xAxis(
          title = list(text = ""),
          plotBands = list(list(
            label = list(text = "COVID-19"),
            from = 0,
            to = 20,
            color = "#f0f0f0"
          ))
        ) %>% 
        highcharter::hc_yAxis(
          min = 0,
          title = list(
            text = paste(
              switch(
                input$metric,
                "PATS" = "<b>Patient count</b>",
                "ITEMS" = "<b>Items</b>",
                "NIC" = "<b>Cost (£)</b>"
                )
              )
            )
          ) %>% 
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>Month: </b> {point.TIME}<br>",
            switch(
              input$metric,
              "PATS" = "<b>Patient count: </b> {point.METRIC:,.0f}",
              "ITEMS" = "<b>Items: </b> {point.METRIC:,.0f}",
              "NIC" = "<b>Cost: </b> £{point.METRIC:,.0f}"
              )
            )
          ) %>% 
        nhsbsaR::theme_nhsbsa_highchart()
    })
    
    # Create download data
    create_download_data <- function(data) {
      data %>%
        dplyr::arrange(.data$TIME) %>%
        dplyr::rename(
          `Time period` = TIME,
          `Metric type` = TYPE,
          `Patient count` = PATS,
          `Total prescription items` = ITEMS,
          `Total drug cost` = NIC,
        )
    }
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "Headline figures for care home prescribing.xlsx",
      export_data = create_download_data(carehomes2::mod_headline_figures_df),
      number_xl_fmt_str = "#,##0"
    )
    
    observeEvent(
      carehomes2::mod_headline_figures_df,
      once = TRUE, {
        req(carehomes2::mod_headline_figures_df)
        
        insertUI(
          selector = ".nhsuk-card__description:eq(0)",
          where = "beforeEnd",
          ui = mod_nhs_download_ui(ns("download_data"))
        )
      })
  })
}

## To be copied in the UI
# mod_02_demographics_ui("02_demographics_ui_1")

## To be copied in the server
# mod_02_demographics_server("02_demographics_ui_1")