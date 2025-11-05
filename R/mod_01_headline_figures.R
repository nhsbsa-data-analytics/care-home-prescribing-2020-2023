mod_01_headline_figures_ui <- function(id) {
  ns <- NS(id)

  tagList(
    include_dynamic_md("inst/markdown/01_headline_figures_1.md"),
    tags$div(style = "margin-top: 25vh"),
    include_dynamic_md("inst/markdown/01_headline_figures_2.md"),
    nhs_card(
      heading = "Estimated number of patients, prescription items and drug cost
                for care home patients aged 65 years and over in England",

      # Select inputs
      nhs_grid_3_col(
        nhs_selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Patient count" = "PATS",
            "Total prescription items" = "ITEMS",
            "Total drug cost (£)" = "NIC",
            "Item % for care homes" = "ITEMS_PERC",
            "Cost % for care homes" = "NIC_PERC"
          ),
          full_width = FALSE
        ),
        nhs_selectInput(
          inputId = ns("geography"),
          label = "Geography",
          choices = levels(carehomes2::mod_headline_figures_df$GEOGRAPHY),
          full_width = FALSE
        ),
        nhs_selectInput(
          inputId = ns("sub_geography"),
          label = "Sub Geography",
          choices = "Overall", # initial value, dynamically updated
          full_width = FALSE
        )
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
        textOutput(ns("caption"))
      )
    ),
    tags$div(style = "margin-top: 25vh")
  )
}

mod_01_headline_figures_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Metric name mappings ------------------------------------------------

    # Map metric column names to UI metric names
    ui_metric_names <- c(
      PATS = "Mean monthly patient count",
      ITEMS = "Mean monthly items",
      NIC = "Mean monthly cost (£)",
      ITEMS_PERC = "Item % for care homes",
      NIC_PERC = "Cost % for care homes"
    )

    # Map metric column names to tooltip metric names
    metric_tooltips <- c(
      PATS = "<b>Mean monthly patients: </b> {point.METRIC:,.0f}",
      ITEMS = "<b>Mean monthly items: </b> {point.METRIC:,.0f}",
      NIC = "<b>Mean monthly cost: </b> £{point.METRIC:,.0f}",
      ITEMS_PERC = "<b>Item % for care homes: </b> {point.METRIC:,.1f}",
      NIC_PERC = "<b>Cost % for care homes: </b> {point.METRIC:,.1f}"
    )

    # Map all column names to download data names
    dl_col_names <- c(
      rlang::set_names(names(ui_metric_names), unname(ui_metric_names)),
      "Time period" = "TIME",
      "Metric type" = "TYPE",
      "Geography" = "GEOGRAPHY",
      "Sub-geography code" = "SUB_GEOGRAPHY_CODE",
      "Sub-geography name" = "SUB_GEOGRAPHY_NAME"
    )

    # Formatted data ------------------------------------------------------

    fmt_data <- carehomes2::mod_headline_figures_df %>%
      dplyr::filter(.data$SUB_GEOGRAPHY_NAME != "Isles of Scilly") %>% 
      dplyr::rename_with(\(x) gsub("PROP", "PERC", x), dplyr::ends_with("PROP")) %>% 
      dplyr::mutate(
        # All except LAD: Patients nearest 100, Items 1,000, Cost 10,000
        # LAD: Patients nearest 10, Items 10, Cost 10
        PATS = dplyr::case_when(
          GEOGRAPHY == "Local Authority" ~ janitor::round_half_up(PATS, -1),
          TRUE ~ janitor::round_half_up(PATS, -2)
        ),
        ITEMS = dplyr::case_when(
          GEOGRAPHY == "Local Authority" ~ janitor::round_half_up(ITEMS, -1),
          TRUE ~ janitor::round_half_up(ITEMS, -3)
        ),
        NIC = dplyr::case_when(
          GEOGRAPHY == "Local Authority" ~ janitor::round_half_up(NIC, -1),
          TRUE ~ janitor::round_half_up(NIC, -4)
        ),
        dplyr::across(dplyr::ends_with("PERC"), \(x) round(100 * x, 1))
      )

    # Reactive data -------------------------------------------------------

    fdata <- reactive({
      req(input$sub_geography)

      fmt_data %>%
        dplyr::filter(
          .data$GEOGRAPHY == isolate(input$geography),
          .data$SUB_GEOGRAPHY_NAME == input$sub_geography
        ) %>%
        dplyr::mutate(
          .data$TIME,
          .data$TYPE,
          .data$GEOGRAPHY,
          .data$SUB_GEOGRAPHY_NAME,
          METRIC = .data[[input$metric]],
          .keep = "none"
        )
    })
    
    # Output functions ----------------------------------------------------

    # Annual chart
    create_headline_annual_chart <- function(data) {
      # Get max of metric to use a common y-axis range
      y_max <- fmt_data %>% 
        dplyr::filter(.data$GEOGRAPHY == isolate(input$geography)) %>%
        dplyr::pull(.data[[input$metric]]) %>% 
        max(na.rm = TRUE)
      
      data %>%
        dplyr::filter(.data$TYPE == "Annual") %>%
        highcharter::hchart(
          "column",
          highcharter::hcaes(
            .data$TIME,
            .data$METRIC,
            color = nhsbsaR::palette_nhsbsa()[1]
          )
        ) %>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = y_max,
          title = list(
            text = ui_metric_names[input$metric] %>% unname(),
            style = list(fontWeight = "bold")
          )
        ) %>%
        highcharter::hc_tooltip(
          useHTML = TRUE,
          headerFormat = "",
          pointFormat = paste0(
            tags$b(paste0("{point.TIME}", ": ")), "{point.SUB_GEOGRAPHY_NAME}",
            tags$br(),
            metric_tooltips[input$metric] %>% unname()
          )
        ) %>%
        nhsbsaR::theme_nhsbsa_highchart()
    }

    # Monthly chart
    create_headline_monthly_chart <- function(data) {
      # Get max of metric to use a common y-axis range
      y_max <- fmt_data %>% 
        dplyr::filter(.data$GEOGRAPHY == isolate(input$geography)) %>%
        dplyr::pull(.data[[input$metric]]) %>% 
        max(na.rm = TRUE)
      
      data %>%
        dplyr::filter(.data$TYPE == "Monthly") %>%
        highcharter::hchart(
          "line",
          highcharter::hcaes(
            .data$TIME,
            .data$METRIC,
            color = nhsbsaR::palette_nhsbsa()[1]
          )
        ) %>%
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
          max = y_max,
          title = list(
            text = ui_metric_names[input$metric] %>% unname(),
            style = list(fontWeight = "bold")
          )
        ) %>%
        highcharter::hc_tooltip(
          useHTML = TRUE,
          headerFormat = "",
          pointFormat = paste0(
            tags$b("Month: "), "{point.TIME}",
            tags$br(),
            metric_tooltips[input$metric] %>% unname()
          )
        ) %>%
        nhsbsaR::theme_nhsbsa_highchart()
    }

    # Create download data (all data)
    create_download_data <- function(data) {
      data %>%
        dplyr::rename(dl_col_names)
    }

    # Outputs -------------------------------------------------------------

    # Charts
    output$headline_annual_chart <- highcharter::renderHighchart({
      create_headline_annual_chart(fdata())
    })
    output$headline_monthly_chart <- highcharter::renderHighchart({
      create_headline_monthly_chart(fdata())
    })
    
    output$caption <- renderText({
      switch(
        input$geography,
        "Local Authority" = "Patient counts, total prescription items and total
         cost (£) are rounded to the nearest 10.",
        "Patient counts are rounded to the nearest 100, total prescription items
         are rounded to the nearest 1,000 and total cost (£) is rounded to the
         nearest 10,000."
      )
    })

    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "Headline figures for care home prescribing.xlsx",
      export_data = create_download_data(fmt_data),
      number_xl_fmt_str = "#,##0"
    )

    # Reactive events -----------------------------------------------------

    observeEvent(
      input$geography,
      {
        # ...get the unique sub-geographies for that geography...
        choices <- fmt_data %>%
          dplyr::filter(.data$GEOGRAPHY == input$geography) %>%
          dplyr::pull(.data$SUB_GEOGRAPHY_NAME) %>%
          unique()

        # ...and update the sub_geography selectInput with these new choices.
        updateSelectInput(
          inputId = "sub_geography",
          choices = choices
        )
      }
    )

    observeEvent(
      fmt_data,
      once = TRUE,
      {
        req(fmt_data)

        insertUI(
          selector = ".nhsuk-card__description:eq(0)",
          where = "beforeEnd",
          ui = mod_nhs_download_ui(ns("download_data"))
        )
      }
    )
  })
}
