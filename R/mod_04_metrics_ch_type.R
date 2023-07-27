mod_04_metrics_ch_type_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown(app_sys("app", "www", "markdown", "04_metrics_ch_type.rmd")),
    nhs_card(
      heading = "Estimated prescribing metrics by prescribing setting for 
                 patients aged 65 years and over in England",
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-two-thirds",
          nhs_selectInput(
            inputId = ns("metric"),
            label = "Metric",
            choices = c(
              "Mean drug cost PPM" = "COST_PPM",
              "Mean items PPM" = "ITEMS_PPM",
              "Mean unique medicines PPM" = "UNIQ_MEDS_PPM",
              "% of patient-months with 6+ unique medicines" = "PCT_PM_GTE_SIX",
              "% of patient-months with 10+ unique medicines" = "PCT_PM_GTE_TEN",
              "% of patient-months with 2+ ACB medicines" = "PCT_PM_ACB",
              "% of patient-months with 2+ DAMN medicines" = "PCT_PM_DAMN",
              "Mean unique falls risk medicines PPM" = "UNIQ_MEDS_FALLS_PPM",
              "% of patient-months with 3+ falls risk medicines" = "PCT_PM_FALLS"
            ),
            full_width = TRUE
          )
        )
      ),
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_ch"), height = "250px")
        ),
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_non_ch"), height = "250px")
        )
      ),
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_nh"), height = "250px")
        ),
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_rh"), height = "250px")
        )
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Where the number of patients is less than 5 the data has been redacted."
      ),
      mod_nhs_download_ui(id = ns("download_data"))
    )
  )
}

mod_04_metrics_ch_type_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Metric and UI mappings-----------------------------------------------

    # Map FY to UI color
    ui_fy_colors <- list(
      `2020/21` = NHSRtheme::get_nhs_colours("DarkBlue"),
      `2021/22` = NHSRtheme::get_nhs_colours("BrightBlue"),
      `2022/23` = NHSRtheme::get_nhs_colours("AquaBlue")
    )
    
    # Map metric column names to UI metric names
    ui_metric_names <- c(
      COST_PPM            = "Mean drug cost PPM",
      ITEMS_PPM           = "Mean items PPM",
      UNIQ_MEDS_PPM       = "Mean unique medicines PPM",
      PCT_PM_GTE_SIX      = "% of patient-months with 6+ unique medicines",
      PCT_PM_GTE_TEN      = "% of patient-months with 10+ unique medicines",
      PCT_PM_ACB          = "% of patient-months with 2+ ACB medicines",
      PCT_PM_DAMN         = "% of patient-months with 2+ DAMN medicines",
      UNIQ_MEDS_FALLS_PPM = "Mean unique falls risk medicines PPM",
      PCT_PM_FALLS        = "% of patient-months with 3+ falls risk medicines"
    )
    
    # Map metric column names to tooltip metric names
    metric_tooltips <- c(
      COST_PPM            = "<b>Mean drug cost PPM:</b> \u00A3{point.y}",
      ITEMS_PPM           = "<b>Mean items PPM:</b> {point.y:.2f}",
      UNIQ_MEDS_PPM       = "<b>Mean unique medicines PPM:</b> {point.y:.2f}",
      PCT_PM_GTE_SIX      = "<b>% of patient-months with 6+ unique medicines:</b> {point.y:.2f}%",
      PCT_PM_GTE_TEN      = "<b>% of patient-months with 10+ unique medicines:</b> {point.y:.2f}%",
      PCT_PM_ACB          = "<b>% of patient-months with 2+ ACB medicines:</b> {point.y:.2f}%",
      PCT_PM_DAMN         = "<b>% of patient-months with 2+ DAMN medicines:</b> {point.y:.2f}%",
      UNIQ_MEDS_FALLS_PPM = "<b>Mean unique falls risk medicines PPM</b> {point.y:.2f}",
      PCT_PM_FALLS        = "<b>% of patient-months with 3+ falls risk medicines</b> {point.y:.2f}%"
    )
    
    # Map all column names to download data names
    dl_col_names <- c(
      rlang::set_names(names(ui_metric_names), unname(ui_metric_names)),
      "Financial year"                      = "FY",
      "Care home type"                       = "CH_TYPE",
      "Total patient-months"                = "TOTAL_PM",
      "Total patient-months with ACB risk"  = "TOTAL_PM_ACB",
      "Total patient-months with DAMN risk" = "TOTAL_PM_DAMN"
    )
    
    # Formatted data ------------------------------------------------------
    
    fmt_data <- carehomes2::metrics_by_ch_type_df %>% 
      dplyr::mutate(
        COST_PPM = janitor::round_half_up(COST_PPM, 0),
        dplyr::across(
          c(dplyr::ends_with("_PPM"), dplyr::starts_with("PCT_")),
          \(x) janitor::round_half_up(x, 2)
        )
      )
    
    # Reactive data -------------------------------------------------------
    
    fdata <- reactive(
      fmt_data %>%
        dplyr::mutate(
          .data$FY,
          .data$CH_TYPE,
          .data[[input$metric]],
          .keep = "none"
        )
    )
    
    # Output functions ----------------------------------------------------
    
    # Create chart
    create_chart <- function(data, 
                             ch_type = c(
                               "Care home",
                               "Nursing home",
                               "Residential home",
                               "Non-care home"
                             )) {
      ch_type <- match.arg(ch_type)
      
      data <- data %>% 
        dplyr::filter(.data$CH_TYPE == ch_type) %>% 
        dplyr::mutate(
          color = ui_fy_colors[.data$FY] %>%
            unlist() %>%
            unname()
        )
      
      # Get max of metric to use a common y-axis range
      y_max <- carehomes2::metrics_by_ch_type_df[[input$metric]] %>%
        max(na.rm = TRUE)
      
      x <- rlang::sym("FY")
      color <- rlang::sym("color")
      
      data %>% 
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = !!x,
            y = !!input$metric,
            color = !!color
          ),
          name = ui_metric_names[[input$metric]],
          tooltip = list(
            useHTML = TRUE,
            pointFormat = paste0(
              metric_tooltips[input$metric] %>% unname()
            )
          )
        ) %>%
        nhsbsaR::theme_nhsbsa_highchart() %>%
        highcharter::hc_xAxis(
          title = NULL,
          categories = data$FY %>%
            unique() %>%
            sort(),
          reversed = FALSE
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = ui_metric_names[[input$metric]]),
          min = 0,
          max = y_max
        ) %>%
        highcharter::hc_title(text = ch_type)
    }
    
    # Create download data
    create_download_data <- function(data) {
      data %>%
        dplyr::arrange(.data$FY, .data$CH_TYPE) %>%
        dplyr::rename(dl_col_names)
    }
    
    # Outputs -------------------------------------------------------------
    
    # Charts
    output$chart_ch <- highcharter::renderHighchart(
      create_chart(fdata(), "Care home")
    )
    output$chart_non_ch <- highcharter::renderHighchart(
      create_chart(fdata(), "Non-care home")
    )
    output$chart_nh <- highcharter::renderHighchart(
      create_chart(fdata(), "Nursing home")
    )
    output$chart_rh <- highcharter::renderHighchart(
      create_chart(fdata(), "Residential home")
    )
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "Selected prescribing metrics by care home type.xlsx",
      export_data = create_download_data(fmt_data)
    )
  })
}
