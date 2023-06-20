mod_04_metrics_ch_type_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2(
      "Estimated prescribing patterns for care home patients aged 65 years or over"
    ),
    p(
      "Care home patients aged 65 years or over received an estimated",
      tags$b("35 million"), "prescription items at a cost of",
      tags$b("£324 million"), "during 2020/21."
    ),
    p(
      "This represents 7% of the total primary care drug spend for ",
      "patients aged 65 years or over during 2020/21."
    ),
    p(
      tags$b(
        "The estimated average monthly drug cost for care home patients ",
        "aged 65 years or over is around twice that for non-care home patients ",
        "aged 65 years or over who received prescriptions."
      )
    ),
    p(
      "We estimate that care home patients aged 65 years or over receive around 60% ",
      "more prescription items and unique medicines per patient month at around ",
      "twice the cost than non-care home patients aged 65 years or over who received ",
      "prescriptions. These prescribing metrics vary by age, gender and ",
      "geography. The chart below allows you to explore them."
    ),
    br(),
    br(),
    p(
      tags$b(
        "The estimated average monthly drug cost for nursing home patients is ",
        "around 1.5 times more than for residential home patients"
      )
    ),
    p(
      "Despite being prescribed a",
      tags$b("similar number of prescription items"), "(both estimated to be ",
      "around 10 per patient month)",
      "the drug cost for", tags$b("nursing home patients is 1.5 times more"),
      "per patient month than for residential home patients."
    ),
    p(
      tags$b("Both"), "nursing and residential home patients are prescribed a",
      tags$b("similar number of unique medicines"),
      "per patient month and", tags$b("both"), "have approximately",
      tags$b("3 in 10 patients on ten or more unique medicines"), "per patient",
      "month.",
    ),
    p(
      "Nursing home patients would be expected to have slightly higher ",
      "prescribing metrics than residential home patients; a qualified nurse is ",
      "provided at nursing homes to cater for patients with more complex needs."
    ),
    nhs_card(
      heading = "Estimated average prescribing metrics per patient month for care home and non-care home patients aged 65 years or over in England by geography, age band or gender (2020/21)",
      div(
        class = "nhsuk-grid-row",
        div(class = "nhsuk-grid-column-one-quarter"),
        div(
          class = "nhsuk-grid-column-one-half",
          nhs_selectInput(
            inputId = ns("metric"),
            label = "Metric",
            choices = c(
              "Drug cost (PPM)" = "COST_PPM",
              "Number of prescription items (PPM)" = "ITEMS_PPM",
              "Number of unique medicines (PPM)" = "UNIQ_MEDS_PPM",
              "Patients on 10+ unique medicines (PPM)" = "PCT_PX_GTE_TEN_PPM"
            ),
            full_width = TRUE
          )
        ),
        div(class = "nhsuk-grid-column-one-quarter")
      ),
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_ch"))
        ),
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_non_ch"))
        )
      ),
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_nh"))
        ),
        div(
          class = "nhsuk-grid-column-one-half",
          highcharter::highchartOutput(ns("chart_rh"))
        )
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Where the number of patients is less than 5 the data has been redacted."
      ),
      nhs_grid_2_col(
        mod_nhs_download_ui(
          id = ns("download_data")
        ),
        div()
      )
    )
  )
}

mod_04_metrics_ch_type_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Metric name mappings----------------------------------------------

    # Map CH_TYPE to UI ch type names
    # ui_ch_type_names <- list(
    #   CH_FLAG     = "Carehome",
    #   NH_FLAG     = "Nursing Home",
    #   RH_FLAG     = "Residential Home",
    #   NON_CH_FLAG = "Non-carehome"
    # )
    
    # Map FY to UI color
    ui_fy_colors <- list(
      `2020/21` = NHSRtheme::get_nhs_colours("DarkBlue"),
      `2021/22` = NHSRtheme::get_nhs_colours("BrightBlue"),
      `2022/23` = NHSRtheme::get_nhs_colours("AquaBlue")
    )
    
    # Map metric column names to UI metric names
    ui_metric_names <- list(
      COST_PPM           = "Drug cost (\u00A3)",
      ITEMS_PPM          = "Number of prescription items",
      UNIQ_MEDS_PPM      = "Number of unique medicines",
      PCT_PX_GTE_TEN_PPM = "Patients on 10+ unique medicines (%)"
    )
    
    # Map metric column names to tooltip metric names
    metric_tooltips <- c(
      COST_PPM           = "<b>Drug cost:</b> \u00A3{point.value}",
      ITEMS_PPM          = "<b>Number of prescription items:</b> {point.value:.1f}",
      UNIQ_MEDS_PPM      = "<b>Number of unique medicines:</b> {point.value:.1f}",
      PCT_PX_GTE_TEN_PPM = "<b>Patients on 10+ unique medicines:</b> {point.value:.1f}%"
    )
    
    # Map metric column names to download data names
    dl_data_metric_names <- c(
      COST_PPM           = "Drug cost ppm (\u00A3)",
      ITEMS_PPM          = "Number of prescription items ppm",
      UNIQ_MEDS_PPM      = "Number of unique medicines ppm",
      PCT_PX_GTE_TEN_PPM = "Patients on 10+ unique medicines ppm (%)"
    )
    
    # Reactive data -------------------------------------------------------
    
    fdata <- reactive(
      carehomes2::metrics_by_ch_type %>%
        dplyr::transmute(
          .data$FY,
          .data$CH_TYPE,
          TOTAL_PATIENTS = dplyr::case_when(
            input$metric == "PCT_PX_GTE_TEN_PPM" ~ .data$TOTAL_PATIENTS_GTE_TEN,
            .default = .data$TOTAL_PATIENTS
          ),
          .data[[input$metric]]
        )
    )
    
    # Output functions ----------------------------------------------------
    
    # Create map
    create_map <- function(data,
                           map_data,
                           ch_status = c("Carehome", "Non-carehome")) {
      # Note that the final displayed limits will not exactly match the min and
      # max values - highcharts will pick appropriate numbers close to the limits
      color_axis_limits <- list(
        min = data[[input$metric]] %>% min(na.rm = TRUE),
        max = data[[input$metric]] %>% max(na.rm = TRUE)
      )
      
      ifelse(
        ch_status == "Carehome",
        data <- data %>% dplyr::filter(.data$CH_FLAG),
        data <- data %>% dplyr::filter(!.data$CH_FLAG)
      )
      
      highcharter::highchart() %>%
        highcharter::hc_add_series_map(
          df = data,
          map = map_data,
          joinBy = "SUB_GEOGRAPHY_CODE",
          value = input$metric,
          tooltip = list(
            headerFormat = "",
            pointFormat = paste0(
              "<b>", input$geography, ":</b> {point.SUB_GEOGRAPHY_NAME}<br>",
              metric_tooltips[input$metric]
            )
          )
        ) %>%
        nhsbsaR::theme_nhsbsa_highchart() %>%
        highcharter::hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = TRUE,
          enableDoubleClickZoom = TRUE
        ) %>%
        highcharter::hc_colorAxis(
          min = color_axis_limits$min,
          max = color_axis_limits$max,
          minColor = substr(min(viridisLite::plasma(n = 2)), 0, 7),
          maxColor = substr(max(viridisLite::plasma(n = 2)), 0, 7),
          stops = list(
            c(0, "#0D0887"),
            c(0.17, "#CC4678"),
            c(1, "#F0F921")
          )
        ) %>% 
        highcharter::hc_title(
          text = paste0(ch_status)
        )
    }
    
    # Create chart
    create_chart <- function(data, 
                             ch_type = c(
                               "Carehome",
                               "Nursing Home",
                               "Residential Home",
                               "Non-carehome"
                              )) {
      ch_type <- match.arg(ch_type)
      
      data <- data %>% 
        dplyr::filter(CH_TYPE == ch_type) %>% 
        dplyr::mutate(
          color = ui_fy_colors[.data$FY] %>%
            unlist() %>%
            unname()
        )
      
      # Get max of metric to use a common y-axis range
      y_max <- carehomes2::metrics_by_ch_type[[input$metric]] %>%
        max(na.rm = TRUE)
      
      data %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = FY,
            y = !!rlang::sym(input$metric),
            color = color
          )
        ) %>%
        nhsbsaR::theme_nhsbsa_highchart() %>%
        highcharter::hc_xAxis(
          title = list(text = "Financial Year"),
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
        highcharter::hc_tooltip(shared = TRUE) %>%
        highcharter::hc_title(text = ch_type)
    }
    
    # Create datatable
    create_datatable <- function(ch_status = c("Carehome", "Non-carehome")) {
      ifelse(
        ch_status == "Carehome",
        data <- data %>% dplyr::filter(.data$CH_FLAG),
        data <- data %>% dplyr::filter(!.data$CH_FLAG)
      )
      
      data %>%
        dplyr::filter(
          .data$GEOGRAPHY == input$geography,
          !is.na(.data$SUB_GEOGRAPHY_NAME)
        ) %>% 
        dplyr::select(
          .data$FY,
          !!rlang::sym(input$geography) := .data$SUB_GEOGRAPHY_NAME,
          .data[[input$metric]]
        ) %>%
        tidyr::pivot_wider(
          names_from = .data$FY,
          values_from = .data[[input$metric]]
        ) %>% 
        dplyr::arrange(.data[[input$geography]]) %>%
        dplyr::rename_with(
          \(cols) purrr::map_vec(
            cols,
            \(col) {
              short_col_name <- ifelse(
                startsWith(col, "20"),
                substr(col, 3, 7),
                col
              )
              span(class = "nhsuk-body-s", style = "font-size: 12px;", short_col_name) %>%
                as.character()
            }
          )
        ) %>%
        DT::datatable(
          escape = FALSE,
          rownames = FALSE,
          options = list(
            dom = "ft",
            scrollCollapse = TRUE,
            paging = FALSE,
            scrollY = "350px",
            overflow = "scroll",
            tabindex = "0"
          ),
          height = "400px",
          filter = "none",
          selection = "none"
        ) %>%
        DT::formatStyle(columns = 1:4, `font-size` = "12px") %>%
        DT::formatRound(
          columns = 2:4,
          digits = ifelse(input$metric != "COST_PPM", 1, 0)
        )
    }
    
    # Create download data
    create_download_data <- function() {
      carehomes2::metrics_by_ch_type %>% 
        dplyr::mutate(
          dplyr::across(
            c(.data$ITEMS_PPM, .data$COST_PPM, .data$UNIQ_MEDS_PPM),
            \(x) dplyr::if_else(
              is.na(x) & .data$TOTAL_PATIENTS > 0,
              "c",
              as.character(x)
            )
          ),
          PCT_PX_GTE_TEN_PPM = dplyr::case_when(
            is.na(.data$PCT_PX_GTE_TEN_PPM) & .data$TOTAL_PATIENTS > 0 ~ "c",
            .default = .data$PCT_PX_GTE_TEN_PPM %>% as.character()
          )
        ) %>% 
        arrange(CH_TYPE, FY) %>%
        dplyr::rename(
          `Financial year` = .data$FY,
          `Carehome type` = .data$CH_TYPE,
          `Total patients` = .data$TOTAL_PATIENTS,
          `Number of prescription items ppm` = .data$ITEMS_PPM,
          `Drug cost ppm (£)` = .data$COST_PPM,
          `Number of unique medicines ppm` = .data$UNIQ_MEDS_PPM,
          `Total patients on 10+ unique medicines` = .data$TOTAL_PATIENTS_GTE_TEN,
          `Patients on 10+ unique medicines ppm (%)` = .data$PCT_PX_GTE_TEN_PPM
        )
    }
    
    # Outputs -------------------------------------------------------------
    
    # Charts
    output$chart_ch <- highcharter::renderHighchart(
      create_chart(fdata(), "Carehome")
    )
    output$chart_non_ch <- highcharter::renderHighchart(
      create_chart(fdata(), "Non-carehome")
    )
    output$chart_nh <- highcharter::renderHighchart(
      create_chart(fdata(), "Nursing Home")
    )
    output$chart_rh <- highcharter::renderHighchart(
      create_chart(fdata(), "Residential Home")
    )
    
    # Download buttons
    mod_nhs_download_server(
      id = "download_data",
      filename = "Selected Prescribing Metrics by Carehome Type.csv",
      export_data = create_download_data()
    )
  })
}

