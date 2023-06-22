mod_06_geo_ch_flag_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2(
      "Estimated prescribing patterns for care home patients aged 65 years or over"
    ),
    p(tags$b("Geography")),
    p(
      tags$b(
        "The London region has the highest estimated average prescribing ",
        "costs and volumes per patient month."
      )
    ),
    p(
      "The London region features the highest average rate per patient month ",
      "on all four prescribing metrics and South West is lowest. There is ",
      "considerable variation per patient month by ICB and local authority ",
      "across metrics, with high pockets in several London and some West ",
      "Midland ICBs and local authorities."
    ),
    p(
      "Each of the metrics can be explored in the chart and table below by ",
      "region, local authority and ICB."
    ),
    nhs_card(
      heading = "Estimated average prescribing metrics per patient month for 
                 older care home patients in England by geography",
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-one-quarter",
          nhs_selectInput(
            inputId = ns("geography"),
            label = "Geography",
            choices = c("Region", "ICB", "Local Authority"),
            full_width = TRUE
          )
        ),
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
        div(
          class = "nhsuk-grid-column-one-quarter",
          nhs_selectInput(
            inputId = ns("fy"),
            label = "Financial Year",
            choices = c(
              "2020/21",
              "2021/22",
              "2022/23"
            ),
            full_width = TRUE
          )
        )
      ),
      nhs_grid_2_col(
        highcharter::highchartOutput(
          outputId = ns("map_ch"),
          height = "500px"
        ),
        highcharter::highchartOutput(
          outputId = ns("map_non_ch"),
          height = "500px"
        )
      ),
      nhs_grid_2_col(
        DT::DTOutput(
          outputId = ns("table_ch")
        ),
        DT::DTOutput(
          outputId = ns("table_non_ch")
        )
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Where the number of patients is less than 5 the data has been redacted."
      ),
      nhs_grid_2_col(
        mod_nhs_download_ui(
          id = ns("download_data_ch")
        ),
        mod_nhs_download_ui(
          id = ns("download_data_non_ch")
        )
      )
    )
  )
}

mod_06_geo_ch_flag_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Metric name mappings ------------------------------------------------

    # Map metric column names to UI metric names
    ui_metric_names <- c(
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
    
    fdata <- reactiveVal(
      carehomes2::metrics_by_geo_and_ch_flag %>%
        dplyr::filter(
          .data$GEOGRAPHY == "Region",
          .data$FY == "2020/21"
        ) %>% 
        dplyr::transmute(
          .data$GEOGRAPHY,
          .data$SUB_GEOGRAPHY_NAME,
          .data$SUB_GEOGRAPHY_CODE,
          .data$TOTAL_PATIENTS,
          .data$COST_PPM,
          .data$CH_FLAG
        )
    )
    
    map_data <- reactiveVal(carehomes2::geo_data$Region)
    
    observe({
      input$geography
      input$metric
      input$fy
      
      fdata({
        carehomes2::metrics_by_geo_and_ch_flag %>%
          dplyr::filter(
            .data$GEOGRAPHY == input$geography,
            .data$FY == input$fy
          ) %>% 
          dplyr::transmute(
            .data$GEOGRAPHY,
            .data$SUB_GEOGRAPHY_NAME,
            .data$SUB_GEOGRAPHY_CODE,
            TOTAL_PATIENTS = switch(
              input$metric,
              "PCT_PX_GTE_TEN_PPM" = .data$TOTAL_PATIENTS_GTE_TEN,
              .data$TOTAL_PATIENTS
            ),
            .data[[input$metric]],
            .data$CH_FLAG
          )
      })
      
      map_data(carehomes2::geo_data[[input$geography]])
    })
    
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
          stops = list(
            c(0, "#313695"),
            c(0.5, "#ffffbf"),
            c(1, "#a50026")
          )
        ) %>% 
        highcharter::hc_title(text = ch_status)
    }
    
    # Create datatable
    create_datatable <- function(ch_status = c("Carehome", "Non-carehome")) {
      ifelse(
        ch_status == "Carehome",
        data <- carehomes2::metrics_by_geo_and_ch_flag %>% dplyr::filter(.data$CH_FLAG),
        data <- carehomes2::metrics_by_geo_and_ch_flag %>% dplyr::filter(!.data$CH_FLAG)
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
              span(
                class = "nhsuk-body-s",
                style = "font-size: 12px;",
                short_col_name
              ) %>%
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
    create_download_data <- function(data, 
                                     ch_status = c("Carehome", "Non-carehome")) {
      ifelse(
        ch_status == "Carehome",
        data <- data %>% dplyr::filter(.data$CH_FLAG),
        data <- data %>% dplyr::filter(!.data$CH_FLAG)
      )
      
      data %>% 
        dplyr::mutate(
          "{input$metric}" := ifelse(
            is.na(.data[[input$metric]]) & .data$TOTAL_PATIENTS > 0,
            "c",
            as.character(.data[[input$metric]])
          )
        ) %>%
        dplyr::select(-.data$CH_FLAG) %>%
        dplyr::rename(
          Geography = .data$GEOGRAPHY,
          `Sub geography name` = .data$SUB_GEOGRAPHY_NAME,
          `Sub geography code` = .data$SUB_GEOGRAPHY_CODE,
          `Total patients`     = .data$TOTAL_PATIENTS,
          !!rlang::sym(dl_data_metric_names[input$metric]) := input$metric
        )
    }
    
    # Outputs -------------------------------------------------------------
    
    # Maps
    output$map_ch <- highcharter::renderHighchart(
      create_map(fdata(), map_data(), "Carehome")
    )
    output$map_non_ch <- highcharter::renderHighchart(
      create_map(fdata(), map_data(), "Non-carehome")
    )
    
    # Datatables
    output$table_ch <- DT::renderDT(
      create_datatable("Carehome")
    )
    output$table_non_ch <- DT::renderDT(
      create_datatable("Non-carehome")
    )
    
    # Download buttons
    mod_nhs_download_server(
      id = "download_data_ch",
      filename = function() glue::glue(
        "Carehome {dl_data_metric_names[input$metric]} ",
        "{input$fy %>% stringr::str_replace('/', '-')}.csv"
      ),
      export_data = create_download_data(fdata(), "Carehome")
    )
    mod_nhs_download_server(
      id = "download_data_non_ch",
      filename = function() glue::glue(
        "Non-carehome {dl_data_metric_names[input$metric]} ",
        "{input$fy %>% stringr::str_replace('/', '-')}.csv"
      ),
      export_data = create_download_data(fdata(), "Non-carehome")
    )
  })
}
