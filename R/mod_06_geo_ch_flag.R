mod_06_geo_ch_flag_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2(
      "Estimated prescribing patterns for care home patients aged 65 years or over"
    ),
    nhs_card(
      heading = "Estimated average prescribing metrics per patient month for 
                 older care home patients in England by geography",
      div(
        class = "nhsuk-grid-row",
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
            selected = carehomes2::metrics_by_geo_and_ch_flag_df$FY %>%
              levels() %>%
              max,
            full_width = TRUE
          )
        ),
        div(
          class = "nhsuk-grid-column-one-half",
          nhs_selectInput(
            inputId = ns("metric"),
            label = "Metric",
            choices = c(
              "Drug cost PPM (\u00A3)" = "COST_PPM",
              "Number of prescription items PPM" = "ITEMS_PPM",
              "Number of unique medicines PPM" = "UNIQ_MEDS_PPM",
              "Patient months with 6+ unique medicines (%)" = "PCT_PM_GTE_SIX",
              "Patient months with 10+ unique medicines (%)" = "PCT_PM_GTE_TEN",
              "Patient months with ACB risk (%)" = "PCT_PM_ACB",
              "Patient months with DAMN risk (%)" = "PCT_PM_DAMN",
              "Number of unique fall-risk medicines PPM" = "UNIQ_MEDS_FALLS_PPM",
              "Patient months with falls risk (%)" = "PCT_PM_FALLS"
            ),
            full_width = TRUE
          )
        ),
        div(
          class = "nhsuk-grid-column-one-quarter",
          nhs_selectInput(
            inputId = ns("geography"),
            label = "Geography",
            choices = c("Region", "ICB", "Local Authority"),
            full_width = TRUE
          )
        )
      ),
      nhs_grid_2_col(
        highcharter::highchartOutput(ns("map_ch"), height = "500px"),
        highcharter::highchartOutput(ns("map_non_ch"), height = "500px")
      ),
      div(DT::DTOutput(ns("table"))),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Where the number of patients is less than 5 the data has been redacted."
      ),
      mod_nhs_download_ui(ns("download_data"))
    )
  )
}

mod_06_geo_ch_flag_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Metric name mappings ------------------------------------------------

    # Map metric column names to UI metric names
    ui_metric_names <- c(
      COST_PPM            = "Drug cost PPM (\u00A3)",
      ITEMS_PPM           = "Number of prescription items PPM",
      UNIQ_MEDS_PPM       = "Number of unique medicines PPM",
      PCT_PM_GTE_SIX      = "Patient months with 6+ unique medicines (%)",
      PCT_PM_GTE_TEN      = "Patient months with 10+ unique medicines (%)",
      PCT_PM_ACB          = "Patient months with ACB risk (%)",
      PCT_PM_DAMN         = "Patient months with DAMN risk (%)",
      UNIQ_MEDS_FALLS_PPM = "Number of unique fall-risk medicines PPM",
      PCT_PM_FALLS        = "Patient months with falls risk (%)"
    )
    
    # Map metric column names to tooltip metric names
    metric_tooltips <- c(
      COST_PPM            = "<b>Drug cost PPM:</b> \u00A3{point.value}",
      ITEMS_PPM           = "<b>Number of prescription items PPM:</b> {point.value:.1f}",
      UNIQ_MEDS_PPM       = "<b>Number of unique medicines PPM:</b> {point.value:.1f}",
      PCT_PM_GTE_SIX      = "<b>Patient months with 6+ unique medicines:</b> {point.value:.1f}%",
      PCT_PM_GTE_TEN      = "<b>Patient months with 10+ unique medicines:</b> {point.value:.1f}%",
      PCT_PM_ACB          = "<b>Patient months with ACB risk:</b> {point.value:.1f}%",
      PCT_PM_DAMN         = "<b>Patient months with DAMN risk:</b> {point.value:.1f}%",
      UNIQ_MEDS_FALLS_PPM = "<b>Number of unique fall-risk medicines PPM</b> {point.value:.1f}",
      PCT_PM_FALLS        = "<b>Patient months with falls risk</b> {point.value:.1f}%"
    )
    
    # Map all column names to download data names
    dl_col_names <- c(
      rlang::set_names(names(ui_metric_names), unname(ui_metric_names)),
      "Financial year"                      = "FY",
      "Geography"                           = "GEOGRAPHY",
      "Sub-geography code"                  = "SUB_GEOGRAPHY_CODE",
      "Sub-geography name"                  = "SUB_GEOGRAPHY_NAME",
      "Carehome Status"                     = "CH_FLAG",
      "Total patient months"                = "TOTAL_PM",
      "Total patient months with ACB risk"  = "TOTAL_PM_ACB",
      "Total patient months with DAMN risk" = "TOTAL_PM_DAMN"
    )
    
    # Formatted data ------------------------------------------------------
    
    fmt_data <- carehomes2::metrics_by_geo_and_ch_flag_df %>% 
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
        dplyr::filter(
          .data$GEOGRAPHY == input$geography,
          .data$FY == input$fy
        ) %>% 
        dplyr::mutate(
          .data$GEOGRAPHY,
          .data$SUB_GEOGRAPHY_NAME,
          .data$SUB_GEOGRAPHY_CODE,
          # Think this is not needed if going with the full data download option?
          # TOTAL_PATIENTS = switch(
          #   input$metric,
          #   "PCT_PATIENTS_GTE_SIX_PPM" = .data$TOTAL_PATIENTS_GTE_SIX,
          #   "PCT_PATIENTS_GTE_TEN_PPM" = .data$TOTAL_PATIENTS_GTE_TEN,
          #   "PCT_PATIENTS_ACB_6_PPM"   = .data$TOTAL_PATIENTS_ACB_6,
          #   "PCT_PATIENTS_ACB_DAMN"    = .data$TOTAL_PATIENTS_DAMN,
          #   "PCT_PATIENTS_FALLS"       = .data$TOTAL_PATIENTS_FALLS,
          #   .data$TOTAL_PATIENTS
          # ),
          .data[[input$metric]],
          .data$CH_FLAG,
          .keep = "none"
        )
    )
    
    map_data <- reactive(carehomes2::geo_data[[input$geography]])
    
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
            useHTML = TRUE,
            headerFormat = "",
            pointFormat = paste0(
              tags$b(input$geography, ": "), "{point.SUB_GEOGRAPHY_NAME}",
              tags$br(),
              metric_tooltips[input$metric] %>% unname()
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
    create_datatable <- function(data) {
      data %>%
        dplyr::filter(.data$GEOGRAPHY == input$geography) %>% 
        dplyr::mutate(
          .data$FY,
          CH_FLAG = dplyr::case_match(
            .data$CH_FLAG,
            TRUE  ~ "CH",
            FALSE ~ "NCH"
          ),
          !!rlang::sym(input$geography) := as.character(.data$SUB_GEOGRAPHY_NAME),
          .data[[input$metric]],
          .keep = "none"
        ) %>%
        tidyr::pivot_wider(
          names_from = dplyr::all_of(c("FY", "CH_FLAG")),
          values_from = .data[[input$metric]],
          names_sep = " "
        ) %>% 
        dplyr::arrange(.data[[input$geography]]) %>%
        # Move CH cols left of sub-geography column, so it is in centre col
        dplyr::relocate(
          dplyr::matches(" CH"),
          .before = !!rlang::sym(input$geography)
        ) %>% 
        # Apply styling for header names, also remove the extraneous CH/NCH
        # NOTE: cannot have identical col names, so we use an extra space for
        # one set
        dplyr::rename_with(
          \(cols) purrr::map_vec(
            cols,
            \(col) {
              span(
                class = "nhsuk-body-s",
                style = "font-size: 12px;",
                gsub(" NCH", " ", gsub(" CH", "", col))
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
            tabindex = "0",
            columnDefs = list(
              list(className = "dt-center", targets = "_all")
            )
          ),
          height = "400px",
          filter = "none",
          selection = "none"
        ) %>%
        DT::formatStyle(columns = 1:7, `font-size` = "12px") %>%
        DT::formatString(
          columns = (1:7)[-4],
          prefix = ifelse(input$metric == "COST_PPM", "\u00A3", ""),
          suffix = ifelse(grepl("PCT_", input$metric), "%", "")
        )
    }
    
    # Create download data (subset by active selections)
    # create_download_data <- function(data, 
    #                                  ch_status = c("Carehome", "Non-carehome")) {
    #   ifelse(
    #     ch_status == "Carehome",
    #     data <- data %>% dplyr::filter(.data$CH_FLAG),
    #     data <- data %>% dplyr::filter(!.data$CH_FLAG)
    #   )
    #   data %>% 
    #     dplyr::mutate(
    #       Geography = .data$GEOGRAPHY,
    #       `Sub geography name` = .data$SUB_GEOGRAPHY_NAME,
    #       `Sub geography code` = .data$SUB_GEOGRAPHY_CODE,
    #       `Total patients`     = .data$TOTAL_PATIENTS,
    #       !!rlang::sym(ui_metric_names[input$metric]) := ifelse(
    #         is.na(.data[[input$metric]]) & .data$TOTAL_PATIENTS > 0,
    #         "c",
    #         as.character(.data[[input$metric]])
    #       ),
    #       .keep = "none"
    #     )
    # }
    
    # Create download data (all data)
    create_download_data <- function(data) {
      temp <- data %>%
        # Need only if SDC is used
        # dplyr::mutate(
        #   # Use TOTAL_PATIENTS for non-% metrics...
        #   dplyr::across(
        #     dplyr::matches("^(?!PCT_).*_PPM$", perl = TRUE),
        #     \(x) dplyr::if_else(
        #       is.na(x) & .data$TOTAL_PATIENTS > 0,
        #       "c",
        #       as.character(x)
        #     )
        #   ),
        #   # ...but the associated TOTAL_PATIENTS_{X} column for % metrics
        #   dplyr::across(
        #     dplyr::matches("^(PCT_).*_PPM$", perl = TRUE), ~ {
        #       # The 'middle' is the substring w/o leading "PCT_" or trailing "_PPM"
        #       middle <- gsub("PCT_|_PPM", "", dplyr::cur_column())
        #       # Get vector of associated total column for test
        #       tot_vec <- dplyr::cur_data() %>% dplyr::pull(paste0("TOTAL_", middle))
        #       dplyr::if_else(
        #         is.na(.x) & tot_vec > 0,
        #         "c",
        #         as.character(.x)
        #       )
        #     }
        #   )
        # ) %>% 
        dplyr::arrange(
          .data$FY,
          .data$GEOGRAPHY,
          .data$SUB_GEOGRAPHY_NAME,
          dplyr::desc(.data$CH_FLAG)
        ) %>%
        dplyr::rename(dl_col_names)
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
    output$table <- DT::renderDT(
      create_datatable(fmt_data)
    )
    
    # Download buttons
    mod_nhs_download_server(
      id = "download_data",
      filename = "Selected Prescribing Metrics by Geography and Carehome Status.xlsx",
      export_data = create_download_data(fmt_data)
    )
  })
}
