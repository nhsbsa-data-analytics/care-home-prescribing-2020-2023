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
          class = "nhsuk-grid-column-one-third",
          nhs_selectInput(
            inputId = ns("geography"),
            label = "Geography",
            choices = c("Region", "ICB", "Local Authority"),
            full_width = TRUE
          )
        ),
        div(
          class = "nhsuk-grid-column-one-third",
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
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-two-thirds",
          nhs_selectInput(
            inputId = ns("metric"),
            label = "Metric",
            choices = c(
              "Drug cost MPPM (\u00A3)" = "COST_PPM",
              "Number of prescription items MPPM" = "ITEMS_PPM",
              "Number of unique medicines MPPM" = "UNIQ_MEDS_PPM",
              "Patients on 6+ unique medicines MPPM (%)" = "PCT_PATIENTS_GTE_SIX_PPM",
              "Patients on 10+ unique medicines MPPM (%)" = "PCT_PATIENTS_GTE_TEN_PPM",
              "Patients with ACB 6+ MPPM (%)" = "PCT_PATIENTS_ACB_6_PPM",
              "Patients with DAMN 2+ MPPM (%)" = "PCT_PATIENTS_DAMN_PPM",
              "Number of unique fall-risk medicines MPPM" = "UNIQ_MEDS_FALLS_PPM",
              "Patients on 3+ unique falls-risk medicines MPPM (%)" = "PCT_PATIENTS_FALLS_PPM"
            ),
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
      COST_PPM                 = "Drug cost MPPM (\u00A3)",
      ITEMS_PPM                = "Number of prescription items MPPM",
      UNIQ_MEDS_PPM            = "Number of unique medicines MPPM",
      PCT_PATIENTS_GTE_SIX_PPM = "Patients on 6+ unique medicines MPPM (%)",
      PCT_PATIENTS_GTE_TEN_PPM = "Patients on 10+ unique medicines MPPM (%)",
      PCT_PATIENTS_ACB_6_PPM   = "Patients with ACB 6+ MPPM (%)",
      PCT_PATIENTS_DAMN_PPM    = "Patients with DAMN 2+ MPPM (%)",
      UNIQ_MEDS_FALLS_PPM      = "Number of unique fall-risk medicines MPPM",
      PCT_PATIENTS_FALLS_PPM   = "Patients on 3+ unique fall-risk medicines MPPM (%)"
    )
    
    # Map metric column names to tooltip metric names
    metric_tooltips <- c(
      COST_PPM                 = "<b>Drug cost MPPM:</b> \u00A3{point.value}",
      ITEMS_PPM                = "<b>Number of prescription items MPPM:</b> {point.value:.1f}",
      UNIQ_MEDS_PPM            = "<b>Number of unique medicines MPPM:</b> {point.value:.1f}",
      PCT_PATIENTS_GTE_SIX_PPM = "<b>Patients on 6+ unique medicines MPPM:</b> {point.value:.1f}%",
      PCT_PATIENTS_GTE_TEN_PPM = "<b>Patients on 10+ unique medicines MPPM:</b> {point.value:.1f}%",
      PCT_PATIENTS_ACB_6_PPM   = "<b>Patients with ACB 6+ MPPM:</b> {point.value:.1f}%",
      PCT_PATIENTS_DAMN_PPM    = "<b>Patients with DAMN 2+ MPPM:</b> {point.value:.1f}%",
      UNIQ_MEDS_FALLS_PPM      = "<b>Number of unique fall-risk medicines MPPM</b> {point.value:.1f}",
      PCT_PATIENTS_FALLS_PPM   = "<b>Patients on 3+ unique fall-risk medicines MPPM</b> {point.value:.1f}%"
    )
    
    # Map all column names to download data names
    dl_col_names <- c(
      rlang::set_names(names(ui_metric_names), unname(ui_metric_names)),
      "Financial year"                             = "FY",
      "Geography"                                  = "GEOGRAPHY",
      "Sub-geography code"                         = "SUB_GEOGRAPHY_CODE",
      "Sub-geography name"                         = "SUB_GEOGRAPHY_NAME",
      "Carehome Status"                            = "CH_FLAG",
      "Total patients"                             = "TOTAL_PATIENTS",
      "Patients on 6+ unique medicines"            = "TOTAL_PATIENTS_GTE_SIX",
      "Patients on 10+ unique medicines"           = "TOTAL_PATIENTS_GTE_TEN",
      "Patients with ACB 6+"                       = "TOTAL_PATIENTS_ACB_6",
      "Patients with DAMN 2+"                      = "TOTAL_PATIENTS_DAMN",
      "Patients on 3+ unique fall-risk medicines"  = "TOTAL_PATIENTS_FALLS"
    )
    
    # Reactive data -------------------------------------------------------
    
    fdata <- reactive(
      carehomes2::metrics_by_geo_and_ch_flag %>%
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
          !!rlang::sym(input$geography) := .data$SUB_GEOGRAPHY_NAME,
          .data[[input$metric]],
          .keep = "none"
        ) %>%
        tidyr::pivot_wider(
          names_from = dplyr::all_of(c("FY", "CH_FLAG")),
          values_from = .data[[input$metric]],
          names_sep = " "
        ) %>% 
        dplyr::arrange(.data[[input$geography]]) %>%
        dplyr::relocate(
          dplyr::ends_with("CH"),
          .before = dplyr::ends_with("NCH")
        ) %>% 
        dplyr::rename_with(
          \(cols) purrr::map_vec(
            cols,
            \(col) {
              span(
                class = "nhsuk-body-s",
                style = "font-size: 12px;",
                col
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
        DT::formatStyle(columns = 1:7, `font-size` = "12px") %>%
        DT::formatRound(
          columns = 2:7,
          digits = ifelse(input$metric != "COST_PPM", 1, 0)
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
    create_download_data_all <- function() {
      temp <- carehomes2::metrics_by_geo_and_ch_flag %>%
        dplyr::mutate(
          # Use TOTAL_PATIENTS for non-% metrics...
          dplyr::across(
            dplyr::matches("^(?!PCT_).*_PPM$", perl = TRUE),
            \(x) dplyr::if_else(
              is.na(x) & .data$TOTAL_PATIENTS > 0,
              "c",
              as.character(x)
            )
          ),
          # ...but the associated TOTAL_PATIENTS_{X} column for % metrics
          dplyr::across(
            dplyr::matches("^(PCT_).*_PPM$", perl = TRUE), ~ {
              # The 'middle' is the substring w/o leading "PCT_" or trailing "_PPM"
              middle <- gsub("PCT_|_PPM", "", dplyr::cur_column())
              # Get vector of associated total column for test
              tot_vec <- dplyr::cur_data() %>% pull(paste0("TOTAL_", middle))
              dplyr::if_else(
                is.na(.x) & tot_vec > 0,
                "c",
                as.character(.x)
              )
            }
          )
        ) %>% 
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
      create_datatable(carehomes2::metrics_by_geo_and_ch_flag)
    )
    
    # Download buttons
    mod_nhs_download_server(
      id = "download_data",
      filename = "Selected Prescribing Metrics by Geography and Carehome Status.xlsx",
      export_data = create_download_data_all()
    )
  })
}
