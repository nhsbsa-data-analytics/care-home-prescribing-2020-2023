mod_06_geo_ch_flag_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    includeMarkdown("inst/markdown/06_geo_ch_flag.md"),
    nhs_card(
      heading = "Estimated prescribing metrics for care home vs non-care home
                 patients aged 65 years and over in England",
      div(
        class = "nhsuk-grid-row",
        div(
          class = "nhsuk-grid-column-one-third",
          nhs_selectInput(
            inputId = ns("fy"),
            label = "Financial year",
            choices = carehomes2::metrics_by_geo_and_ch_flag_df$FY %>%
              levels(),
            selected = carehomes2::metrics_by_geo_and_ch_flag_df$FY %>%
              levels() %>%
              max(),
            full_width = TRUE
          )
        ),
        div(
          class = "nhsuk-grid-column-one-third",
          nhs_selectInput(
            inputId = ns("metric"),
            label = "Metric",
            choices = c(
              "Mean drug cost PPM" = "COST_PPM",
              "Mean prescription items PPM" = "ITEMS_PPM",
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
        ),
        div(
          class = "nhsuk-grid-column-one-third",
          nhs_selectInput(
            inputId = ns("geography"),
            label = "Geography",
            choices = names(geographies)[-1],
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
        style = "font-size: 9pt;",
        "Clicking a row will outline the selected area on the maps.",
        tags$br(),
        "The Isles of Scilly were removed due to the number of care homes in the
         Local Authority.",
        tags$br(),
        "Mean drug cost PPM is rounded to the nearest GBP. All other values 
        are rounded to 2 decimal places."
      )
    ),
    tags$div(style = "margin-top: 25vh") # Some buffer space after the chart
  )
}

mod_06_geo_ch_flag_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Metric name mappings ------------------------------------------------

    # Map metric column names to UI metric names
    ui_metric_names <- c(
      COST_PPM            = "Mean drug cost PPM",
      ITEMS_PPM           = "Mean prescription items PPM",
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
      COST_PPM            = "<b>Mean drug cost PPM</b> \u00A3{point.value}",
      ITEMS_PPM           = "<b>Mean prescription items PPM:</b> {point.value:.2f}",
      UNIQ_MEDS_PPM       = "<b>Mean unique medicines PPM:</b> {point.value:.2f}",
      PCT_PM_GTE_SIX      = "<b>% of patient-months with 6+ unique medicines:</b> {point.value:.2f}%",
      PCT_PM_GTE_TEN      = "<b>% of patient-months with 10+ unique medicines:</b> {point.value:.2f}%",
      PCT_PM_ACB          = "<b>% of patient-months with 2+ ACB medicines:</b> {point.value:.2f}%",
      PCT_PM_DAMN         = "<b>% of patient-months with 2+ DAMN medicines:</b> {point.value:.2f}%",
      UNIQ_MEDS_FALLS_PPM = "<b>Mean unique falls risk medicines PPM:</b> {point.value:.2f}",
      PCT_PM_FALLS        = "<b>% of patient-months with 3+ falls risk medicines:</b> {point.value:.2f}%"
    )
    
    # Map all column names to download data names
    dl_col_names <- c(
      rlang::set_names(names(ui_metric_names), unname(ui_metric_names)),
      "Financial year"                      = "FY",
      "Geography"                           = "GEOGRAPHY",
      "Sub-geography code"                  = "SUB_GEOGRAPHY_CODE",
      "Sub-geography name"                  = "SUB_GEOGRAPHY_NAME",
      "Care home status"                    = "CH_FLAG"
    )
    
    # Formatted data ------------------------------------------------------
    
    fmt_data <- carehomes2::metrics_by_geo_and_ch_flag_df %>% 
      dplyr::filter(SUB_GEOGRAPHY_NAME != "Isles of Scilly") %>% 
      dplyr::mutate(
        dplyr::across(
          c(dplyr::ends_with("_PPM"), dplyr::starts_with("PCT_")),
          \(x) janitor::round_half_up(x, 2)
        ),
        COST_PPM = janitor::round_half_up(COST_PPM, 0)
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
                           ch_status = c("Care home", "Non-care home")) {
      # City of London has no care home activity, so will have 0 for all metrics.
      # This should be treated as not existing when setting limits of the colour
      # scale.
      fmt_data_no_city_of_london <- fmt_data %>% 
        dplyr::filter(SUB_GEOGRAPHY_NAME != "City of London")
      
      # Note that the final displayed limits will not exactly match the min and
      # max values - highcharts will pick appropriate numbers close to the limits
      color_axis_limits <- list(
        min = fmt_data_no_city_of_london[[input$metric]] %>%
          min(na.rm = TRUE),
        max = fmt_data_no_city_of_london[[input$metric]] %>% 
          max(na.rm = TRUE)
      )
      
      ifelse(
        ch_status == "Care home",
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
          ),
          borderWidth = 0,
          borderColor = 'black'
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
            c(0, NHSRtheme::get_nhs_colours("DarkBlue") |> unname()),
            c(0.4, NHSRtheme::get_nhs_colours("LightBlue") |> unname()),
            c(0.5, NHSRtheme::get_nhs_colours("Yellow") |> unname()),
            c(0.6, NHSRtheme::get_nhs_colours("WarmYellow") |> unname()),
            c(1, NHSRtheme::get_nhs_colours("Red") |> unname())
          )
        ) %>%
        highcharter::hc_title(text = ch_status)
    }
    
    # Create datatable
    # NOTE: There is no arrange on SUB_GEOGRAPHY_NAME. Original code did have 
    # this, but it turned out that the underlying data is not sorted. This meant
    # that JS code for map border highlighting was using a different order to 
    # what was shown in the table. It should be possible to align the order in 
    # JS with the order in an `arrange`d table, but in this case the data is 
    # already well-ordered, so best solution is to just remove arrange.
    create_datatable <- function(data) {
      tdata <- data %>%
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
                gsub("^20", "", gsub(" NCH", " ", gsub(" CH", "", col)))
              ) %>%
                as.character()
            }
          )
        )
      
      table_container <- tags$table(
        DT::tableHeader(tdata, escape = FALSE),
        DT::tableFooter(
          purrr::map(
            tdata,
            function(x) ifelse(
              is.numeric(x),
              ifelse(
                grepl("cost", input$metric, ignore.case = TRUE),
                format(round(mean(x, na.rm = TRUE), 0), nsmall = 0),
                format(round(mean(x, na.rm = TRUE), 2), nsmall = 2)
              ),
              "National average"
            )
          )
        )
      )
      
      # Over-ride CSS in NHS front-end toolkit
      table_container <- htmltools::tagQuery(table_container)$
        find("tfoot>tr>th")$
        addAttrs("style" = "font-size: 12px;")$
        allTags()
      
      # Callback to handle empty cells and display as NA
      rowCallback <- c(
        "function(row, data){",
        "  for(var i=0; i<data.length; i++){",
        "    if(data[i] === null){",
        "      $('td:eq('+i+')', row).html('NA')",
        "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
        "    }",
        "  }",
        "}"
      )
      
      DT::datatable(
        tdata,
        escape = FALSE,
        container = table_container,
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
          ),
          rowCallback = DT::JS(rowCallback)
        ),
        height = "500px",
        filter = "none",
        selection = "single"
      ) %>%
        DT::formatStyle(columns = 1:ncol(tdata), `font-size` = "12px")
    }
    
    # Create download data (all data)
    create_download_data <- function(data) {
      data %>%
        dplyr::select(!dplyr::starts_with("TOTAL")) %>% 
        dplyr::mutate(
          CH_FLAG = ifelse(CH_FLAG, "Care home", "Non-care home")
        )  %>% 
        dplyr::arrange(
          .data$FY,
          .data$GEOGRAPHY,
          .data$SUB_GEOGRAPHY_NAME,
          data$CH_FLAG
        ) %>%
        dplyr::rename(dl_col_names)
    }
    
    # Outputs -------------------------------------------------------------
    
    # Maps
    output$map_ch <- highcharter::renderHighchart(
      create_map(fdata(), map_data(), "Care home")
    )
    output$map_non_ch <- highcharter::renderHighchart(
      create_map(fdata(), map_data(), "Non-care home")
    )
    
    # Datatables
    output$table <- DT::renderDT(
      create_datatable(fmt_data)
    )
    
    # Download buttons
    mod_nhs_download_server(
      id = "download_data",
      filename = "Selected prescribing metrics by geography and care home status.xlsx",
      export_data = create_download_data(fmt_data)
    )
    

    # Reactive events -----------------------------------------------------

    # Need to track previously selected row to toggle border w/o searching whole
    # dataset
    previous_row_selected <- reactiveVal(NULL)
    
    # When a table row is clicked, fire a custom Shiny message to toggle the
    # border of the same map area.
    observe({
      if (!is.null(previous_row_selected())) {
        session$sendCustomMessage(
          type = 'rowClicked',
          message = list(
            # Need index - 1 since JavaScript is 0-indexed vs R 1-indexed
            previous_row = previous_row_selected() - 1,
            row = input$table_rows_selected - 1
          )
        )
      } 
      
      # Update previous selected row to be the current selected row
      previous_row_selected(input$table_rows_selected)
    })
    
    observeEvent(
      fmt_data,
      once = TRUE, {
        req(fmt_data)
        
        insertUI(
          selector = ".nhsuk-card__description:eq(4)",
          where = "beforeEnd",
          ui = mod_nhs_download_ui(ns("download_data"))
        )
      })
  })
}
