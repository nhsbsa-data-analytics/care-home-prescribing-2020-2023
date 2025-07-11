#' mod 08 geographic care home drug analysis
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_08_geo_ch_flag_drug_ui <- function(id) {
  ns <- NS(id)

  reactable_table_css = "
  .my-header {
    border-bottom: 1px solid #4C4E52;
  }

  .my-row {
    border-bottom: 1.8px solid #ABB0B8;
  }

  .my-row:hover {
    background-color: #E6E6E3;
  }
  "

  tagList(
    tags$style(HTML(reactable_table_css)),
    includeMarkdown("inst/markdown/08_geo_ch_flag_drug.md"),

    # Overall nhs card
    nhs_card(

      # Overall Mod heading
      heading = "BNF-level prescribing estimates by geography for care home
                 patients aged 65 years and over in England",

      # 3 Tabs for differing geographies
      tabsetPanel(
        type = "tabs",

        # Tab 1: Region --------------------------------------------------------
        tabPanel(
          title = "Region",
          br(),

          # 3 select-inputs per tab
          nhs_grid_3_col(

            # Input 1: Metric
            nhs_selectInput(
              inputId = ns("input_region_metric"),
              label = "Metric",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$METRIC),
              full_width = TRUE
            ),

            # Input 2: BNF Parent
            nhs_selectInput(
              inputId = ns("input_region_bnf_parent"),
              label = "BNF level",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$BNF_PARENT),
              full_width = TRUE
            ),

            # Input 3: BNF Child
            nhs_selectInput(
              inputId = ns("input_region_bnf_child"),
              label = "BNF type",
              choices = carehomes2::mod_geo_ch_flag_drug_df %>%
                dplyr::filter(
                  GEOGRAPHY_PARENT == "Region",
                  BNF_PARENT == "Chapter"
                ) %>%
                dplyr::pull(BNF_CHILD) %>%
                unique() %>%
                sort(),
              full_width = TRUE
            )

          ),

          reactable::reactableOutput(outputId = ns("region_table")),
          br(),

          shiny::htmlOutput(outputId = ns("region_title")),
          uiOutput(ns("region_splines")),
          br(),

          # Chart caption
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Click on a row to display chart for one of the 7 NHS regions.",
            tags$br(),
            "Only the top 50 elements nationally by total item count across all 
             years per BNF level are presented.",
            tags$br(),
            "For example, only the 50 paragraphs with the largest national item
             count are described.",
            tags$br(),
            "The number of patients contributing to each metric are provided in 
             the data download, offering additional context.",
            tags$br(),
            "Patient counts and annual totals between one and four have been
             rounded to five, otherwise to the nearest ten.",
            tags$br(),
            "Values over 1,000 have been shortened with an appropriate suffix and
             then rounded to 2 decimal places.",
            tags$br(),
            "All other values are rounded to 2 decimal places."
          )
        ),

        # Tab 2: ICS -----------------------------------------------------------
        tabPanel(
          title = "ICS",
          br(),

          # 3 select-inputs per tab
          nhs_grid_3_col(

            # Input 1: Metric
            nhs_selectInput(
              inputId = ns("input_ics_metric"),
              label = "Metric",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$METRIC),
              full_width = TRUE
            ),

            # Input 2: BNF Parent
            nhs_selectInput(
              inputId = ns("input_ics_bnf_parent"),
              label = "BNF Level",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$BNF_PARENT),
              full_width = TRUE
            ),

            # Input 3: BNF Child
            nhs_selectInput(
              inputId = ns("input_ics_bnf_child"),
              label = "BNF Type",
              choices = carehomes2::mod_geo_ch_flag_drug_df %>%
                dplyr::filter(
                  GEOGRAPHY_PARENT == "ICS",
                  BNF_PARENT == "Chapter"
                ) %>%
                dplyr::pull(BNF_CHILD) %>%
                unique() %>%
                sort(),
              full_width = TRUE
            )

          ),

          reactable::reactableOutput(
            outputId = ns("ics_table"),
            height = "450px"
          ),
          br(),

          shiny::htmlOutput(outputId = ns("ics_title")),
          uiOutput(ns("ics_splines")),
          br(),

          # Chart caption
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Click on a row to display chart for one of the 42 ICSs.",
            tags$br(),
            "Only the top 50 elements nationally by total item count across all 
             years per BNF level are presented.",
            tags$br(),
            "For example, only the 50 paragraphs with the largest national item
             count are described.",
            tags$br(),
            "The number of patients contributing to each metric are provided in 
             the data download, offering additional context.",
            tags$br(),
            "Patient counts and annual totals between one and four have been
             rounded to five, otherwise to the nearest ten.",
            tags$br(),
            "Values over 1,000 have been shortened with an appropriate suffix and
             then rounded to 2 decimal places.",
            tags$br(),
            "All other values are rounded to 2 decimal places."
          )
        ),

        # Tab 3: Local Authority -----------------------------------------------
        tabPanel(
          title = "Local Authority",
          br(),

          # 3 select-inputs per tab
          nhs_grid_3_col(

            # Input 1: Metric
            nhs_selectInput(
              inputId = ns("input_lad_metric"),
              label = "Metric",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$METRIC),
              full_width = TRUE
            ),

            # Input 2: BNF Parent
            nhs_selectInput(
              inputId = ns("input_lad_bnf_parent"),
              label = "BNF Level",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$BNF_PARENT),
              full_width = TRUE
            ),

            # Input 3: BNF Child
            nhs_selectInput(
              inputId = ns("input_lad_bnf_child"),
              label = "BNF Type",
              choices = carehomes2::mod_geo_ch_flag_drug_df %>%
                dplyr::filter(
                  GEOGRAPHY_PARENT == "Local Authority",
                  BNF_PARENT == "Chapter"
                ) %>%
                dplyr::pull(BNF_CHILD) %>%
                unique() %>%
                sort(),
              full_width = TRUE
            )

          ),

          reactable::reactableOutput(
            outputId = ns("lad_table"),
            height = "450px"
          ),
          br(),

          shiny::htmlOutput(outputId = ns("lad_title")),
          uiOutput(ns("lad_splines")),
          br(),

          # Chart caption
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Click on a row to display chart for one of the 307 Local Authorities.",
            tags$br(),
            "The Isles of Scilly were removed due to the number of care homes in
             the Local Authority.",
            tags$br(),
            "City of London has no care home activity so is not present in this data",
            tags$br(),
            "Only the top 50 elements nationally by total item count across all 
             years per BNF level are presented.",
            tags$br(),
            "For example, only the 50 paragraphs with the largest national item
             count are described.",
            tags$br(),
            "The number of patients contributing to each metric are provided in 
             the data download, offering additional context.",
            tags$br(),
            "Patient counts and annual totals between one and four have been
             rounded to five, otherwise to the nearest ten.",
            tags$br(),
            "Values over 1,000 have been shortened with an appropriate suffix and
             then rounded to 2 decimal places.",
            tags$br(),
            "All other values are rounded to 2 decimal places."
          )
        )
      ),
    ),
    tags$div(style = "margin-top: 25vh") # Some buffer space after the chart
  )
}

mod_08_geo_ch_flag_drug_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper functions ---------------------------------------------------------

    # One: spline chart
    spline_chart_plot <- function(df, df_select, fy, metric, plot_pos = "") {
      # Shared y axis max value across all 3 plots
      y_axis_max_val <- max(df$`20/21`, df$`21/22`, df$`22/23`, df$`23/24`)

      accuracy <- \(val) {
        ifelse(startsWith(metric, "Total") & (val < 10^3), 1, 0.01)
      }

      # Process original df
      df <- df %>%
        dplyr::rename_at(fy, ~"VALUE") %>%
        dplyr::arrange(VALUE) %>%
        dplyr::mutate(
          index = dplyr::row_number(),
          rank = rev(dplyr::row_number()),
          total = max(rank),
          col = "#f7a35c",
          title = dplyr::case_when(
            GEOGRAPHY_CHILD == df_select ~ glue::glue("{fy}<br>{rank} of {total}")
          )
        )

      title <- df %>%
        dplyr::select(title) %>%
        tidyr::drop_na() %>%
        dplyr::pull()

      hc <- highcharter::highchart() %>%
        highcharter::hc_title(
          text = ifelse(
            plot_pos == "first",
            "Shared<br>y-axis",
            title
          ),
          style = list(
            fontSize = "12px",
            fontFamily = "arial",
            useHTML = TRUE
          )
        ) %>%
        highcharter::hc_add_series(df,
          "spline",
          highcharter::hcaes(rank, VALUE),
          showInLegend = FALSE
        ) %>%
        highcharter::hc_add_series(
          df %>% dplyr::filter(GEOGRAPHY_CHILD == df_select),
          "scatter",
          highcharter::hcaes(rank, VALUE, color = col),
          showInLegend = FALSE
        ) %>%
        highcharter::hc_yAxis(
          visible = plot_pos == "first",
          # Only first chart has y-axis
          min = 0,
          max = y_axis_max_val,
          showFirstLabel = TRUE,
          showLastLabel = TRUE,
          endOnTick = TRUE,
          tickWidth = 0,
          title = list(text = metric)
        ) %>%
        highcharter::hc_xAxis(
          min = 1,
          showLastLabel = TRUE,
          endOnTick = TRUE,
          tickWidth = 0,
          # Show label only for 1 and max rank
          tickPositioner = htmlwidgets::JS("
            function () {
              var positions = [1, this.dataMax];

              return positions;
            }
          "),
          title = list(text = "Rank"),
          reversed = TRUE
        ) %>%
        highcharter::hc_plotOptions(
          spline = list(
            marker = list(enabled = FALSE),
            states = list(inactive = list(opacity = 1))
          ),
          scatter = list(
            marker = list(radius = 5, symbol = "circle"),
            states = list(inactive = list(opacity = 1))
          ),
          series = list(
            animation = FALSE,
            states = list(
              hover = list(enabled = FALSE),
              select = list(enabled = FALSE)
            )
          )
        ) %>%
        highcharter::hc_tooltip(enabled = FALSE) %>%
        nhsbsaR::theme_nhsbsa_highchart() %>%
        highcharter::hc_plotOptions(spline = list(dataLabels = list(color = "black"))) %>%
        # Include credits on last chart only
        highcharter::hc_credits(enabled = plot_pos == "last")

      hc
    }

    # Select Inputs ------------------------------------------------------------

    # BNF lookup to speed up filtering
    region_lookup <- reactive({
      carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(GEOGRAPHY_PARENT == "Region") %>%
        dplyr::select(BNF_PARENT, BNF_CHILD) %>%
        dplyr::distinct() %>%
        dplyr::arrange(BNF_CHILD)
    })

    # BNF lookup to speed up filtering
    ics_lookup = reactive({
      carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(GEOGRAPHY_PARENT == "ICS") %>%
        dplyr::select(BNF_PARENT, BNF_CHILD) %>%
        dplyr::distinct() %>%
        dplyr::arrange(BNF_CHILD)
    })

    # BNF lookup to speed up filtering
    lad_lookup = reactive({
      carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(GEOGRAPHY_PARENT == "Local Authority") %>%
        dplyr::select(BNF_PARENT, BNF_CHILD) %>%
        dplyr::distinct() %>%
        dplyr::arrange(BNF_CHILD)
    })

    observeEvent(input$input_region_bnf_parent, {

      choices = region_lookup() %>%
        dplyr::filter(BNF_PARENT == input$input_region_bnf_parent) %>%
        dplyr::select(BNF_CHILD) %>%
        dplyr::pull()

      updateSelectInput(inputId = "input_region_bnf_child", choices = choices)
    })

    # Region: observe ICS parent choice
    observeEvent(input$input_ics_bnf_parent, {

      choices = ics_lookup() %>%
        dplyr::filter(BNF_PARENT == input$input_ics_bnf_parent) %>%
        dplyr::select(BNF_CHILD) %>%
        dplyr::pull()

      updateSelectInput(inputId = "input_ics_bnf_child", choices = choices)
    })

    # Region: observe lad parent choice
    observeEvent(input$input_lad_bnf_parent, {

      choices = lad_lookup() %>%
        dplyr::filter(BNF_PARENT == input$input_lad_bnf_parent) %>%
        dplyr::select(BNF_CHILD) %>%
        dplyr::pull()

      updateSelectInput(inputId = "input_lad_bnf_child", choices = choices)
    })

    # Initial df from select inputs --------------------------------------------

    # Metrics
    p1 = "% of total annual number of prescription items"
    p2 = "% of total annual drug cost"
    c1 = "Mean drug cost PPM"
    c2 = "Total annual drug cost"

    # Region: df after 4 initial filters applied
    region_df = reactive({
      # Filter, pivot and rename
      df <- carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::select(-PATS) %>%
        dplyr::filter(
          GEOGRAPHY_PARENT == "Region",
          BNF_PARENT == isolate(input$input_region_bnf_parent),
          BNF_CHILD == input$input_region_bnf_child,
          METRIC == input$input_region_metric
        ) %>%
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
        dplyr::select(
          GEOGRAPHY_PARENT,
          GEOGRAPHY_CHILD,
          dplyr::starts_with("20")
        ) %>%
        dplyr::rename_with(\(x) gsub("^20", "", x)) %>% 
        dplyr::arrange(GEOGRAPHY_CHILD)

      if (startsWith(input$input_region_metric, "Total")) {
        df <- df %>%
          dplyr::mutate(
            dplyr::across(
              -dplyr::starts_with("GEOGRAPHY"), bespoke_round
            )
          )
      }

      df
    })

    # ICS: df after 4 initial filters applied
    ics_df = reactive({

      # Filter, pivot an rename
      df <- carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::select(-PATS) %>%
        dplyr::filter(
          GEOGRAPHY_PARENT == "ICS",
          BNF_PARENT == isolate(input$input_ics_bnf_parent),
          BNF_CHILD == input$input_ics_bnf_child,
          METRIC == input$input_ics_metric
        ) %>%
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
        dplyr::select(
          GEOGRAPHY_PARENT,
          GEOGRAPHY_CHILD,
          dplyr::starts_with("20")
        ) %>%
        dplyr::rename_with(\(x) gsub("^20", "", x)) %>% 
        dplyr::arrange(GEOGRAPHY_CHILD)

      if (startsWith(input$input_ics_metric, "Total")) {
        df <- df %>%
          dplyr::mutate(
            dplyr::across(
              -dplyr::starts_with("GEOGRAPHY"), bespoke_round
            )
          )
      }

      df
    })

    # Lad: df after 4 initial filters applied
    lad_df = reactive({

      # Filter, pivot an rename
      df <- carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::select(-PATS) %>%
        dplyr::filter(
          GEOGRAPHY_PARENT == "Local Authority",
          BNF_PARENT == isolate(input$input_lad_bnf_parent),
          BNF_CHILD == input$input_lad_bnf_child,
          METRIC == input$input_lad_metric
        ) %>%
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
        dplyr::select(
          GEOGRAPHY_PARENT,
          GEOGRAPHY_CHILD,
          dplyr::starts_with("20")
        ) %>%
        dplyr::rename_with(\(x) gsub("^20", "", x)) %>% 
        dplyr::arrange(GEOGRAPHY_CHILD)

      if (startsWith(input$input_lad_metric, "Total")) {
        df <- df %>%
          dplyr::mutate(
            dplyr::across(
              -dplyr::starts_with("GEOGRAPHY"), bespoke_round
            )
          )
      }

      df
    })

    # LHS: Initial table ------------------------------------------------------

    # Function for each table
    geo_table = function(df, df_select, metric, geo_name){
      # reactable is limited when setting col widths based on content, e.g.
      # numeric columns to be certain width is not possible
      # https://github.com/glin/reactable/issues/399
      # Instead, can build columns beforehand
      fys <- names(dplyr::select(df, -(1:2)))
      columns <- purrr::map(fys, \(x) reactable::colDef(width = 70)) %>%
        stats::setNames(fys)
      columns <- c(list(.selection = reactable::colDef(width = 15)), columns)
      
      df %>%
        dplyr::rename_at("GEOGRAPHY_CHILD", ~geo_name) %>%
        dplyr::select(-GEOGRAPHY_PARENT) %>%
        reactable::reactable(
          selection = "single",
          defaultSelected = df_select,
          onClick = "select",
          pagination = FALSE,
          searchable = TRUE,
          striped = TRUE,
          highlight = TRUE,
          borderless = FALSE,
          columns = columns,
          defaultColDef = reactable::colDef(
            headerClass = "my-header",
            cell = function(val, row, col_name) {
              if (col_name %in% c(names(geographies), ".selection")) return (val)

              accuracy = ifelse(
                startsWith(metric, "Total") & (val < 10^3),
                1,
                0.01
              )

              return (
                scales::label_comma(
                  accuracy = accuracy,
                  # See this issue for reason why append of 1 is necessary:
                  # https://github.com/r-lib/scales/issues/413#issuecomment-1876179071 #gitleaks:allow
                  scale_cut = append(scales::cut_long_scale(), 1, 1)
                )(janitor::round_half_up(val, 2))
              )
            },
            footer = function(values, name) {
              # Do nothing for selection column
              if (name == ".selection") return ()
              
              # If numeric column, apply usual formatting
              if (is.numeric(values)) {
                # Get mean values of column
                val = mean(values, na.rm = TRUE)
                
                # Control behaviour for sub 1K Total values vs others
                accuracy = ifelse(
                  startsWith(metric, "Total") & (val < 10^3),
                  1,
                  0.01
                )
                
                htmltools::div(tags$b(
                  scales::label_comma(
                    accuracy = accuracy,
                    # See this issue for reason why append of 1 is necessary:
                    # https://github.com/r-lib/scales/issues/413#issuecomment-1876179071 #gitleaks:allow
                    scale_cut = append(scales::cut_long_scale(), 1, 1)
                  )(janitor::round_half_up(val, 2))
                ))
              # If character column set 'row' name
              } else if (is.character(values)) {
                htmltools::div(tags$b("National average"))
              }
            }
          ),
          style = list(fontSize = "14px", fontFamily = "Arial"),
          theme = reactable::reactableTheme(stripedColor = "#f8f8f8"),
          class = "my-tbl",
          rowClass = "my-row"
        )
    }

    # Region: select row
    index_region = reactive({
      # Select 1st row on initialisation
      t <- reactable::getReactableState("region_table", "selected")
      ifelse(is.null(t), 1, t)
    })

    # Region: select row
    index_ics = reactive({
      t <- reactable::getReactableState("ics_table", "selected")
      ifelse(is.null(t), 1, t)
    })

    # Region: select row
    index_lad = reactive({
      t <- reactable::getReactableState("lad_table", "selected")
      ifelse(is.null(t), 1, t)
    })

    # Region: Initial table
    output$region_table = reactable::renderReactable({

      # Plot table
      geo_table(region_df(), index_region(), input$input_region_metric, "Region") %>%
        htmlwidgets::onRender("() => {$('.rt-no-data').removeAttr('aria-live')}")
    })

    # ICS: Initial table
    output$ics_table = reactable::renderReactable({

      # Plot table
      geo_table(ics_df(), index_ics(), input$input_ics_metric, "ICS") %>%
        htmlwidgets::onRender("() => {$('.rt-no-data').removeAttr('aria-live')}")
    })

    # LA: Initial table
    output$lad_table = reactable::renderReactable({

      # Plot table
      geo_table(lad_df(), index_lad(), input$input_lad_metric, "Local Authority") %>%
        htmlwidgets::onRender("() => {$('.rt-no-data').removeAttr('aria-live')}")
    })

    # Table affects ------------------------------------------------------------

    # Region: get selected row category name
    selected_region <- reactive({
      region_df() %>%
        dplyr::filter(dplyr::row_number() == index_region()) %>%
        dplyr::select(GEOGRAPHY_CHILD) %>%
        dplyr::pull()
    })

    # ICS: get selected row category name
    selected_ics <- reactive({
      ics_df() %>%
        dplyr::filter(dplyr::row_number() == index_ics()) %>%
        dplyr::select(GEOGRAPHY_CHILD) %>%
        dplyr::pull()
    })

    # Lad: get selected row category name
    selected_lad <- reactive({
      lad_df() %>%
        dplyr::filter(dplyr::row_number() == index_lad()) %>%
        dplyr::select(GEOGRAPHY_CHILD) %>%
        dplyr::pull()
    })

    # Create chart titles from selection
    output$region_title = renderUI({

      # Ensure select input required
      req(index_region())

      # Region name text
      tags$div(
        style = "text-align: center;",
        tags$text(
          style = "font-weight: bold; font-size: 12pt;",
          glue::glue("Annual ranking for {selected_region()}")
        )
      )
    })

    # Create chart titles from selection
    output$ics_title = renderUI({

      # Ensure select input required
      req(index_ics())

      # Region name text
      tags$div(
        style = "text-align: center;",
        tags$text(
          style = "font-weight: bold; font-size: 12pt;",
          glue::glue("Annual ranking for {selected_ics()}")
        )
      )
    })

    # Create chart titles from selection
    output$lad_title = renderUI({

      # Ensure select input required
      req(index_lad())

      # Region name text
      tags$div(
        style = "text-align: center;",
        tags$text(
          style = "font-weight: bold; font-size: 12pt;",
          glue::glue("Annual ranking for {selected_lad()}")
        )
      )
    })

    # 4 charts -----------------------------------------------------------------

    # Region charts
    output$region_splines <- renderUI({
      # Get distinct financial years from column names
      fin_years <- region_df() %>%
        dplyr::select(-dplyr::starts_with("GEOGRAPHY")) %>%
        names()

      # Add a duplicate of first year at start (will be used as a dummy chart to
      # get y-axis)
      fin_years <- c(min(fin_years), fin_years)

      # Position matters only for first and last chart
      plot_pos <- c("first", rep("", length(fin_years) - 2), "last")

      # Map each year and position to spline chart ...
      purrr::map2(fin_years, plot_pos, \(fy, pos) {
        spline_chart_plot(
          region_df(),
          selected_region(),
          fy,
          input$input_region_metric,
          pos
        )
      }) %>%
        # ... and create a single row grid
        highcharter::hw_grid(ncol = length(fin_years), rowheight = 250) %>%
        # Required to allow shiny to display the grid
        htmltools::browsable()
    })

    # Ics charts
    output$ics_splines <- renderUI({
      fin_years <- ics_df() %>%
        dplyr::select(-dplyr::starts_with("GEOGRAPHY")) %>%
        names()

      fin_years <- c(min(fin_years), fin_years)

      plot_pos <- c("first", rep("", length(fin_years) - 2), "last")

      purrr::map2(fin_years, plot_pos, \(fy, pos) {
        spline_chart_plot(
          ics_df(),
          selected_ics(),
          fy,
          input$input_ics_metric,
          pos
        )
      }) %>%
        highcharter::hw_grid(ncol = length(fin_years), rowheight = 250) %>%
        htmltools::browsable()
    })

    # Lad charts
    output$lad_splines <- renderUI({
      fin_years <- lad_df() %>%
        dplyr::select(-dplyr::starts_with("GEOGRAPHY")) %>%
        names()

      fin_years <- c(min(fin_years), fin_years)

      plot_pos <- c("first", rep("", length(fin_years) - 2), "last")

      purrr::map2(fin_years, plot_pos, \(fy, pos) {
        spline_chart_plot(
          lad_df(),
          selected_lad(),
          fy,
          input$input_lad_metric,
          pos
        )
      }) %>%
        highcharter::hw_grid(ncol = length(fin_years), rowheight = 250) %>%
        htmltools::browsable()
    })

    # Downloads ----------------------------------------------------------------

    # Create download data
    create_download_data <- function(data) {
      tryCatch(
        return (carehomes2::bnf_level_prescribing_estimates_in_care_homes_df),
        error = \(e) {
          data <- data %>%
            tidyr::pivot_wider(
              names_from = .data$METRIC,
              values_from = .data$VALUE
            ) %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::starts_with("Total"), bespoke_round
              )
            )
          print(nrow(data))
          # Need to start a new chain to prevent dplyr trying to arrange the
          # original longer vectors
          data %>%
            dplyr::arrange(
              .data$FY,
              .data$GEOGRAPHY_PARENT,
              .data$GEOGRAPHY_CHILD,
              .data$BNF_PARENT,
              .data$BNF_CHILD
            ) %>%
            dplyr::rename(
              `Financial year` = .data$FY,
              Geography = .data$GEOGRAPHY_PARENT,
              `Sub-geography name` = .data$GEOGRAPHY_CHILD,
              `BNF level` = .data$BNF_PARENT,
              `BNF sub-level` = .data$BNF_CHILD,
              `Patient count` = .data$PATS
            ) %>%
            dplyr::mutate(`Patient count` = bespoke_round(`Patient count`))
        }
      )
    }

    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "BNF-level prescribing estimates in care homes.xlsx",
      export_data = create_download_data(carehomes2::mod_geo_ch_flag_drug_df),
      currency_xl_fmt_str = "£#,##0.00",
      number_xl_fmt_str = "#,##0.00"
    )
    
    observeEvent(
      carehomes2::mod_geo_ch_flag_drug_df,
      once = TRUE, {
      req(carehomes2::mod_geo_ch_flag_drug_df)
      
      insertUI(
        selector = ".nhsuk-card__description:eq(6)",
        where = "beforeEnd",
        ui = mod_nhs_download_ui(ns("download_data"))
      )
    })
  })
}
