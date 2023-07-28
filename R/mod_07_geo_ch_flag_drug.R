# TODO: make things sentence case
# gsub("ppm", "PPM", gsub("Percent", "%", janitor::make_clean_names("% Drug Cost (PPM)", case = "sentence")))

#' mod 07 geographic care home drug analysis
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_07_geo_ch_flag_drug_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown(app_sys("app", "www", "markdown", "07_geo_ch_flag_drug.rmd")),
    
    # Overall nhs card
    nhs_card(
      
      # Overall Mod heading
      heading = "BNF level prescribing analysis by geography for care home 
                 patients aged 65 years and over in England",
      
      # 3 Tabs for differing geographies
      tabsetPanel(
        type = "tabs",
        
        # Tab 1: Region --------------------------------------------------------
        tabPanel(
          title = "Region",
          br(),
          #h4_tabstop("... Region ..."),
          
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
              label = "BNF Level",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$BNF_PARENT),
              full_width = TRUE
              ),
            
            # Input 3: BNF Child
            nhs_selectInput(
              inputId = ns("input_region_bnf_child"),
              label = "BNF Type",
              choices = NULL,
              full_width = TRUE
            )
              
          ),
        
          # First column with 1 long table
          fluidPage(
            
            # LHS: Single table
            column(
              7,
              reactable::reactableOutput(
                outputId = ns("region_table"),
                height = "525px"
                )
              ),
            # RHS: 3 charts
            column(
              5,
              shiny::htmlOutput(outputId = ns("region_title")),
              highcharter::highchartOutput(
                outputId = ns("region_chart_one"), 
                height = "175px"
              ),
              highcharter::highchartOutput(
                outputId = ns("region_chart_two"), 
                height = "175px"
              ),
              highcharter::highchartOutput(
                outputId = ns("region_chart_three"), 
                height = "175px"
              ),
              shiny::htmlOutput(outputId = ns("region_axis"))
              ),
            
            # Chart caption
            tags$text(
              class = "highcharts-caption",
              style = "font-size: 9pt",
              "Click on a row to select one of the 7 regions. Only the top 50 elements by total item count per BNF level are presented. ",
              "For example, only the top 50 paragraphs are presented, determined by the 50 paragraphs with the largest total item count."
            )
          )
        ),
        
        # Tab 2: ICS -----------------------------------------------------------
        tabPanel(
          title = "ICS",
          br(),
          #h4_tabstop("... ICB ..."),
          
          # 3 select-inputs per tab
          nhs_grid_3_col(
            
            # Input 1: Metric
            nhs_selectInput(
              inputId = ns("input_icb_metric"),
              label = "Metric",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$METRIC),
              full_width = TRUE
            ),
            
            # Input 2: BNF Parent
            nhs_selectInput(
              inputId = ns("input_icb_bnf_parent"),
              label = "BNF Level",
              choices = unique(carehomes2::mod_geo_ch_flag_drug_df$BNF_PARENT),
              full_width = TRUE
            ),
            
            # Input 3: BNF Child
            nhs_selectInput(
              inputId = ns("input_icb_bnf_child"),
              label = "BNF Type",
              choices = NULL,
              full_width = TRUE
            )
            
          ),
          
          # First column with 1 long table
          fluidPage(
            
            # LHS: Single table
            column(
              7,
              reactable::reactableOutput(
                outputId = ns("icb_table"),
                height = "525px"
              )
            ),
            # RHS: 3 charts
            column(
              5,
              shiny::htmlOutput(outputId = ns("icb_title")),
              highcharter::highchartOutput(
                outputId = ns("icb_chart_one"), 
                height = "175px"
              ),
              highcharter::highchartOutput(
                outputId = ns("icb_chart_two"), 
                height = "175px"
              ),
              highcharter::highchartOutput(
                outputId = ns("icb_chart_three"), 
                height = "175px"
              ),
              shiny::htmlOutput(outputId = ns("icb_axis"))
            ),
            
            # Chart caption
            tags$text(
              class = "highcharts-caption",
              style = "font-size: 9pt",
              "Click on a row to select one of the 42 ICBs. Only the top 50 elements by total item count per BNF level are presented. ",
              "For example, only the top 50 paragraphs are presented, determined by the 50 paragraphs with the largest total item count."
            )
          )
        ),
        
        # Tab 3: Local Authority -----------------------------------------------
        tabPanel(
          title = "Local Authority",
          br(),
          #h4_tabstop("... Local Authority ..."),
          
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
              choices = NULL,
              full_width = TRUE
            )
            
          ),
          
          # First column with 1 long table
          fluidPage(
            
            # LHS: Single table
            column(
              7,
              reactable::reactableOutput(
                outputId = ns("lad_table"),
                height = "525px"
              )
            ),
            # RHS: 3 charts
            column(
              5,
              shiny::htmlOutput(outputId = ns("lad_title")),
              highcharter::highchartOutput(
                outputId = ns("lad_chart_one"), 
                height = "175px"
              ),
              highcharter::highchartOutput(
                outputId = ns("lad_chart_two"), 
                height = "175px"
              ),
              highcharter::highchartOutput(
                outputId = ns("lad_chart_three"), 
                height = "175px"
              ),
              shiny::htmlOutput(outputId = ns("lad_axis"))
            ),
            
            # Chart caption
            tags$text(
              class = "highcharts-caption",
              style = "font-size: 9pt",
              "Click on a row to select one of the 308 Local Authorities. Only the top 50 elements by total item count per BNF level are presented. ",
              "For example, only the top 50 paragraphs are presented, determined by the 50 paragraphs with the largest total item count."
            )
          )
        )
      ),

      # Data download option
      mod_nhs_download_ui(
        id = ns("download_data")
      )
    )
  )
}

mod_07_geo_ch_flag_drug_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper functions ---------------------------------------------------------

    # One: spline chart
    spline_chart_plot = function(df, df_select, fy, prefix, suffix, bottom_plot = FALSE){

      # Shared y axis max value across all 3 plots
      y_axis_max_val = max(df$`20/21`, df$`21/22`, df$`22/23`)
      
      # Process original df
      df = df %>%
        dplyr::rename_at(fy, ~"VALUE") %>%
        dplyr::mutate(VALUE_LABEL = sprintf("%.2f", janitor::round_half_up(VALUE, 2))) %>% 
        dplyr::arrange(VALUE) %>%
        dplyr::mutate(
          index = dplyr::row_number(),
          rank = rev(dplyr::row_number()),
          total = max(rank),
          col =  "#f7a35c",
          label = dplyr::case_when(
            GEOGRAPHY_CHILD == df_select ~ paste0(fy, ":   ", prefix, VALUE_LABEL, suffix),
            TRUE ~ ""
          )
        )

      hc = highcharter::highchart() %>%
        highcharter::hc_add_series(
          df,
          "spline",
          highcharter::hcaes(index, VALUE),
          showInLegend = FALSE,
          dataLabels = list(
            y = 40,
            enabled = TRUE,
            format = "{point.label}",
            style = list(fontSize = "15px", fontFamily = "arial")
          )
        ) %>%
        highcharter::hc_add_series(
          df %>%  dplyr::filter(GEOGRAPHY_CHILD == df_select),
          "scatter",
          highcharter::hcaes(index, VALUE, color = col),
          showInLegend = FALSE
        ) %>%
        highcharter::hc_yAxis(
          min = 0, 
          max = y_axis_max_val,
          labels = list(format = "{value:.2f}")
          ) %>%
        highcharter::hc_xAxis(categories = c(rep("", max(df$index)+1))) %>%
        highcharter::hc_plotOptions(
          spline = list(
            marker = list(
              enabled = FALSE
            ),
            states = list(
              inactive = list(opacity = 1)
            )
          ),
          scatter = list(
            marker = list(
              radius = 5,
              symbol = "circle"
            ),
            states = list(
              inactive = list(opacity = 1)
            )
          ),
          series = list(
            animation = FALSE,
            states = list(
              hover = list(
                enabled = FALSE
              ),
              select = list(
                enabled = FALSE
              )
            )
          )
        ) %>%
        highcharter::hc_tooltip(enabled = FALSE) %>%
        nhsbsaR::theme_nhsbsa_highchart()
        
      
      # Only caption if bottom chart
      if(bottom_plot){
        hc = hc %>%
          highcharter::hc_credits(enabled = TRUE)
      }else{
        hc = hc %>%
          highcharter::hc_credits(enabled = FALSE)
      }
      hc
    }

    # Select Inputs ------------------------------------------------------------

    # BNF lookup to speed up filtering
    region_lookup = reactive({
      carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(GEOGRAPHY_PARENT == "Region") %>%
        dplyr::select(BNF_PARENT, BNF_CHILD) %>%
        dplyr::distinct() %>%
        dplyr::arrange(BNF_CHILD)
    })

    # BNF lookup to speed up filtering
    icb_lookup = reactive({
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

    # Region: observe bnf parent choice
    observeEvent(input$input_region_bnf_parent, {

      choices = region_lookup() %>%
        dplyr::filter(BNF_PARENT == input$input_region_bnf_parent) %>%
        dplyr::select(BNF_CHILD) %>%
        dplyr::pull()

      updateSelectInput(inputId = "input_region_bnf_child", choices = choices)
    })

    # Region: observe icb parent choice
    observeEvent(input$input_icb_bnf_parent, {

      choices = icb_lookup() %>%
        dplyr::filter(BNF_PARENT == input$input_icb_bnf_parent) %>%
        dplyr::select(BNF_CHILD) %>%
        dplyr::pull()

      updateSelectInput(inputId = "input_icb_bnf_child", choices = choices)
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
    
    # Pound sign for pound metrics
    region_prefix = reactive({ifelse(input$input_region_metric == c1, "£", "")})
    region_suffix = reactive({ifelse(input$input_region_metric %in% c(p1,p2), "%", "")})
    
    # Pound sign for pound metrics
    icb_prefix = reactive({ifelse(input$input_icb_metric == c1, "£", "")})
    icb_suffix = reactive({ifelse(input$input_icb_metric %in% c(p1,p2), "%", "")})
    
    # Pound sign for pound metrics
    lad_prefix = reactive({ifelse(input$input_lad_metric == c1, "£", "")})
    lad_suffix = reactive({ifelse(input$input_lad_metric %in% c(p1,p2), "%", "")})

    # Region: df after 4 initial filters applied
    region_df = reactive({

      # Ensure select input required
      req(input$input_region_bnf_child)
      req(input$input_region_bnf_parent)
      req(input$input_region_metric)

      # Filter, pivot an rename
      carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(
          GEOGRAPHY_PARENT == "Region",
          BNF_PARENT == input$input_region_bnf_parent,
          BNF_CHILD == input$input_region_bnf_child,
          METRIC == input$input_region_metric
        ) %>%
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
        dplyr::select(
          GEOGRAPHY_PARENT,
          GEOGRAPHY_CHILD,
          `20/21` = `2020/21`,
          `21/22` = `2021/22`,
          `22/23` = `2022/23`
        ) %>%
        dplyr::arrange(GEOGRAPHY_CHILD)
    })

    # Icb: df after 4 initial filters applied
    icb_df = reactive({

      # Ensure select input required
      req(input$input_icb_bnf_child)
      req(input$input_icb_bnf_parent)
      req(input$input_icb_metric)

      # Filter, pivot an rename
      carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(
          GEOGRAPHY_PARENT == "ICS",
          BNF_PARENT == input$input_icb_bnf_parent,
          BNF_CHILD == input$input_icb_bnf_child,
          METRIC == input$input_icb_metric
        ) %>%
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
        dplyr::select(
          GEOGRAPHY_PARENT,
          GEOGRAPHY_CHILD,
          `20/21` = `2020/21`,
          `21/22` = `2021/22`,
          `22/23` = `2022/23`
        ) %>%
        dplyr::arrange(GEOGRAPHY_CHILD)
    })

    # Lad: df after 4 initial filters applied
    lad_df = reactive({

      # Ensure select input required
      req(input$input_lad_bnf_child)
      req(input$input_lad_bnf_parent)
      req(input$input_lad_metric)

      # Filter, pivot an rename
      carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(
          GEOGRAPHY_PARENT == "Local Authority",
          BNF_PARENT == input$input_lad_bnf_parent,
          BNF_CHILD == input$input_lad_bnf_child,
          METRIC == input$input_lad_metric
        ) %>%
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE') %>%
        dplyr::select(
          GEOGRAPHY_PARENT,
          GEOGRAPHY_CHILD,
          `20/21` = `2020/21`,
          `21/22` = `2021/22`,
          `22/23` = `2022/23`
        ) %>%
        dplyr::arrange(GEOGRAPHY_CHILD)
    })

    # LHS: Initial table ------------------------------------------------------

    # Function for each table
    geo_table = function(df, df_select, geo_name, prefix, suffix){
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
          #outlined = TRUE, 
          highlight = TRUE,
          borderless = TRUE,
          columns = list(
            .selection = reactable::colDef(width = 15),
            `20/21` = reactable::colDef(width = 70, format = reactable::colFormat(digits = 2, prefix = prefix, suffix = suffix)),
            `21/22` = reactable::colDef(width = 70, format = reactable::colFormat(digits = 2, prefix = prefix, suffix = suffix)),
            `22/23` = reactable::colDef(width = 70, format = reactable::colFormat(digits = 2, prefix = prefix, suffix = suffix))
          ),
          style = list(fontSize = "14px", fontFamily = "Arial"),
          theme = reactable::reactableTheme(stripedColor = "#f8f8f8")
        )
    }
    
    # Region: select row
    index_region = reactive({
      # Select 1st row on initialisation
      t <- reactable::getReactableState("region_table", "selected")
      ifelse(is.null(t), 1, t)
    })
    
    # Region: select row
    index_icb = reactive({
      t <- reactable::getReactableState("icb_table", "selected")
      ifelse(is.null(t), 1, t) 
    })
    
    # Region: select row
    index_lad = reactive({
      t <- reactable::getReactableState("lad_table", "selected")
      ifelse(is.null(t), 1, t)
    })

    # Region: Initial table
    output$region_table = reactable::renderReactable({

      # Ensure select input required
      req(input$input_region_bnf_child)
      req(input$input_region_bnf_parent)
      req(input$input_region_metric)

      # Plot table
      geo_table(region_df(), index_region(), "Region", region_prefix(), region_suffix()) %>% 
      htmlwidgets::onRender("() => {$('.rt-no-data').removeAttr('aria-live')}")
    })

    # Icb: Initial table
    output$icb_table = reactable::renderReactable({

      # Ensure select input required
      req(input$input_icb_bnf_child)
      req(input$input_icb_bnf_parent)
      req(input$input_icb_metric)

      # Plot table
      geo_table(icb_df(), index_icb(), "ICS", icb_prefix(), icb_suffix()) %>% 
        htmlwidgets::onRender("() => {$('.rt-no-data').removeAttr('aria-live')}")
    })

    # LA: Initial table
    output$lad_table = reactable::renderReactable({

      # Ensure select input required
      req(input$input_lad_bnf_child)
      req(input$input_lad_bnf_parent)
      req(input$input_lad_metric)

      # Plot table
      geo_table(lad_df(), index_lad(), "Local Authority", lad_prefix(), lad_suffix()) %>% 
        htmlwidgets::onRender("() => {$('.rt-no-data').removeAttr('aria-live')}")
    })
    
    # LHS: table affects -------------------------------------------------------
    
    # Region: get selected row category name
    selected_region <- reactive({
      region_df() %>%
        dplyr::filter(dplyr::row_number() == index_region()) %>%
        dplyr::select(GEOGRAPHY_CHILD) %>%
        dplyr::pull()
    })

    # Icb: get selected row category name
    selected_icb <- reactive({
      icb_df() %>%
        dplyr::filter(dplyr::row_number() == index_icb()) %>%
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
          selected_region()
        )
      )
    })
    
    # Create chart titles from selection
    output$icb_title = renderUI({
      
      # Ensure select input required
      req(index_icb())
      
      # Region name text
      tags$div(
        style = "text-align: center;",
        tags$text(
          style = "font-weight: bold; font-size: 12pt;", 
          selected_icb()
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
          selected_lad()
        )
      )
    })
    
    # Region bottom axis text
    output$region_axis = renderUI({
      
      # Require region row select
      req(index_region())
      
      # Region axiss text
      tags$div(
        style = "text-align: center;",
        tags$text(
          style = "font-weight: bold; font-size: 12pt;", 
          "Region order (out of 8)"
        )
      )
    })
    
    # ICS bottom axis text
    output$icb_axis = renderUI({
      
      # Require region row select
      req(index_icb())
      
      # Region axiss text
      tags$div(
        style = "text-align: center;",
        tags$text(
          style = "font-weight: bold; font-size: 12pt;", 
          "ICS order (out of 42)"
        )
      )
    })
    
    # LA bottom axis text
    output$lad_axis = renderUI({
      
      # Require region row select
      req(index_lad())
      
      # Region axiss text
      tags$div(
        style = "text-align: center;",
        tags$text(
          style = "font-weight: bold; font-size: 12pt;", 
          "Local Authority order (out of 308)"
        )
      )
    })

    # RHS: 3 charts ------------------------------------------------------------

    # Region charts
    output$region_chart_one = highcharter::renderHighchart({
      req(index_region())
      spline_chart_plot(region_df(), selected_region(), "20/21", region_prefix(), region_suffix())
    })

    output$region_chart_two = highcharter::renderHighchart({
      req(index_region())
      spline_chart_plot(region_df(), selected_region(), "21/22", region_prefix(), region_suffix())
    })

    output$region_chart_three = highcharter::renderHighchart({
      req(index_region())
      spline_chart_plot(region_df(), selected_region(), "22/23", region_prefix(), region_suffix(), bottom_plot = TRUE)
    })

    # Ics charts
    output$icb_chart_one = highcharter::renderHighchart({
      req(index_icb())
      spline_chart_plot(icb_df(), selected_icb(), "20/21", icb_prefix(), icb_suffix())
    })

    output$icb_chart_two = highcharter::renderHighchart({
      req(index_icb())
      spline_chart_plot(icb_df(), selected_icb(), "21/22", icb_prefix(), icb_suffix())
    })

    output$icb_chart_three = highcharter::renderHighchart({
      req(index_icb())
      spline_chart_plot(icb_df(), selected_icb(), "22/23", icb_prefix(), icb_suffix(), bottom_plot = TRUE)
    })

    # Lad charts
    output$lad_chart_one = highcharter::renderHighchart({
      req(index_lad())
      spline_chart_plot(lad_df(), selected_lad(), "20/21", lad_prefix(), lad_suffix())
    })

    output$lad_chart_two = highcharter::renderHighchart({
      req(index_lad())
      spline_chart_plot(lad_df(), selected_lad(), "21/22", lad_prefix(), lad_suffix())
    })

    output$lad_chart_three = highcharter::renderHighchart({
      req(index_lad())
      spline_chart_plot(lad_df(), selected_lad(), "22/23", lad_prefix(), lad_suffix(), bottom_plot = TRUE)
    })
    
    # Downloads ----------------------------------------------------------------
    
    # Create download data
    create_download_data <- function(data) {
      data <- data %>%
        tidyr::pivot_wider(
          names_from = .data$METRIC,
          values_from = .data$VALUE
        ) 
      
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
          `BNF sub-level` = .data$BNF_CHILD
        )
    }
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "BNF level prescribing in care homes.xlsx",
      export_data = create_download_data(carehomes2::mod_geo_ch_flag_drug_df),
      currency_xl_fmt_str = "£#,##0.00"
    )
  })
}

## To be copied in the UI
# mod_07_geo_ch_flag_drug_ui("geo_ch_flag_drug")

## To be copied in the server
# mod_07_geo_ch_flag_drug_server("geo_ch_flag_drug")
