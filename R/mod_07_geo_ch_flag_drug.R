
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
    h2(
      "Demographic estimates for care home patients aged 65 years or over ",
      "receiving prescriptions"
    ),
    
    # Overall nhs card
    nhs_card(
      
      # Overall Mod heading
      heading = "... ABC ...",
      
      # 3 Tabs for differing geographies
      tabsetPanel(
        type = "tabs",
        
        # Tab 1: Region --------------------------------------------------------
        tabPanel(
          title = "Region",
          br(),
          h4_tabstop("... Region ..."),
          
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
            )
            ),
          
          # Chart caption
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Click on a row to select one of the 7 regions."
          ),
          
          # Data download option
          mod_nhs_download_ui(
            id = ns("download_region_table")
          )
          )
        ),
        
        # Tab 2: ICB -----------------------------------------------------------
        tabPanel(
          title = "ICB",
          br(),
          h4_tabstop("... ICB ..."),
          
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
              )
            ),
            
            # Chart caption
            tags$text(
              class = "highcharts-caption",
              style = "font-size: 9pt",
              "Click on a row to select one of the 42 ICBs."
            ),
            
            # Data download option
            mod_nhs_download_ui(
              id = ns("download_icb_table")
            )
          )
        ),
        
        # Tab 3: Local Authority -----------------------------------------------
        tabPanel(
          title = "Local Authority",
          br(),
          h4_tabstop("... Local Authority ..."),
          
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
              )
            ),
            
            # Chart caption
            tags$text(
              class = "highcharts-caption",
              style = "font-size: 9pt",
              "Click on a row to select one of the 308 Local Authorities."
            ),
            
            # Data download option
            mod_nhs_download_ui(
              id = ns("download_lad_table")
            )
          )
        )
      )
    )
  )
}

mod_07_geo_ch_flag_drug_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper functions ---------------------------------------------------------

    # One: spline chart
    spline_chart_plot = function(df, df_select, fy, top_plot = FALSE, bottom_plot = FALSE){

      # Shared y axis max value across all 3 plots
      y_axis_max_val = max(df$`20/21`, df$`21/22`, df$`22/23`)
      
      # Process original df
      df = df %>%
        dplyr::rename_at(fy, ~"VALUE") %>%
        dplyr::arrange(VALUE) %>%
        dplyr::mutate(
          index = dplyr::row_number(),
          rank = rev(dplyr::row_number()),
          total = max(rank),
          col =  "#f7a35c",
          label = dplyr::case_when(
            GEOGRAPHY_CHILD == df_select ~ paste0(fy, ":   ", VALUE),
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
        highcharter::hc_yAxis(min = 0, max = y_axis_max_val) %>%
        highcharter::hc_xAxis(categories = c(rep("", max(df$index)), max(df$index))) %>%
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
        highcharter::hc_chart(borderColor = "grey", borderWidth = 0.3)

      # Add a title if required
      if(top_plot){
        hc = hc %>%
          highcharter::hc_title(
            text = paste0("<b>", df_select, "</b>"),
            style = list(
              textAlign = "center",
              fontSize = "15px",
              fontFamily = "arial"
            )
          )
      }

      # Add x-axis text if required
      if(bottom_plot){
        hc = hc %>%
          highcharter::hc_xAxis(
            title = list(
              text = paste0("<b>", df$GEOGRAPHY_PARENT[1], " Order</b>"),
              style = list(
                textAlign = "center",
                fontSize = "15px",
                fontFamily = "arial",
                color = "black",
                fontWeight = "bold"
              )
            )
          )
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
        dplyr::filter(GEOGRAPHY_PARENT == "ICB") %>%
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
          GEOGRAPHY_PARENT == "ICB",
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
    geo_table = function(df, df_select, geo_name){

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
            `20/21` = reactable::colDef(width = 60),
            `21/22` = reactable::colDef(width = 60),
            `22/23` = reactable::colDef(width = 60)
          ),
          style = list(fontSize = "14px", fontFamily = "Arial"),
          theme = reactable::reactableTheme(stripedColor = "#f8f8f8")
        )
    }
    
    # Region: select row
    index_region = reactive({
      reactable::getReactableState("region_table", "selected")
    })
    
    # Region: select row
    index_icb = reactive({
      reactable::getReactableState("icb_table", "selected")
    })
    
    # Region: select row
    index_lad = reactive({
      reactable::getReactableState("lad_table", "selected")
    })

    # Region: Initial table
    output$region_table = reactable::renderReactable({

      # Ensure select input required
      req(input$input_region_bnf_child)
      req(input$input_region_bnf_parent)
      req(input$input_region_metric)

      # Plot table
      geo_table(region_df(), index_region(), "Region")
    })

    # Icb: Initial table
    output$icb_table = reactable::renderReactable({

      # Ensure select input required
      req(input$input_icb_bnf_child)
      req(input$input_icb_bnf_parent)
      req(input$input_icb_metric)

      # Plot table
      geo_table(icb_df(), index_icb(), "ICB")
    })

    # LA: Initial table
    output$lad_table = reactable::renderReactable({

      # Ensure select input required
      req(input$input_lad_bnf_child)
      req(input$input_lad_bnf_parent)
      req(input$input_lad_metric)

      # Plot table
      geo_table(lad_df(), index_lad(), "Local Authority")
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

    # RHS: 3 charts ------------------------------------------------------------

    # Region charts
    output$region_chart_one = highcharter::renderHighchart({
      req(index_region())
      spline_chart_plot(region_df(), selected_region(), "20/21", top_plot = TRUE)
    })

    output$region_chart_two = highcharter::renderHighchart({
      req(index_region())
      spline_chart_plot(region_df(), selected_region(), "21/22")
    })

    output$region_chart_three = highcharter::renderHighchart({
      req(index_region())
      spline_chart_plot(region_df(), selected_region(), "22/23", bottom_plot = TRUE)
    })

    # Icb charts
    output$icb_chart_one = highcharter::renderHighchart({
      req(index_icb())
      spline_chart_plot(icb_df(), selected_icb(), "20/21", top_plot = TRUE)
    })

    output$icb_chart_two = highcharter::renderHighchart({
      req(index_icb())
      spline_chart_plot(icb_df(), selected_icb(), "21/22")
    })

    output$icb_chart_three = highcharter::renderHighchart({
      req(index_icb())
      spline_chart_plot(icb_df(), selected_icb(), "22/23", bottom_plot = TRUE)
    })

    # Lad charts
    output$lad_chart_one = highcharter::renderHighchart({
      req(index_lad())
      spline_chart_plot(lad_df(), selected_lad(), "20/21", top_plot = TRUE)
    })

    output$lad_chart_two = highcharter::renderHighchart({
      req(index_lad())
      spline_chart_plot(lad_df(), selected_lad(), "21/22")
    })

    output$lad_chart_three = highcharter::renderHighchart({
      req(index_lad())
      spline_chart_plot(lad_df(), selected_lad(), "22/23", bottom_plot = TRUE)
    })
    
    # Downloads ----------------------------------------------------------------
    
    # Region: download
    mod_nhs_download_server(
      id = "download_region_table",
      filename = "region_drug_data.csv",
      export_data = carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(GEOGRAPHY_PARENT == "Region") %>% 
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE')
      )
    
    # ICB: download
    mod_nhs_download_server(
      id = "download_icb_table",
      filename = "icb_drug_data.csv",
      export_data = carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(GEOGRAPHY_PARENT == "ICB") %>% 
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE')
    )
    
    # LA: download
    mod_nhs_download_server(
      id = "download_lad_table",
      filename = "lad_drug_data.csv",
      export_data = carehomes2::mod_geo_ch_flag_drug_df %>%
        dplyr::filter(GEOGRAPHY_PARENT == "Local Authority") %>% 
        tidyr::pivot_wider(names_from = 'FY', values_from = 'VALUE')
    )
    
  })
}

## To be copied in the UI
# mod_07_geo_ch_flag_drug_ui("geo_ch_flag_drug")

## To be copied in the server
# mod_07_geo_ch_flag_drug_server("geo_ch_flag_drug")
