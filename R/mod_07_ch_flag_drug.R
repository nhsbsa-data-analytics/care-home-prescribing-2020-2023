
#' mod 07 national care home drug analysis
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_07_ch_flag_drug_ui <- function(id) {
  ns <- NS(id)
  tagList(
    include_dynamic_md("inst/markdown/07_ch_flag_drug.md"),
    
    nhs_card(
      heading = "National BNF-level prescribing estimates for care home patients
                aged 65 years and over in England",
      
      # 3 column select input
      nhs_grid_3_col(
        
        # Financial year
        nhs_selectInput(
          inputId = ns("input_financial_year"),
          label = "Financial year",
          choices = sort(unique(carehomes2::mod_ch_flag_drug_df$FY)),
          selected = max(sort(unique(carehomes2::mod_ch_flag_drug_df$FY))),
          full_width = T
          ),
        
        # BNF Level
        nhs_selectInput(
          inputId = ns("input_bnf"),
          label = "BNF level",
          choices = unique(carehomes2::mod_ch_flag_drug_df$BNF_PARENT),
          full_width = T
        ),
        
        # Metric
        nhs_selectInput(
          inputId = ns("input_metric"),
          label = "Metric",
          choices = unique(carehomes2::mod_ch_flag_drug_df$METRIC),
          full_width = T
        )
      ),
      
      # Chart output
      highcharter::highchartOutput(
        outputId = ns("ch_flag_drug_chart"), 
        height = "450px"
        ),
      
      # Chart caption
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "For all metrics, only the top 20 drugs or drug groups by total care
        home item count are presented.",
        tags$br(),
        "Values over 1,000 have been shortened with an appropriate suffix and
        then rounded to 1 decimal place.",
        tags$br(),
        "All other values are rounded to 1 decimal place, with values less than 
        0.05 rounded to zero."
      )
    )
  )
}

mod_07_ch_flag_drug_server <- function(id, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Data --------------------------------------------------------------------
    
    # Filter by select inputs
    data = reactive({
      carehomes2::mod_ch_flag_drug_df %>%
        dplyr::mutate(
          VALUE = dplyr::case_when(
            startsWith(METRIC, "Total") ~ bespoke_round(VALUE),
            TRUE ~ janitor::round_half_up(VALUE, 1)
          )
        ) %>%
        dplyr::filter(
          FY == input$input_financial_year,
          BNF_PARENT == input$input_bnf,
          METRIC == input$input_metric
        ) %>%
        dplyr::arrange(CH_FLAG, desc(VALUE))
    })

    # x axis categories (also used to sort data)
    bnf_categories = reactive({
      data() %>%
        dplyr::filter(CH_FLAG == 1) %>%
        dplyr::select(BNF_CHILD) %>%
        dplyr::pull()
    })

    # Ch data
    ch = reactive({
      data() %>%
        dplyr::filter(CH_FLAG == 1) %>%
        dplyr::arrange(factor(BNF_CHILD, levels = bnf_categories()))
    })

    # Non-ch data
    non_ch = reactive({
      data() %>%
        dplyr::filter(CH_FLAG == 0) %>%
        dplyr::arrange(factor(BNF_CHILD, levels = bnf_categories()))
      })

    # Chart --------------------------------------------------------------------

    # Generate highchart
    output$ch_flag_drug_chart = highcharter::renderHighchart({

      highcharter::highchart() %>%
        highcharter::hc_add_series(
          ch(),
          "column",
          name = "Care home",
          highcharter::hcaes(BNF_CHILD, VALUE),
          color = nhsbsaR::palette_nhsbsa()[3],
          pointWidth = 5
        ) %>%
        highcharter::hc_add_series(
          non_ch(),
          "bar",
          name = "Non-care home",
          highcharter::hcaes(BNF_CHILD, VALUE),
          color = "rgba(0, 0, 0, 0)",
          pointWidth = 10,
          opacity = 0.9,
          borderWidth = 0.8,
          borderColor = nhsbsaR::palette_nhsbsa()[1]
        ) %>%
        highcharter::hc_xAxis(
          categories = bnf_categories(),
          title = list(
            text = input$input_bnf
            )
          ) %>%
        highcharter::hc_yAxis(
          min = 0,
          title = list(
            text = input$input_metric
          ),
          labels = list(
            formatter = htmlwidgets::JS("
              function() {
                if(this.value >= 10**9) {
                    return (this.value / 10**9) + 'B';
                }
                else if(this.value >= 10**6) {
                    return (this.value / 10**6) + 'M';
                }
                else if(this.value >= 10**3) {
                    return (this.value / 10**3) + 'K';
                }
                else {
                    return this.value;
                }
              }
            ")
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          formatter = htmlwidgets::JS("
            function() {
              var fmt_num = function(x) {
                if(x >= 10**9){
                    return Highcharts.numberFormat(x / 10**9, 1) + 'B';
                }
                else if(x >= 10**6) {
                    return Highcharts.numberFormat(x / 10**6, 1) + 'M';
                }
                else if(x >= 10**3) {
                    return Highcharts.numberFormat(x / 10**3, 1) + 'K';
                }
                else {
                    return Highcharts.numberFormat(x, 1);
                }
              }
            
              var prefix = function(x) {
                if(!x.startsWith('%') & x.includes('cost')) {
                  return '£';
                }
                else {
                  return '';
                }
              }
            
              var suffix = function(x) {
                if(x.startsWith('%')) {
                    return '%';
                }
                else {
                    return '';
                }
              }
            
              return this.points.reduce(
                function (s, point) {
                  return s + '<br/>' + 
                    point.series.name + ': ' +
                    prefix(point.point.METRIC) + 
                    fmt_num(point.y) + 
                    suffix(point.point.METRIC);
                },
                '<b>' + this.x + '</b>'
              );
            }
            "
          )
        ) %>%
        highcharter::hc_plotOptions(
          series = list(
            states = list(
              inactive = list(enabled = FALSE)
            )
          )
        ) %>%
        highcharter::hc_chart(inverted = T) %>%
        nhsbsaR::theme_nhsbsa_highchart()
    })
    
    # Downloads ----------------------------------------------------------------
    
    # Create download data
    download_data <- carehomes2::mod_ch_flag_drug_df %>%
      dplyr::mutate(
        VALUE = dplyr::case_when(
          startsWith(METRIC, "Total") ~ bespoke_round(VALUE),
          TRUE ~ janitor::round_half_up(VALUE, 1)
        )
      ) %>%
      tidyr::pivot_wider(
        names_from = METRIC,
        values_from = VALUE
      ) %>%
      dplyr::arrange(FY, CH_FLAG, BNF_PARENT, BNF_CHILD) %>%
      dplyr::mutate(CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non-care home")) %>%
      dplyr::rename(
        `Financial year` = FY,
        `Care home` = CH_FLAG,
        `BNF level` = BNF_PARENT,
        `BNF sub-level` = BNF_CHILD
      )
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "National BNF-level prescribing estimates.xlsx",
      export_data = download_data,
      currency_xl_fmt_str = "£#,##0.0"
    )
    
    observeEvent(
      download_data,
      once = TRUE, {
        req(download_data)
        
        insertUI(
          selector = ".nhsuk-card__description:eq(5)",
          where = "beforeEnd",
          ui = mod_nhs_download_ui(ns("download_data"))
        )
      })
  })
}

## To be copied in the UI
# mod_07_ch_flag_drug_ui("ch_flag_drug_ui")

## To be copied in the server
# mod_07_ch_flag_drug_server("ch_flag_drug_server")
