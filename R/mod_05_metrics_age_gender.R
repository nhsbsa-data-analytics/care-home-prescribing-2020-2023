mod_05_metrics_age_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2_tabstop("Title"),
    h3_tabstop("Subtitle"),
    p("Paragraph text…"),
    nhs_card(
      heading = "Estimated average prescribing metrics per patient month for care home and non-care home patients aged 65+ in England by age band and gender",
      nhs_grid_2_col(
        nhs_selectInput(inputId = ns("fy"),
                        label = "Financial year",
                        choices = levels(patients_by_fy_geo_age_gender_df$FY),
                        full_width = T),
        nhs_selectInput(inputId = ns("gender_and_age_band_and_ch_flag_metric"),
                        label = "Metric",
                        choices = c(
                          "Drug cost (PPM)" = "SDC_COST_PER_PATIENT_MONTH",
                          "Number of prescription items (PPM)" = "SDC_ITEMS_PER_PATIENT_MONTH",
                          "Number of unique medicines (PPM)" = "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH"
                          #"Patients on ten or more unique medicines (PPM)" = "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH"
                          ),
                        full_width = T)
        ),
      highcharter::highchartOutput(outputId = ns("metrics_by_gender_and_age_band_and_ch_flag_chart"), height = "350px"),
      shiny::htmlOutput(outputId = ns("pct_excluded_patients")),
      #mod_nhs_download_ui(id = ns("download_metrics_by_gender_and_age_band_and_ch_flag_chart"))
    ),
    tags$div(style = "margin-top: 25vh") # Some buffer space after the chart
    
  )
}

#' metrics_age_gender Server Functions
#'
#' @noRd 
mod_05_metrics_age_gender_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # % of excluded patients for the chart label from source df that didn't exclude them yet
    pct_excluded_patients <- reactive({
      req(input$fy)

      t <- metrics_by_age_gender_and_ch_flag_df |> 
        dplyr::group_by(FY) |>
        dplyr::summarise(
          EXCLUDED_PATIENTS = sum(ifelse(is.na(GENDER) | is.na(SDC_TOTAL_PATIENTS), TOTAL_PATIENTS, 0)),
          TOTAL_PATIENTS = sum(TOTAL_PATIENTS),
          .groups = "drop"
        ) |>
        dplyr::mutate(PCT_EXCLUDED_PATIENTS = (EXCLUDED_PATIENTS/TOTAL_PATIENTS*100) |> janitor::round_half_up(1)) |>
        # Extract % for the selected sub-geography
        dplyr::filter(FY == input$fy) |>
        dplyr::pull(PCT_EXCLUDED_PATIENTS)
      
      if (t < 1) "less than 1" else t
      
    })
    
    output$pct_excluded_patients <- renderUI({
      
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt;",
        
        paste0("This excludes ",
               pct_excluded_patients(),
               "% of patients where the gender was unknown or where statistical disclosure control has been applied.")
      )
      
    })
    
    
    # Define colours outside the chart
    ch_col = NHSRtheme::get_nhs_colours()["Orange"]
    non_ch_col = NHSRtheme::get_nhs_colours()["DarkBlue"]

    symbol_scaling = 1.4
    
    # Manually define the icons
    female_ch <- fa_to_png_to_datauri(name = "venus", width = 9 * symbol_scaling, fill = ch_col)
    female_non_ch <- fa_to_png_to_datauri(name = "venus", width = 9 * symbol_scaling, fill = non_ch_col)
    male_ch <- fa_to_png_to_datauri(name = "mars", width = 11 * symbol_scaling, fill = ch_col)
    male_non_ch <- fa_to_png_to_datauri(name = "mars", width = 11 * symbol_scaling, fill = non_ch_col)
    
    # Create chart
    output$metrics_by_gender_and_age_band_and_ch_flag_chart <- highcharter::renderHighchart({
        req(input$gender_and_age_band_and_ch_flag_metric)
        req(input$fy)
        
        highcharter::highchart() |>
      
          highcharter::hc_add_series(
            data = metrics_by_age_gender_and_ch_flag_df |>
              dplyr::filter(FY == input$fy & CH_FLAG == 1 & GENDER == "Female"),
              type = "line",
              highcharter::hcaes(
            x = AGE_BAND,
            y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
            ),
            name = "Care home - Female",
            color = ch_col,
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = female_ch),
              radius = 2
              ),
            icon = female_ch) |>
      
          highcharter::hc_add_series(
            data = metrics_by_age_gender_and_ch_flag_df |>
              dplyr::filter(FY == input$fy & CH_FLAG == 1 & GENDER == "Male"),
            type = "line",
            highcharter::hcaes(
                x = AGE_BAND,
                y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
              ),
              name = "Care home - Male",
              color = ch_col,
              marker = list(
                symbol = stringr::str_glue("url({data_uri})", data_uri = male_ch),
                radius = 2
              ),
              icon = male_ch) |>
      
          highcharter::hc_add_series(
            data = metrics_by_age_gender_and_ch_flag_df |>
                dplyr::filter(FY == input$fy, CH_FLAG == 0 & GENDER == "Female"),
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
            ),
            name = "Non-care home - Female",
            color = non_ch_col,
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = female_non_ch),
              radius = 2
            ),
            icon = female_non_ch) |>
      
          highcharter::hc_add_series(
            data = metrics_by_age_gender_and_ch_flag_df |>
              dplyr::filter(FY == input$fy & CH_FLAG == 0 & GENDER == "Male"),
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = .data[[input$gender_and_age_band_and_ch_flag_metric]]
            ),
            name = "Non-care home - Male",
            color = non_ch_col,
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = male_non_ch),
              radius = 2
            ),
            icon = male_non_ch) |>
      
        theme_nhsbsa(stack = NA) |>
      
        highcharter::hc_yAxis(
          min = 0,
          title = list(
          text = paste(
          switch(input$gender_and_age_band_and_ch_flag_metric,
                "SDC_COST_PER_PATIENT_MONTH" = "Drug cost (£)",
                "SDC_ITEMS_PER_PATIENT_MONTH" = "Number of prescription items",
                 "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH" = "Number of unique medicines",
                "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH" = "Patients on ten or more unique medicines (%)")
                  )
                )
              ) |>
      
        highcharter::hc_xAxis(
          title = list(text = "Patient Age Band"),
          categories = unique(metrics_by_age_gender_and_ch_flag_df$AGE_BAND)
          ) |>
      
        highcharter::hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          valueDecimals = switch(input$gender_and_age_band_and_ch_flag_metric,
                                "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH" = 1,
                                "SDC_UNIQUE_MEDICINES_PER_PATIENT_MONTH" = 1,
                                "SDC_ITEMS_PER_PATIENT_MONTH" = 1),
          # valueDecimals = 1,
          headerFormat = "<b> {point.value:.1f} </b>",
          valueSuffix = switch(input$gender_and_age_band_and_ch_flag_metric,
                              "SDC_PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH" = "%"),
          valuePrefix = switch(input$gender_and_age_band_and_ch_flag_metric,
                              "SDC_COST_PER_PATIENT_MONTH" = "£")
          ) |>
      
        highcharter::hc_legend(
          squareSymbol = T,
          symbolWidth = 0.1, # Hide line through symbols
          itemStyle = list(textDecoration = "none")
        )
    
        })

  })
}

## To be copied in the UI
# mod_05_metrics_age_gender_ui("patients_age_gender")

## To be copied in the server
# mod_05_metrics_age_gender_server("patients_age_gender")