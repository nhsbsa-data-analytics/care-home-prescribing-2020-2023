mod_05_metrics_age_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2_tabstop("Title"),
    h3_tabstop("Subtitle"),
    p("Paragraph text…"),
    nhs_card(
      heading = "Estimated prescribing metrics by age band and gender for care
                 home patients aged 65 years and over in England",
      nhs_grid_2_col(
        nhs_selectInput(inputId = ns("fy"),
                        label = "Financial year",
                        choices = levels(carehomes2::metrics_by_age_gender_and_ch_flag_df$FY),
                        selected = levels(carehomes2::metrics_by_age_gender_and_ch_flag_df$FY) |> max(),
                        full_width = T),
        nhs_selectInput(inputId = ns("gender_and_age_band_and_ch_flag_metric"),
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
                        full_width = T)
        ),
      highcharter::highchartOutput(outputId = ns("metrics_by_gender_and_age_band_and_ch_flag_chart"), height = "350px"),
      shiny::htmlOutput(outputId = ns("pct_excluded_patients")),
      mod_nhs_download_ui(id = ns("download_data"))
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

      t <- carehomes2::metrics_by_age_gender_and_ch_flag_df |> 
        dplyr::group_by(FY) |>
         # Re-visit this when we have a final decision on SDC
        dplyr::summarise(
          EXCLUDED_PATIENTS = sum(ifelse(is.na(GENDER),# | is.na(SDC_TOTAL_PATIENTS),
                                         TOTAL_PM, # TOTAL_PATIENTS,
                                         0)),
          TOTAL_PATIENTS = sum(TOTAL_PM), #  sum(TOTAL_PATIENTS),
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
    
    # Create download data
    create_download_data <- function(data) {
      data  %>% 
        dplyr::filter(!is.na(GENDER))  %>% 
        dplyr::mutate(
          CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non-care home")
        )  %>% 
        dplyr::arrange(
          .data$FY,
          .data$GENDER,
          .data$AGE_BAND,
          .data$CH_FLAG)  %>% 
        dplyr::select(
          `Financial year` = .data$FY,
          Gender = .data$GENDER,
          `Age band` = .data$AGE_BAND,
          `Care home status` = .data$CH_FLAG,
          `Mean drug cost PPM` = .data$COST_PPM,
          `Mean prescription items PPM` = .data$ITEMS_PPM,
          `Mean unique medicines PPM` = .data$UNIQ_MEDS_PPM,
          `% of patient-months with 6+ unique medicines` = .data$PCT_PM_GTE_SIX,
          `% of patient-months with 10+ unique medicines` = .data$PCT_PM_GTE_TEN,
          `% of patient-months with 2+ ACB medicines` = .data$PCT_PM_ACB,
          `% of patient-months with 2+ DAMN medicines` = .data$PCT_PM_DAMN,
          `Mean unique falls risk medicines PPM` = .data$UNIQ_MEDS_FALLS_PPM,
          `% of patient-months with 3+ falls risk medicines` = .data$PCT_PM_FALLS
        )
    }
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "Selected prescribing metrics by demographic.xlsx",
      export_data = create_download_data(carehomes2::metrics_by_age_gender_and_ch_flag_df)
    )
    
    # Define colours outside the chart
    ch_col = NHSRtheme::get_nhs_colours()["Orange"]
    non_ch_col = NHSRtheme::get_nhs_colours()["DarkBlue"]

    # Define icons outside the chart
    symbol_scaling = 1.4
    
    female_ch <- fa_to_png_to_datauri(name = "venus", width = 9 * symbol_scaling, fill = ch_col)
    female_non_ch <- fa_to_png_to_datauri(name = "venus", width = 9 * symbol_scaling, fill = non_ch_col)
    male_ch <- fa_to_png_to_datauri(name = "mars", width = 11 * symbol_scaling, fill = ch_col)
    male_non_ch <- fa_to_png_to_datauri(name = "mars", width = 11 * symbol_scaling, fill = non_ch_col)
    
    # Max on Y-axis per metric
    max_values_per_metric <- carehomes2::metrics_by_age_gender_and_ch_flag_df |>
      dplyr::summarise(
        dplyr::across(dplyr::ends_with("_PPM") | dplyr::starts_with("PCT_"), max)
      ) |>
      tidyr::pivot_longer(everything(), names_to = "metric")
    
    # Create the chart
    output$metrics_by_gender_and_age_band_and_ch_flag_chart <- highcharter::renderHighchart({
        req(input$gender_and_age_band_and_ch_flag_metric)
        req(input$fy)
        
        highcharter::highchart() |>
      
          highcharter::hc_add_series(
            data = carehomes2::metrics_by_age_gender_and_ch_flag_df |>
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
            data = carehomes2::metrics_by_age_gender_and_ch_flag_df |>
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
            data = carehomes2::metrics_by_age_gender_and_ch_flag_df |>
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
            data = carehomes2::metrics_by_age_gender_and_ch_flag_df |>
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
      
        nhsbsaR::theme_nhsbsa_highchart(stack = NA) |>
      
        highcharter::hc_yAxis(
          min = 0,
          max = max_values_per_metric |>
                dplyr::filter(metric == input$gender_and_age_band_and_ch_flag_metric) |>
                dplyr::pull(value) * 1.1,
          title = list(
          text = paste(
          switch(input$gender_and_age_band_and_ch_flag_metric,
                 "COST_PPM" = "Mean drug cost PPM",
                 "ITEMS_PPM" = "Mean prescription items PPM",
                 "UNIQ_MEDS_PPM" = "Mean unique medicines PPM",
                 "PCT_PM_GTE_SIX" = "% of patient-months with 6+ unique medicines",
                 "PCT_PM_GTE_TEN" = "% of patient-months with 10+ unique medicines",
                 "PCT_PM_ACB" = "% of patient-months with 2+ ACB medicines",
                 "PCT_PM_DAMN" = "% of patient-months with 2+ DAMN medicines",
                 "UNIQ_MEDS_FALLS_PPM" = "Mean unique falls risk medicines PPM",
                 "PCT_PM_FALLS" = "% of patient-months with 3+ falls risk medicines"
                  )
                )
              )
            ) |>
      
        highcharter::hc_xAxis(
          title = list(text = "Patient age band"),
          categories = unique(carehomes2::metrics_by_age_gender_and_ch_flag_df$AGE_BAND)
          ) |>
      
        highcharter::hc_tooltip(
          shared = T,
          useHTML = T,
          valueDecimals = switch(input$gender_and_age_band_and_ch_flag_metric,
                                 "COST_PPM" = 0,
                                 "ITEMS_PPM" = 2,
                                 "UNIQ_MEDS_PPM" = 2,
                                 "PCT_PM_GTE_SIX" = 2,
                                 "PCT_PM_GTE_TEN" = 2,
                                 "PCT_PM_ACB" = 2,
                                 "PCT_PM_DAMN" = 2,
                                 "UNIQ_MEDS_FALLS_PPM" = 2,
                                 "PCT_PM_FALLS" = 2
                                 ),
          headerFormat = "<b> {point.value:.1f} </b>",
          valueSuffix = switch(input$gender_and_age_band_and_ch_flag_metric,
                              "PCT_PM_GTE_SIX" = "%",
                              "PCT_PM_GTE_TEN" = "%",
                              "PCT_PM_ACB" = "%",
                              "PCT_PM_DAMN" = "%",
                              "PCT_PM_FALLS" = "%"),
          valuePrefix = switch(input$gender_and_age_band_and_ch_flag_metric,
                              "COST_PPM" = "£")
          ) |>
      
        highcharter::hc_legend(
          squareSymbol = T,
          symbolWidth = 0.1, # Hide a line through symbols
          itemStyle = list(textDecoration = "none"),
          symbolPadding = 10,
          itemDistance = 30
        )
    
        })

  })
}

## To be copied in the UI
# mod_05_metrics_age_gender_ui("patients_age_gender")

## To be copied in the server
# mod_05_metrics_age_gender_server("patients_age_gender")