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
                          "Drug cost (MPMM)" = "SDC_COST_MPMM",
                          "Number of prescription items (MPMM)" = "SDC_ITEMS_MPMM",
                          "Number of unique medicines (MPMM)" = "SDC_UNIQUE_MEDICINES_MPMM",
                          "Months with 6+ unique medicines (MPP, %)" = "SDC_PCT_PATIENTS_6_PLUS_MPP",
                          "Months with 10+ unique medicines (MPP, %)" = "SDC_PCT_PATIENTS_10_PLUS_MPP",
                          "Months with ACB of 6+ (MPP, %)" = "SDC_PCT_PATIENTS_ACB_6_MPP",
                          "Months with 2+ DAMN medicines (MPP, %)" = "SDC_PCT_PATIENTS_DAMN_MPP"
                          ),
                        full_width = T)
        ),
      highcharter::highchartOutput(outputId = ns("metrics_by_gender_and_age_band_and_ch_flag_chart"), height = "350px"),
      shiny::htmlOutput(outputId = ns("pct_excluded_patients")),
      mod_nhs_download_ui(id = ns("download_metrics_by_gender_and_age_band_and_ch_flag_chart"))
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
    
    
    
    # Swap NAs for "c" for data download and tidy the table
    metrics_by_gender_and_age_band_and_ch_flag_download_df <- 
      
      metrics_by_age_gender_and_ch_flag_df |> # Download entire df with all FYs
      dplyr::filter(!is.na(GENDER)) |>
      dplyr::mutate(
        SDC_COST_MPMM = ifelse(
          test = is.na(SDC_COST_MPMM),
          yes = "c",
          no = as.character(SDC_COST_MPMM)
        ),
        SDC_ITEMS_MPMM = ifelse(
          test = is.na(SDC_ITEMS_MPMM),
          yes = "c",
          no = as.character(SDC_ITEMS_MPMM)
        ),
        SDC_UNIQUE_MEDICINES_MPMM = ifelse(
          test = is.na(SDC_UNIQUE_MEDICINES_MPMM),
          yes = "c",
          no = as.character(SDC_UNIQUE_MEDICINES_MPMM)
        ),
        SDC_PCT_PATIENTS_6_PLUS_MPP = ifelse(
          test = is.na(SDC_PCT_PATIENTS_6_PLUS_MPP),
          yes = "c",
          no = as.character(SDC_PCT_PATIENTS_6_PLUS_MPP)
        ),
        SDC_PCT_PATIENTS_10_PLUS_MPP = ifelse(
          test = is.na(SDC_PCT_PATIENTS_10_PLUS_MPP),
          yes = "c",
          no = as.character(SDC_PCT_PATIENTS_10_PLUS_MPP)
        ),
        SDC_PCT_PATIENTS_ACB_6_MPP = ifelse(
          test = is.na(SDC_PCT_PATIENTS_ACB_6_MPP),
          yes = "c",
          no = as.character(SDC_PCT_PATIENTS_ACB_6_MPP)
        ),
        SDC_PCT_PATIENTS_DAMN_MPP = ifelse(
          test = is.na(SDC_PCT_PATIENTS_DAMN_MPP),
          yes = "c",
          no = as.character(SDC_PCT_PATIENTS_DAMN_MPP)
        ),
        CH_FLAG = ifelse(CH_FLAG == 1, "Care home", "Non care home")
      ) |>
      dplyr::arrange(FY, GENDER, AGE_BAND, CH_FLAG) |>
       dplyr::select(
        `Financial year` = FY,
         Gender = GENDER,
        `Age band` = AGE_BAND,
        `Care home flag` = CH_FLAG,
        `Drug cost (MPMM)` = SDC_COST_MPMM,
        `Number of prescription items (MPMM)` = SDC_ITEMS_MPMM,
        `Number of unique medicines (MPMM)` = SDC_UNIQUE_MEDICINES_MPMM,
        `Months with 6+ unique medicines (MPP, %)` = SDC_PCT_PATIENTS_6_PLUS_MPP,
        `Months with 10+ unique medicines (MPP, %)` = SDC_PCT_PATIENTS_10_PLUS_MPP,
        `Months with ACB of 6+ (MPP, %)` = SDC_PCT_PATIENTS_ACB_6_MPP,
        `Months with 2+ DAMN medicines (MPP, %)` = SDC_PCT_PATIENTS_DAMN_MPP
      )

    # Add a download button
    mod_nhs_download_server(
      id = "download_metrics_by_gender_and_age_band_and_ch_flag_chart",
      filename = "metrics_by_gender_and_age_band_and_ch_flag.csv",
      export_data = metrics_by_gender_and_age_band_and_ch_flag_download_df
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
    
    # Create the chart
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
                "SDC_COST_MPMM" = "Drug cost (£)",
                "SDC_ITEMS_MPMM" = "Number of prescription items",
                "SDC_UNIQUE_MEDICINES_MPMM" = "Number of unique medicines",
                "SDC_PCT_PATIENTS_6_PLUS_MPP" = "Months with 6+ unique medicines (MPP, %)",
                "SDC_PCT_PATIENTS_10_PLUS_MPP" = "Months with 10+ unique medicines (MPP, %)",
                "SDC_PCT_PATIENTS_ACB_6_MPP" = "Months with ACB of 6+ (MPP, %)",
                "SDC_PCT_PATIENTS_DAMN_MPP" = "Months with 2+ DAMN medicines (MPP, %)"
                  )
                )
              )
            ) |>
      
        highcharter::hc_xAxis(
          title = list(text = "Patient Age Band"),
          categories = unique(metrics_by_age_gender_and_ch_flag_df$AGE_BAND)
          ) |>
      
        highcharter::hc_tooltip(
          shared = T,
          useHTML = T,
          valueDecimals = switch(input$gender_and_age_band_and_ch_flag_metric,
                                 "SDC_COST_MPMM" = 0,
                                 "SDC_ITEMS_MPMM" = 1,
                                 "SDC_UNIQUE_MEDICINES_MPMM" = 1,
                                 "SDC_PCT_PATIENTS_6_PLUS_MPP" = 1,
                                 "SDC_PCT_PATIENTS_10_PLUS_MPP" = 1,
                                 "SDC_PCT_PATIENTS_ACB_6_MPP" = 1,
                                 "SDC_PCT_PATIENTS_DAMN_MPP" = 1
                                 ),
          headerFormat = "<b> {point.value:.1f} </b>",
          valueSuffix = switch(input$gender_and_age_band_and_ch_flag_metric,
                              "SDC_PCT_PATIENTS_6_PLUS_MPP" = "%",
                              "SDC_PCT_PATIENTS_10_PLUS_MPP" = "%",
                              "SDC_PCT_PATIENTS_ACB_6_MPP" = "%",
                              "SDC_PCT_PATIENTS_DAMN_MPP" = "%"),
          valuePrefix = switch(input$gender_and_age_band_and_ch_flag_metric,
                              "SDC_COST_MPMM" = "£")
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