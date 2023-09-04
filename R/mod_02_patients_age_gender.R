#' patients_age_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_patients_age_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/markdown/02_patients_age_gender.md"),
    nhs_card(
      heading = "Age band and gender of care home patients aged 65 years and over in England",
      nhs_grid_3_col(
        nhs_selectInput(inputId = ns("fy"),
                        label = "Financial year",
                        choices = levels(carehomes2::patients_by_fy_geo_age_gender_df$FY),
                        selected = levels(carehomes2::patients_by_fy_geo_age_gender_df$FY) |> max(),
                        full_width = T),
        nhs_selectInput(inputId = ns("geography"),
                        label = "Geography",
                        choices = levels(carehomes2::patients_by_fy_geo_age_gender_df$GEOGRAPHY),
                        full_width = T),
        nhs_selectInput(inputId = ns("sub_geography"),
                        label = "Sub Geography",
                        choices = NULL, # dynamically generated
                        full_width = T)
      ),
      highcharter::highchartOutput(outputId = ns("patients_by_fy_geo_age_gender_chart"), height = "350px"),
      shiny::htmlOutput(outputId = ns("excluded_patients")),
      mod_nhs_download_ui(id = ns("download_data"))
    ),
    tags$div(style = "margin-top: 25vh") # Some buffer space after the chart
    
  )
}
    
#' patients_age_gender Server Functions
#'
#' @noRd 
mod_02_patients_age_gender_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # % of excluded patients for the chart label from source df that didn't exclude them yet
    # In these version, exclusion is reported for all patients, ch & non-ch
    excluded_patients <- carehomes2::patients_by_fy_geo_age_gender_df |> 
      dplyr::group_by(FY, GEOGRAPHY, SUB_GEOGRAPHY_NAME) |>
      dplyr::summarise(
        EXCLUDED_UNK = sum(ifelse(GENDER=="Unknown", TOTAL_PATIENTS, 0)),
        EXCLUDED_IND = sum(ifelse(GENDER=="Indeterminate", TOTAL_PATIENTS, 0)),
        TOTAL_PATIENTS = sum(TOTAL_PATIENTS),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        PCT_EXCLUDED_UNK = (EXCLUDED_UNK/TOTAL_PATIENTS*100) |> janitor::round_half_up(1),
        PCT_EXCLUDED_IND = (EXCLUDED_IND/TOTAL_PATIENTS*100) |> janitor::round_half_up(1)
      )
    
    excluded_unk <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      t <- excluded_patients |>
      # Extract % for the selected sub-geography
      dplyr::filter(
        FY == input$fy,
        GEOGRAPHY == input$geography,
        SUB_GEOGRAPHY_NAME == input$sub_geography
      ) |>
      dplyr::pull(PCT_EXCLUDED_UNK)

      if (t < 0.1 & t > 0) "less than 0.1" else as.character(t)

    })

    excluded_ind <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      t <- excluded_patients |>
        # Extract % for the selected sub-geography
        dplyr::filter(
          FY == input$fy,
          GEOGRAPHY == input$geography,
          SUB_GEOGRAPHY_NAME == input$sub_geography
        ) |>
        dplyr::pull(PCT_EXCLUDED_IND)

      if (t < 0.1 & t > 0) "less than 0.1" else as.character(t)

    })
    

    output$excluded_patients <- renderUI({
      
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt;",

        paste0(
          
          # Example to test a caption that shows both unk and ind in the caption: 2022/23, LA = Hinckley and Bosworth

          if (stringr::str_extract(excluded_unk(), "[\\d\\.]+") != 0 & stringr::str_extract(excluded_ind(), "[\\d\\.]+") != 0) {
            
            paste0("This chart does not show ",
            excluded_unk(), "%",
            " and ",
            excluded_ind(), "%",
            " patients where the gender was not known and not specified, respectively.")
            
          } else if (stringr::str_extract(excluded_unk(), "[\\d\\.]+") != 0 & stringr::str_extract(excluded_ind(), "[\\d\\.]+") == 0) {
            
            paste0("This chart does not show ",
                   excluded_unk(), "%",
                   " patients where the gender was not known.")
            
          } else if (stringr::str_extract(excluded_unk(), "[\\d\\.]+") == 0 & stringr::str_extract(excluded_ind(), "[\\d\\.]+") != 0) {
            
            paste0("This chart does not show ",
                   excluded_ind(), "%",
                   " patients where the gender was not specified.")
            
          } else NULL,
          
          " In each age band, patient counts of ≤5 were rounded up to the nearest 5, otherwise to the nearest 10; and the percentages are based on rounded counts. Hollow bars show percentages of non-care home patients."
              
        )
      )
      
    })
    
    
    # Patients by geography and gender and age band chart
    
    # Filter to relevant data for this chart
    patients_by_fy_geo_age_gender_df <-
      carehomes2::patients_by_fy_geo_age_gender_df %>%
      dplyr::filter(GENDER %in% c("Male","Female"))
     
    # Filter the data based on the FY and the geography
    patients_by_geo_age_gender_at_specific_fy_and_geo_df <- reactive({
      req(input$geography)

      patients_by_fy_geo_age_gender_df %>%
        dplyr::filter(
          FY == input$fy,
          GEOGRAPHY == input$geography
          )
    })

    # Update the list of choices for sub geography from the non NA rows in the
    # geography dataframe
    observeEvent(
      eventExpr = patients_by_geo_age_gender_at_specific_fy_and_geo_df(),
      handlerExpr = {
        freezeReactiveValue(input, "sub_geography")
        updateSelectInput(
          inputId = "sub_geography",
          choices =
            patients_by_geo_age_gender_at_specific_fy_and_geo_df()$SUB_GEOGRAPHY_NAME %>%
            stats::na.omit() %>%
            unique()
        )
      }
    )

    # Filter the data based on the sub geography
    patients_by_geo_age_gender_at_specific_fy_and_subgeo_df <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      patients_by_geo_age_gender_at_specific_fy_and_geo_df() %>%
        dplyr::filter(
          SUB_GEOGRAPHY_NAME == input$sub_geography
          )
    })

    # Pull the max value
    max_value <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      patients_by_geo_age_gender_at_specific_fy_and_subgeo_df() %>%
        dplyr::summarise(max(PCT_PATIENTS, na.rm = TRUE)) %>%
        dplyr::pull()
    })

    # # Pull the total CH patients
    total <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      patients_by_geo_age_gender_at_specific_fy_and_subgeo_df() %>%
        dplyr::filter(CH_FLAG==1) %>%
        dplyr::summarise(TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = TRUE)) %>%
        dplyr::mutate(TOTAL_PATIENTS = format(TOTAL_PATIENTS, big.mark = ",")) %>%
        dplyr::pull(TOTAL_PATIENTS)
    })

    # Pull percentage of female CH patients
    percentage_female_patients <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      # Get the total female patients
      female_patients_df <-
        patients_by_geo_age_gender_at_specific_fy_and_subgeo_df() %>%
        dplyr::filter(CH_FLAG==1) %>%
        dplyr::summarise(
          TOTAL_FEMALE_PATIENTS =
            sum(ifelse(GENDER == "Female", TOTAL_PATIENTS, 0)),
          TOTAL_PATIENTS = sum(TOTAL_PATIENTS)
        )

      # Calculate the percentage of patients
      female_patients_df <- female_patients_df %>%
        dplyr::mutate(
          PCT_FEMALE_PATIENTS = as.character(janitor::round_half_up(TOTAL_FEMALE_PATIENTS / TOTAL_PATIENTS * 100, 1))
        ) %>%
        dplyr::pull(PCT_FEMALE_PATIENTS)

    })

    # Pull percentage of elderly female patients
    percentage_elderly_female_patients <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      # Get the total elderly female patients
      elderly_female_patients_df <-
        patients_by_geo_age_gender_at_specific_fy_and_subgeo_df() %>%
        dplyr::filter(CH_FLAG==1) %>%
        dplyr::summarise(
          TOTAL_ELDERLY_FEMALE_PATIENTS = sum(
            ifelse(
              test = GENDER == "Female" & AGE_BAND %in% c("85-89", "90+"),
              yes = TOTAL_PATIENTS,
              no = 0
            )
          ),
          TOTAL_PATIENTS = sum(TOTAL_PATIENTS)
        )

      # Calculate the percentage of patients
      elderly_female_patients_df <- elderly_female_patients_df %>%
        dplyr::mutate(
          PCT_ELDERLY_FEMALE_PATIENTS = janitor::round_half_up(TOTAL_ELDERLY_FEMALE_PATIENTS / TOTAL_PATIENTS * 100, 1)
        ) %>%
        dplyr::pull(PCT_ELDERLY_FEMALE_PATIENTS)

    })

    # Create download data
    create_download_data <- function(data) {
      data |>
        dplyr::filter(
          .data$GENDER %in% c("Male", "Female")
        ) |>
        dplyr::arrange(
          .data$FY,
          .data$GEOGRAPHY,
          .data$SUB_GEOGRAPHY_NAME,
          .data$GENDER,
          .data$AGE_BAND
        ) |>
        dplyr::rename(
          `Financial year` = .data$FY,
          Geography = .data$GEOGRAPHY,
          `Sub geography code` = .data$SUB_GEOGRAPHY_CODE,
          `Sub geography name` = .data$SUB_GEOGRAPHY_NAME,
          `Age band` = .data$AGE_BAND,
          Gender = .data$GENDER,
          `Number of care home patients` = .data$TOTAL_PATIENTS,
          `% of care home patients` = .data$PCT_PATIENTS
        )
    }
    
    # Download button
    mod_nhs_download_server(
      id = "download_data",
      filename = "Demographics of care home prescribing.xlsx",
      export_data = create_download_data(
        carehomes2::patients_by_fy_geo_age_gender_df
      ),
      number_xl_fmt_str = "#,##0",
      percent_xl_fmt_str = "#0.0%"
    )

    # Filter out unknown genders for the plot and format
    patients_by_fy_geo_age_gender_plot_df <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      patients_by_geo_age_gender_at_specific_fy_and_subgeo_df() %>%
        # Negate male values so the butterfly chart works
        dplyr::mutate(
          TOTAL_PATIENTS = TOTAL_PATIENTS * ifelse(GENDER == "Male", 1, -1),
          PCT_PATIENTS = PCT_PATIENTS * ifelse(GENDER == "Male", 1, -1)
        ) %>%
        dplyr::mutate(CH_FLAG = ifelse(CH_FLAG==1, "CH", "NCH")) %>%
        tidyr::pivot_wider(names_from = CH_FLAG, values_from = c(TOTAL_PATIENTS, PCT_PATIENTS))
    })
    
    # Pyramid plot for age band and gender
    output$patients_by_fy_geo_age_gender_chart <-
      highcharter::renderHighchart({
        req(input$fy)
        req(input$geography)
        req(input$sub_geography)
        
        # Process annotation
        text <- paste0(
          ifelse(input$sub_geography == "Overall", "", "In "),
          input$sub_geography, ", there were an estimated ", tags$b(total()),
          " care home patients in ", input$fy, ", of which ",
          tags$b(paste0(percentage_female_patients(), "%")), " were females and ",
          tags$b(paste0(percentage_elderly_female_patients(), "%")), " were",
          " females aged 85 or over."
        )
        
        # Create the chart
        highcharter::highchart() %>%
        highcharter::hc_add_series(
          patients_by_fy_geo_age_gender_plot_df(),
          "bar",
          highcharter::hcaes(AGE_BAND, PCT_PATIENTS_CH, group = GENDER),
          pointWidth = 24
        ) %>%
        highcharter::hc_add_series(
          patients_by_fy_geo_age_gender_plot_df(),
          "column",
          highcharter::hcaes(AGE_BAND, PCT_PATIENTS_NCH, group = GENDER),
          color = "rgba(0, 0, 0, 0)",
          pointWidth = 28,
          opacity = 0.9,
          borderWidth = 1,
          borderColor = "#000000",
          showInLegend = F
        ) %>%
        nhsbsaR::theme_nhsbsa_highchart() %>%
        highcharter::hc_colors(colors = c(
          NHSRtheme::get_nhs_colours("Orange") |> unname(),
          NHSRtheme::get_nhs_colours("DarkBlue") |> unname()
          )
        ) %>%
        highcharter::hc_annotations(
            list(
              labels = list(
                list(
                  point = list(
                    x = 0,
                    # Need -1 otherwise it fails when max_value() is axis max
                    y = max_value() - 0.1,
                    xAxis = 0,
                    yAxis = 0
                  ),
                  padding = 0,
                  x = 20, # Offset
                  text = text,
                  style = list(
                    width = 150,
                    fontSize = "9pt"
                  )
                )
              ),
              labelOptions = list(
                backgroundColor = "#FFFFFF",
                borderWidth = 0,
                align = "right",
                verticalAlign = "top",
                useHTML = TRUE
              )
            )
          ) %>%
          highcharter::hc_xAxis(
            title = list(text = "Age band"),
            categories =
              patients_by_fy_geo_age_gender_plot_df()$AGE_BAND %>%
              unique() %>%
              sort(),
            reversed = FALSE
          ) %>%
          highcharter::hc_yAxis(
            title = list(text = "Proportion of patients (%)"),
            min = -max_value(),
            max = max_value(),
            labels = list(
              formatter = highcharter::JS(
                "
                function() {

                  outHTML = this.axis.defaultLabelFormatter.call(this)

                  return outHTML.replace('-', '')

                }
                "
              )
            )
          ) %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            useHTML = TRUE,
            formatter = htmlwidgets::JS(
              "
              function() {

                outHTML =
                  '<b>Gender: </b>' + this.series.name + '<br>' +
                  '<b>Age band: </b>' + this.point.category + '<br/>' +
                  '<b>Number of care home patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.TOTAL_PATIENTS_CH), 0) + '<br>' +
                  '<b>Percentage of care home patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.PCT_PATIENTS_CH), 1) + '%' + '<br>' +
                  '<b>Percentage of non-care home patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.PCT_PATIENTS_NCH), 1) + '%'

                return outHTML

              }
              "
            )
          ) %>%
        highcharter::hc_plotOptions(
          series = list(
            states = list(
              #Disable series highlighting
              inactive = list(opacity = 1)
              ), 
            events = list(
              # Disables turning the series off
              legendItemClick = htmlwidgets::JS("function () { return false; }")
              ) 
        )
        )
      })
 
  })
}
    
## To be copied in the UI
# mod_02_patients_age_gender_ui("patients_age_gender")
    
## To be copied in the server
# mod_02_patients_age_gender_server("patients_age_gender")
