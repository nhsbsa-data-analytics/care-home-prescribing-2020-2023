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
    includeMarkdown(app_sys("app", "www", "markdown", "02_patients_age_gender.rmd")),
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
                        choices = names(geographies),
                        full_width = T),
        nhs_selectInput(inputId = ns("sub_geography"),
                        label = "Sub Geography",
                        choices = NULL, # dynamically generated
                        full_width = T)
      ),
      highcharter::highchartOutput(outputId = ns("patients_by_fy_geo_age_gender_chart"), height = "350px"),
      shiny::htmlOutput(outputId = ns("pct_excluded_patients")),
      mod_nhs_download_ui(id = ns("patients_by_geo_age_gender_at_specific_fy_and_subgeo_download"))
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
    pct_excluded_patients <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)
      
      t <- carehomes2::patients_by_fy_geo_age_gender_df |> 
      dplyr::group_by(FY, GEOGRAPHY, SUB_GEOGRAPHY_NAME) |>
      dplyr::summarise(
        EXCLUDED_PATIENTS = sum(ifelse(is.na(GENDER), TOTAL_PATIENTS, 0)),
        TOTAL_PATIENTS = sum(TOTAL_PATIENTS),
        .groups = "drop"
      ) |>
      dplyr::mutate(PCT_EXCLUDED_PATIENTS = (EXCLUDED_PATIENTS/TOTAL_PATIENTS*100) |> janitor::round_half_up(1)) |>
      # Extract % for the selected sub-geography
      dplyr::filter(
        FY == input$fy,
        GEOGRAPHY == input$geography,
        SUB_GEOGRAPHY_NAME == input$sub_geography
      ) |>
      dplyr::pull(PCT_EXCLUDED_PATIENTS)
      
      if (t < 1 & t > 0) "less than 1" else t
      
    })
    
    output$pct_excluded_patients <- renderUI({
      
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt;",

          paste0("This excludes ",
                 pct_excluded_patients(),
                 "% of patients where the gender was unknown.")

      )
      
    })
    
    
    # Patients by geography and gender and age band chart
    
    # Filter to relevant data for this chart
    patients_by_fy_geo_age_gender_df <-
      carehomes2::patients_by_fy_geo_age_gender_df %>%
      dplyr::filter(!is.na(GENDER))
     
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
        dplyr::summarise(max(TOTAL_PATIENTS, na.rm = TRUE)) %>%
        dplyr::pull()
    })

    # # Pull the total
    total <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      patients_by_geo_age_gender_at_specific_fy_and_subgeo_df() %>%
        dplyr::summarise(TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = TRUE)) %>%
        dplyr::mutate(TOTAL_PATIENTS = format(TOTAL_PATIENTS, big.mark = ",")) %>%
        dplyr::pull(TOTAL_PATIENTS)
    })

    # Pull percentage of female patients
    percentage_female_patients <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)

      # Get the total female patients
      female_patients_df <-
        patients_by_geo_age_gender_at_specific_fy_and_subgeo_df() %>%
        dplyr::summarise(
          TOTAL_FEMALE_PATIENTS =
            sum(ifelse(!is.na(GENDER) & GENDER == "Female", TOTAL_PATIENTS, 0)),
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

    patients_by_geo_age_gender_at_specific_fy_and_subgeo_download_df <- reactive({
      req(input$fy)
      req(input$geography)
      req(input$sub_geography)
      
      # Download entire df with all FYs and geo levels
      carehomes2::patients_by_fy_geo_age_gender_df %>% 
        dplyr::rename(
          `Financial year` = FY,
          Geography = GEOGRAPHY,
          `Sub geography` = SUB_GEOGRAPHY_CODE,
          `Sub geography name` = SUB_GEOGRAPHY_NAME,
          `Age band` = AGE_BAND,
          Gender = GENDER,
          `Number of patients` = TOTAL_PATIENTS,
          `Percentage of patients` = PCT_PATIENTS
        )
    })

    # Add a download button
    mod_nhs_download_server(
      id = "patients_by_geo_age_gender_at_specific_fy_and_subgeo_download",
      filename = "patients_by_geography_and_gender_and_age_band_chart.csv",
      export_data = patients_by_geo_age_gender_at_specific_fy_and_subgeo_download_df
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
        )
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
        patients_by_fy_geo_age_gender_plot_df() %>%
          highcharter::hchart(
            type = "bar",
            highcharter::hcaes(
              x = AGE_BAND,
              y = TOTAL_PATIENTS,
              group = GENDER
            )
          ) %>%
          nhsbsaR::theme_nhsbsa_highchart() %>%
          highcharter::hc_colors(colors = c(
            NHSRtheme::get_nhs_colours("Orange") |> unname(),
            NHSRtheme::get_nhs_colours("DarkBlue") |> unname()
            
          )) %>%
          highcharter::hc_annotations(
            list(
              labels = list(
                list(
                  point = list(
                    x = 0,
                    # Need -1 otherwise it fails when max_value() is axis max
                    y = max_value() - 1,
                    xAxis = 0,
                    yAxis = 0
                  ),
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
            title = list(text = "Number of patients"),
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
                  '<b>Number of patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.y), 0) + '<br>' +
                  '<b>Percentage of patients: </b>' + Highcharts.numberFormat(Math.abs(this.point.PCT_PATIENTS), 1) + '%'

                return outHTML

              }
              "
            )
          )
      })
 
  })
}
    
## To be copied in the UI
# mod_02_patients_age_gender_ui("patients_age_gender")
    
## To be copied in the server
# mod_02_patients_age_gender_server("patients_age_gender")
