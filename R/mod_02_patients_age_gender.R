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
    h2_tabstop("Title"),
    h3_tabstop("Subtitle"),
    p("Text…"),
    nhs_card(
      heading = p("Chart heading"),
      nhs_grid_3_col(
        nhs_selectInput(inputId = ns("my_input1"),
                        label = "Year",
                        choices = c("1","2","3"),
                        full_width = T),
        nhs_selectInput(inputId = ns("geography"),
                        label = "Geography",
                        choices = names(geographys),
                        full_width = T),
        nhs_selectInput(inputId = ns("sub_geography"),
                        label = "Sub Geography",
                        choices = NULL, # dynamically generated
                        full_width = T)
      ),
      highcharter::highchartOutput(outputId = ns("my_chart"), height = "400px"),
      mod_nhs_download_ui(id = ns("download_my_chart"))
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
    
    
    # Patients by geography and gender and age band chart
    
    # # Filter to relevant data for this chart
    # patients_by_geography_and_gender_and_age_band_df <-
    #   careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
    #   dplyr::filter(!is.na(GENDER))
    # 
    # # Handy resource: https://mastering-shiny.org/action-dynamic.html
    # 
    # # Filter the data based on the geography
    # patients_by_geography_and_gender_and_age_band_geography_df <- reactive({
    #   req(input$geography)
    #   
    #   careHomePrescribingScrollytellR::patients_by_geography_and_gender_and_age_band_df %>%
    #     dplyr::filter(GEOGRAPHY == input$geography)
    # })
    # 
    # # Update the list of choices for sub geography from the non NA rows in the
    # # geography dataframe
    # observeEvent(
    #   eventExpr = patients_by_geography_and_gender_and_age_band_geography_df(),
    #   handlerExpr = {
    #     freezeReactiveValue(input, "sub_geography")
    #     updateSelectInput(
    #       inputId = "sub_geography",
    #       choices =
    #         patients_by_geography_and_gender_and_age_band_geography_df()$SUB_GEOGRAPHY_NAME %>%
    #         na.omit() %>%
    #         unique()
    #     )
    #   }
    # )
    # 
    # # Filter the data based on the sub geography
    # patients_by_geography_and_gender_and_age_band_sub_geography_df <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   patients_by_geography_and_gender_and_age_band_geography_df() %>%
    #     dplyr::filter(SUB_GEOGRAPHY_NAME == input$sub_geography)
    # })
    # 
    # # Pull the max value
    # max_value <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
    #     dplyr::summarise(max(SDC_TOTAL_PATIENTS, na.rm = TRUE)) %>%
    #     dplyr::pull()
    # })
    # 
    # # Pull the total
    # total <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
    #     dplyr::summarise(TOTAL_PATIENTS = sum(TOTAL_PATIENTS, na.rm = TRUE)) %>%
    #     dplyr::mutate(
    #       SDC_TOTAL_PATIENTS = ifelse(
    #         test = TOTAL_PATIENTS %in% c(1, 2, 3, 4),
    #         yes = "c",
    #         no = format(round(TOTAL_PATIENTS, -1), big.mark = ",")
    #       )
    #     ) %>%
    #     dplyr::pull(SDC_TOTAL_PATIENTS)
    # })
    # 
    # # Pull percentage of female patients
    # percentage_female_patients <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   # Get the total female patients
    #   female_patients_df <-
    #     patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
    #     dplyr::summarise(
    #       TOTAL_FEMALE_PATIENTS =
    #         sum(ifelse(!is.na(GENDER) & GENDER == "Female", TOTAL_PATIENTS, 0)),
    #       TOTAL_PATIENTS = sum(TOTAL_PATIENTS)
    #     )
    #   
    #   # Calculate the percentage of patients
    #   female_patients_df <- female_patients_df %>%
    #     dplyr::mutate(
    #       PCT_FEMALE_PATIENTS = TOTAL_FEMALE_PATIENTS / TOTAL_PATIENTS * 100
    #     )
    #   
    #   # Apply SDC to percentage of female patients
    #   female_patients_df <- female_patients_df %>%
    #     dplyr::mutate(
    #       SDC = ifelse(TOTAL_FEMALE_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    #       SDC_PCT_FEMALE_PATIENTS =
    #         ifelse(
    #           test = SDC == 1,
    #           yes = "c",
    #           no = as.character(janitor::round_half_up(PCT_FEMALE_PATIENTS, 1))
    #         )
    #     )
    #   
    #   # Pull percentage
    #   female_patients_df %>%
    #     dplyr::pull(SDC_PCT_FEMALE_PATIENTS)
    # })
    # 
    # # Pull percentage of elderly female patients
    # percentage_elderly_female_patients <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   # Get the total elderly female patients
    #   elderly_female_patients_df <-
    #     patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
    #     dplyr::summarise(
    #       TOTAL_ELDERLY_FEMALE_PATIENTS = sum(
    #         ifelse(
    #           test = GENDER == "Female" & AGE_BAND %in% c("85-89", "90+"),
    #           yes = TOTAL_PATIENTS,
    #           no = 0
    #         )
    #       ),
    #       TOTAL_PATIENTS = sum(TOTAL_PATIENTS)
    #     )
    #   
    #   # Calculate the percentage of patients
    #   elderly_female_patients_df <- elderly_female_patients_df %>%
    #     dplyr::mutate(
    #       PCT_ELDERLY_FEMALE_PATIENTS =
    #         TOTAL_ELDERLY_FEMALE_PATIENTS / TOTAL_PATIENTS * 100
    #     )
    #   
    #   # Apply SDC to percentage of elderly female patients
    #   elderly_female_patients_df <- elderly_female_patients_df %>%
    #     dplyr::mutate(
    #       SDC = ifelse(TOTAL_ELDERLY_FEMALE_PATIENTS %in% c(1, 2, 3, 4), 1, 0),
    #       SDC_PCT_ELDERLY_FEMALE_PATIENTS =
    #         ifelse(
    #           test = SDC == 1,
    #           yes = "c",
    #           no = as.character(
    #             janitor::round_half_up(PCT_ELDERLY_FEMALE_PATIENTS, 1)
    #           )
    #         )
    #     )
    #   
    #   # Pull percentage
    #   elderly_female_patients_df %>%
    #     dplyr::pull(SDC_PCT_ELDERLY_FEMALE_PATIENTS)
    # })
    # 
    # # Pull the number of NA gender patients
    # patients_with_na_gender <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
    #     dplyr::filter(is.na(GENDER)) %>%
    #     # Format number
    #     dplyr::mutate(
    #       SDC_TOTAL_PATIENTS = ifelse(
    #         test = is.na(SDC_TOTAL_PATIENTS),
    #         yes = "c",
    #         no = as.character(SDC_TOTAL_PATIENTS)
    #       )
    #     ) %>%
    #     dplyr::pull(SDC_TOTAL_PATIENTS)
    # })
    # 
    # # Swap NAs for "c" for data download and subset columns
    # patients_by_geography_and_gender_and_age_band_download_df <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
    #     dplyr::mutate(
    #       SDC_TOTAL_PATIENTS = ifelse(
    #         test = is.na(SDC_TOTAL_PATIENTS),
    #         yes = "c",
    #         no = as.character(SDC_TOTAL_PATIENTS)
    #       ),
    #       SDC_PCT_PATIENTS = ifelse(
    #         test = is.na(SDC_PCT_PATIENTS),
    #         yes = "c",
    #         no = as.character(SDC_PCT_PATIENTS)
    #       )
    #     ) %>%
    #     dplyr::select(-c(TOTAL_PATIENTS, PCT_PATIENTS)) %>%
    #     dplyr::rename(
    #       Geography = GEOGRAPHY,
    #       `Sub geography` = SUB_GEOGRAPHY_CODE,
    #       `Sub geography name` = SUB_GEOGRAPHY_NAME,
    #       `Age band` = AGE_BAND,
    #       Gender = GENDER,
    #       `Number of patients` = SDC_TOTAL_PATIENTS,
    #       `Percentage of patients` = SDC_PCT_PATIENTS
    #     )
    # })
    # 
    # # Filter out unknown genders for the plot and format
    # patients_by_geography_and_gender_and_age_band_plot_df <- reactive({
    #   req(input$geography)
    #   req(input$sub_geography)
    #   
    #   patients_by_geography_and_gender_and_age_band_sub_geography_df() %>%
    #     dplyr::filter(!is.na(GENDER)) %>%
    #     # Negate male values so the butterfly chart works
    #     dplyr::mutate(
    #       SDC_TOTAL_PATIENTS =
    #         SDC_TOTAL_PATIENTS * ifelse(GENDER == "Male", 1, -1),
    #       SDC_PCT_PATIENTS =
    #         SDC_PCT_PATIENTS * ifelse(GENDER == "Male", 1, -1)
    #     )
    # })
 
  })
}
    
## To be copied in the UI
# mod_02_patients_age_gender_ui("patients_age_gender")
    
## To be copied in the server
# mod_02_patients_age_gender_server("patients_age_gender")
