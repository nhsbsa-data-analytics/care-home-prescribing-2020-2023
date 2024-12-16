#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  shinyusertracking::use_logging()
  
  mod_01_headline_figures_server("headline_figures")
  mod_02_patients_age_gender_server("patients_age_gender")
  mod_04_metrics_ch_type_server("metrics_ch_type")
  mod_05_metrics_age_gender_server("metrics_age_gender")
  mod_06_geo_ch_flag_server("geo_ch_flag")
  mod_07_ch_flag_drug_server("ch_flag_drug")
  mod_08_geo_ch_flag_drug_server("geo_ch_flag_drug")
}
