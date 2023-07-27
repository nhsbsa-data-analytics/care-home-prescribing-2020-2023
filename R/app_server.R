#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_01_headline_figures_server("headline_figures")
  mod_02_patients_age_gender_server("patients_age_gender")
  mod_03_patients_imd_server("patients_imd")
  mod_04_metrics_ch_type_server("metrics_ch_type")
  mod_05_metrics_age_gender_server("metrics_age_gender")
  mod_06_geo_ch_flag_server("geo_ch_flag")
  mod_07_ch_flag_drug_server("ch_flag_drug")
  mod_07_geo_ch_flag_drug_server("geo_ch_flag_drug")
  #mod_08_definitions_server("definitions")
  #mod_09_methodology_server("methodology")
  #mod_10_caveats_server("caveats")
}
