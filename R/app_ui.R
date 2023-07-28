#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Need this for accessibility
    tags$html(lang = "en"),
    # Need this for shiny bootstrap dependencies TODO: check if needed
    bootstrapLib(),
    # First level UI elements
    nhs_header(),
    br(),
    tags$div(
      class = "nhsuk-width-container",
      tags$div(
        class = "nhsuk-main-wrapper",
        id = "maincontent",
        role = "main",
        h1("Estimated prescribing patterns for care home patients aged 65 years or over"),
        nhs_navlistPanel(
          well = FALSE,
          widths = c(2, 10),
          tabPanel(
            title = "Article",
            mod_01_headline_figures_ui("headline_figures"),
            mod_02_patients_age_gender_ui("patients_age_gender"),
            mod_03_patients_imd_ui("patients_imd"),
            mod_04_metrics_ch_type_ui("metrics_ch_type"),
            mod_05_metrics_age_gender_ui("metrics_age_gender"),
            mod_06_geo_ch_flag_ui("geo_ch_flag"),
            mod_07_ch_flag_drug_ui("ch_flag_drug"),
            mod_08_geo_ch_flag_drug_ui("geo_ch_flag_drug")
          ),
          tabPanel(
            title = "Definitions"#,
            #mod_09_definitions_ui("definitions")
          ),
          tabPanel(
            title = "Methodology"#,
            #mod_10_methodology_ui("methodology")
          ),
          tabPanel(
            title = "Caveats"#,
            #mod_11_caveats_ui("caveats")
          )
        ),
        tags$script("
          $(document).ready(
            function () {
              $('.app-side-nav__list a[data-toggle=\"tab\"]').on('click', function (e) {
                window.scrollTo(0, 0)
            });
          });
        ")
      )
    ),
    br(),
    nhs_footer()
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Estimated prescribing patterns for care home patients aged 65 years and over"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
