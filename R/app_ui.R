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
        h1(
          "Estimated prescribing patterns for care home patients aged 65 years
           and over from 2020/21 until 2022/23"
        ),
        nhs_navlistPanel(
          well = FALSE,
          widths = c(2, 10),
          tabPanel(
            title = "Article",
            mod_01_headline_figures_ui("headline_figures"),
            h2_tabstop(
              "Demographic estimates for care home patients aged 65 years and
               over receiving prescriptions"
            ),
            mod_02_patients_age_gender_ui("patients_age_gender"),
            mod_03_patients_imd_ui("patients_imd"),
            h2_tabstop(
              "Estimated prescribing metrics for care home vs non-care home 
               patients aged 65 years and over"
            ),
            mod_04_metrics_ch_type_ui("metrics_ch_type"),
            mod_05_metrics_age_gender_ui("metrics_age_gender"),
            mod_06_geo_ch_flag_ui("geo_ch_flag"),
            h2_tabstop("Care home prescribing drug profile"),
            mod_07_geo_ch_flag_drug_ui("geo_ch_flag_drug"),
            h2_tabstop("Final thoughts"),
            p(
              "This article provides estimates of primary care prescribing patterns
               for care home  and non-care home patients aged 65 years and over in
               England during 2020/21, 2021/22 and 2022/23 based on experimental
               data linkage work."
            ),
            p(
              "This analysis addresses a key gap in knowledge and gives valuable
               insights which can inform the use and management of medicines in
               care homes to help improve health outcomes, quality of care and
               ensure value.
            ")
          ),
          tabPanel(
            title = "Metrics",
            mod_09_metrics_ui("metrics")
          ),
          tabPanel(
            title = "Datasets",
            mod_10_datasets_ui("datasets")
          ),
          tabPanel(
            title = "Methodology",
            mod_11_methodology_ui("methodology")
          ),
          tabPanel(
            title = "Feedback",
            mod_12_feedback_ui("feedback")
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
