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
    tags$a(id = "skiplink", "Skip to Main Content", href = "#maincontent"),
    # First level UI elements
    nhs_header(),
    br(),
    div(id = "maincontent"),
    tags$div(
      class = "nhsuk-width-container",
      tags$div(
        class = "nhsuk-main-wrapper",
        role = "main",
        h1(
          "Estimated prescribing patterns for care home patients aged 65 years 
           and over in England"
        ),
        tags$div(style = "
                  border:3px solid #768692;
                  #color:#768692;
                  border-radius: 10px;
                  #margin: 0px;
                  #margin-bottom: 20px;
                  padding: 20px;
                  padding-bottom: 0;
                  background-color: #D9E2E3;
                  ",
                 p(
                   span("Feedback Survey: ",
                        style = "font-weight: 600; font-size: 12pt;"
                   ),
                   span(
                     "The NHS Business Services Authority are committed to improving
                      the reports that we publish. You have the opportunity to give us
                      your opinion of this report, its content and what you would like
                      to see in the future by completing a",
                     enurl(
                       text = "feedback survey (opens in new tab).",
                       url = "https://online1.snapsurveys.com/bsareport?rpt=5"
                     ),
                     style = "font-size: 11pt"
                   )
                 )
        ),
        hr(),
        nhs_navlistPanel(
          id = "mainTabs",
          well = FALSE,
          widths = c(2, 10),
          tabPanel(
            title = "Article",
            mod_01_headline_figures_ui("headline_figures"),
            mod_02_patients_age_gender_ui("patients_age_gender"),
            mod_03_patients_ch_type_ui("patients_ch_type"),
            mod_04_metrics_ch_type_ui("metrics_ch_type"),
            mod_05_metrics_age_gender_ui("metrics_age_gender"),
            mod_06_geo_ch_flag_ui("geo_ch_flag"),
            mod_07_ch_flag_drug_ui("ch_flag_drug"),
            mod_08_geo_ch_flag_drug_ui("geo_ch_flag_drug"),
            mod_09_final_thoughts_ui("final_thoughts")
          ),
          tabPanel(
            title = "Metrics",
            mod_10_metrics_ui("metrics")
          ),
          tabPanel(
            title = "Datasets",
            mod_11_datasets_ui("datasets")
          ),
          tabPanel(
            title = "Address Matching",
            mod_12_address_matching_ui("address_matching")
          ),
          tabPanel(
            title = "Feedback",
            mod_13_feedback_ui("feedback")
          ),
          tabPanel(
            title = "Annex",
            mod_14_annex_ui("annex")
          )
        )
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
  # add_resource_path(
  #   "www", app_sys("app/www")
  # )
  # 
  # add_resource_path(
  #   "markdown", app_sys("markdown")
  # )
  # 
  # tags$head(
  #   favicon(),
  #   bundle_resources(
  #     path = app_sys("app/www"),
  #     app_title = "Estimated prescribing patterns for care home patients aged 65 years and over"
  #   )
  #   # Add here other external resources
  #   # for example, you can add shinyalert::useShinyalert()
  # )
  
  resources <- app_sys("app/www")
  addResourcePath("www", resources)
  # addResourcePath("markdown", app_sys("app/markdown"))
  
  tags$head(
    favicon(),
    tags$title(
      "Estimated prescribing patterns for care home patients aged 65 years and over"
    ),
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    # Javascript resources
    htmltools::htmlDependency(
      name = "resources",
      version = "0.0.1",
      src = resources,
      script = list.files(resources, pattern = "\\.js$", recursive = TRUE),
      package = NULL,
      all_files = TRUE
    ),
    # CSS resources
    lapply(
      list.files(resources, pattern = "\\.css$", recursive = TRUE),
      function(x) tags$link(href = file.path("www", x), rel = "stylesheet")
    )
  )
}
