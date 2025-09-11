#' nhs_footer Function
#' 
#' @noRd
#'
#' @importFrom shiny tagList
mod_nhs_footer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$footer(
      role = "contentinfo",
      tags$div(
        class = "nhsuk-footer",
        id = "nhsuk-footer",
        tags$div(
          class = "nhsuk-width-container app-width-container",
          style = "display: flex; align-items: center;",
          tags$ul(
            class = "nhsuk-footer__list",
            tags$li(
              class = "nhsuk-footer__list-item",
              actionLink(
                inputId = ns("ethics_link"),
                label = "Data ethics statement",
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;"
              )
            ),
            tags$li(
              class = "nhsuk-footer__list-item",
              a(
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;",
                href = "https://www.nhsbsa.nhs.uk/accessibility-statement-website-estimated-prescribing-patterns-care-home-patients-aged-65-years-or",
                target = "_blank",
                "Accessibility statement",
                icon("up-right-from-square")
              )
            ),
            tags$li(
              class = "nhsuk-footer__list-item",
              a(
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;",
                href = "mailto:dall@nhsbsa.nhs.uk",
                target = "_blank",
                "Contact us"
              )
            ),
            tags$li(
              class = "nhsuk-footer__list-item",
              a(
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;",
                href = "https://github.com/nhsbsa-data-analytics/care-home-prescribing-2020-2023",
                target = "_blank",
                "GitHub",
                icon("up-right-from-square")
              )
            )
          ),
          div(
            class = "nhsuk-footer__copyright",
            style = "display: inline-flex; flex-direction: column; width: 30%;",
            tags$image(
              class = "nhsuk-logo",
              style = "height: 30px;",
              src = "www/assets/logos/logo-ogl.svg",
              name = "Open Government License logo",
              alt = "Open Government License"
            ),
            span(
              "All content is available under the",
              style = "text-align: center;",
              tags$a(
                "Open Government Licence v3.0",
                href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
                rel = "license"
              ),
              "except where otherwise stated"
            )
          )
        )
      )
    )
  )
}

mod_nhs_footer_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(reactive(input$ethics_link))
  })
}
