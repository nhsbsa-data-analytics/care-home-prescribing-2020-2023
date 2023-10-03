#' nhs_footer Function
#' 
#' @noRd
#'
#' @importFrom shiny tagList
nhs_footer <- function() {
  tagList(
    tags$footer(
      role = "contentinfo",
      tags$div(
        class = "nhsuk-footer",
        id = "nhsuk-footer",
        tags$div(
          class = "nhsuk-width-container app-width-container",
          tags$ul(
            class = "nhsuk-footer__list",
            tags$li(
              class = "nhsuk-footer__list-item",
              a(
                class = "nhsuk-footer__list-item-link",
                style = "text-decoration: underline;",
                href = "https://www.nhsbsa.nhs.uk/accessibility-statement-website-estimated-prescribing-patterns-care-home-patients-aged-65-years-or",
                target = "_blank",
                "Accessibility statement"
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
                href = "https://github.com/nhsbsa-data-analytics/careHomePrescribingTwo",
                target = "_blank",
                "GitHub"
              )
            )
          ),
          p(
            class = "nhsuk-footer__copyright",
            HTML("&#169; APLv2")
          )
        )
      )
    )
  )
}
