#' nhs_navlistPanel Function
#'
#' @noRd
#'
#' @importFrom shiny tagList
nhs_navlistPanel <- function(...,
                             id = NULL,
                             selected = NULL,
                             header = NULL,
                             footer = NULL,
                             fluid = TRUE,
                             widths = c(4, 8)) {

  # Create navlist panel
  nhs_navlistPanel <- shiny::navlistPanel(
    ...,
    id = id,
    selected = selected,
    header = header,
    footer = footer,
    fluid = fluid,
    widths = widths
  )

  # Hack the CSS to look like an NHS list
  nhs_navlistPanel$children[[1]]$children[[1]]$attribs$class <- "nhsuk-list nav-pills nav-stacked"
  nhs_navlistPanel$children[[1]]$attribs$role <- "navigation"
  nhs_navlistPanel$children[[1]]$attribs$`aria-label` <- "Navigation menu"

  tagList(
    nhs_navlistPanel
  )
}
