#' nhs_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nhs_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "position:relative; height:55px;",
      tags$div(
        class = "nhsuk-action-link",
        style = "position:absolute; bottom:0; right:0; margin-bottom:0;",
        shiny::downloadLink(
          outputId = ns("download"),
          class = "nhsuk-action-link__link",
          tags$svg(
            class = "nhsuk-icon nhsuk-icon__arrow-right-circle",
            xmlns = "http://www.w3.org/2000/svg",
            viewBox = "0 0 24 24",
            `aria-hidden` = "true",
            width = "36",
            height = "36",
            tags$path(
              d = "M0 0h24v24H0z",
              fill = "none"
            ),
            tags$path(
              d = "M12 2a10 10 0 0 0-9.95 9h11.64L9.74 7.05a1 1 0 0 1 1.41-1.41l5.66 5.65a1 1 0 0 1 0 1.42l-5.66 5.65a1 1 0 0 1-1.41 0 1 1 0 0 1 0-1.41L13.69 13H2.05A10 10 0 1 0 12 2z"
            )
          ),
          tags$span(
            class = "nhsuk-action-link__text",
            "Download Data"
          )
        )
      )
    )
  )
}

#' nhs_download Server Functions
#'
#' @noRd
mod_nhs_download_server <- function(id, filename, export_data,
                                    currency_xl_fmt_str = "£#,##0",
                                    percent_xl_fmt_str = "#0.00%",
                                    number_xl_fmt_str = "#,##0"
                                    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$download <- downloadHandler(
      filename = filename,
      content = function(file) {
        # Separate process for CSV
        if (filename %>% endsWith("csv")) {
          write.table(
            # Handle possibility of reactive input
            x = if (is.data.frame(export_data)) export_data else export_data(),
            file = file,
            row.names = FALSE,
            quote = FALSE,
            sep = ","
          )
          # If it is not a CSV, it is an XLSX
        } else {
          # Handle possibility of reactive input
          df <- if (is.data.frame(export_data)) export_data else export_data()
          
          # Divide any % columns by 100, as will be using formatting whereby
          # they will be multiplied by 100
          df <- df %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::contains("%"),
                ~ . / 100
              )
            )
          
          # Get currency, % and remaining numeric column indices for styling
          currency_cols <- grep("cost", names(df))
          percent_cols <- grep("%", names(df))
          number_cols <- df %>% 
            purrr::map(is.numeric) %>% 
            unlist() %>% 
            which() %>% 
            setdiff(union(currency_cols, percent_cols)) 
          
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Prescribing data")
          
          # Add currency styling
          s <- openxlsx::createStyle(numFmt = currency_xl_fmt_str)
          openxlsx::addStyle(
            wb,
            1,
            style = s,
            rows = 1:nrow(df) + 1,
            cols = currency_cols,
            gridExpand = TRUE
          )
          # Add % styling
          s <- openxlsx::createStyle(numFmt = percent_xl_fmt_str)
          openxlsx::addStyle(
            wb,
            1,
            style = s,
            rows = 1:nrow(df) + 1,
            cols = percent_cols,
            gridExpand = TRUE
          )
          # Add 1000s separator styling
          s <- openxlsx::createStyle(numFmt = number_xl_fmt_str)
          openxlsx::addStyle(
            wb,
            1,
            style = s,
            rows = 1:nrow(df) + 1,
            cols = number_cols,
            gridExpand = TRUE
          )
          
          # Writes to an XLSX file, with auto-filtering applied
          openxlsx::writeData(
            wb,
            "Prescribing data",
            df,
            rowNames = FALSE,
            withFilter = TRUE
          )
          # Set column widths to be wide enough to contain their name
          openxlsx::setColWidths(
            wb,
            "Prescribing data",
            cols = seq_along(export_data),
            widths = "auto"
          )
          openxlsx::saveWorkbook(wb, file)
        }
      }
    )
  })
}
## To be copied in the UI
# mod_nhs_download_ui("nhs_download_ui_1")

## To be copied in the server
# mod_nhs_download_server("nhs_download_ui_1")
