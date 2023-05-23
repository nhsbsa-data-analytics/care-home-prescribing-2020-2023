#' \code{carehomes2} package
#'
#' Includes analysis of carehomes data and a report as a shiny app with modules.
#'
#' @docType package
#' @name carehomes2
#'
#' @importFrom rlang := .data
#' 
NULL

if(getRversion() >= "2.15.1") utils::globalVariables(
  c(
    # Oracle connection object
    "con",
    
    # Oracle SQL
    "TO_DATE",
    "REGEXP_REPLACE",
    
    # Lazy table columns
    "REGISTRATION_DATE",
    "DEREGISTRATION_DATE",
    "POSTCODE",
    "POSTCODE_OLD",
    "TOKEN",
    "TOKEN_NUMBER",
    "UPRN"
  )
)
