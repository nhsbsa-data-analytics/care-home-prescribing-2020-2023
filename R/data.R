#' Aggregated prescribing data, with breakdowns by geography, demography,
#' gender, age band and carehome type
#'
#' \itemize{
#'   \item BREAKDOWN. overall breakdown category (Overall, Geographical - Region,
#'     Geographical - STP/ICS, Geographical - Local Authority, Demographical - Gender,
#'     Demographical - Age Band, Additional - Gender and Age Band,
#'     Additional - Care home type)
#'   \item SUB_BREAKDOWN_CODE. region or organisational codes (geography only)
#'   \item SUB_BREAKDOWN_NAME. name in breakdown (not in additional)
#'   \item GENDER. Female or Male (Additional - Gender and Age Band only)
#'   \item AGE_BAND. 65-69, ..., 85-89, 90+ (Additional - Gender and Age Band only)
#'   \item NURSING_HOME_FLAG. 1 if a nursing home, 0 otherwise (Additional - Care home type only)
#'   \item RESIDENTIAL_HOME_FLAG. 1 if a residential home, 0 otherwise (Additional - Care home type only)
#'   \item TOTAL_PATIENTS.number of distinct patients
#'   \item ITEMS_PER_PATIENT_MONTH. average number of distinct items per patient per month
#'   \item COST_PER_PATIENT_MONTH. average cost of items per patient per month
#'   \item TOTAL_PATIENTS_UNIQUE_MEDICINES. total unique medicines
#'   \item UNIQUE_MEDICINES_PER_PATIENT_MONTH average number of unique medicines per patient per month
#'   \item TOTAL_PATIENTS_TEN_OR_MORE total number of patients prescribed 10 or more unique medicines
#'   \item PCT_PATIENTS_TEN_OR_MORE_PER_PATIENT_MONTH percentage of patients
#'     prescribed 10 or more unique medicines per month
#'   \item SDC_*. Same 7 aggregate patient data fields, but with Statistical
#'     Disclosure Control and rounding
#' }
#'
#' @source {
#'   Data and Advanced Analytics Team at NHS BSA, using 
#'   \href{https://www.cqc.org.uk/about-us/transparency/using-cqc-data}{CQC} and 
#'   \href{https://beta.ordnancesurvey.co.uk/products/addressbase-plus}{AddressBase+} 
#'   data in combination with internal data
#' }
#'
#' @docType data
#' @keywords datasets
#' @name metrics_by_breakdown_and_ch_flag_df
#' @usage data(metrics_by_breakdown_and_ch_flag_df)
#' @format A data frame with 784 rows and 22 variables
#' 
NULL

#' GIS data for boundaries at Region, STP/ICS and Local Authority level
#'
#' \itemize{
#'   \item GEOGRAPHY. one of Region, STP/ICS or Local Authority
#'   \item SUB_GEOGRAPHY_CODE. region or organisation code
#'   \item SUB_GEOGRAPHY_NAME. region or organisation name
#'   \item GEOMETRY. boundary polygons
#' }
#'
#' @source {
#'   Data and Advanced Analytics Team at NHS BSA, using data from the API 
#'   provided by \href{https://www.arcgis.com/index.html}{ArcGIS}
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name map_df
#' @usage data(map_df)
#' @format A \code{sf} object, with 4 fields
#' 
NULL
