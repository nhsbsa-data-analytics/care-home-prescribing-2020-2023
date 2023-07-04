#' Aggregated prescribing data broken down by financial year, geography and 
#' carehome or non-carehome
#'
#' \itemize{
#'   \item FY. financial year, one of 2020/21, 2021/22 or 2022/23
#'   \item GEOGRAPHY. Region, ICB or Local Authority
#'   \item SUB_GEOGRAPHY_CODE. region or organisational codes
#'   \item SUB_GEOGRAPHY_NAME. name in breakdown
#'   \item CH_FLAG. do metrics apply to carehomes?, \code{TRUE} or \code{FALSE}
#'   \item TOTAL_PATIENTS. number of distinct patients
#'   \item ITEMS_PPM. mean number of distinct items per patient per month
#'   \item COST_PPM. mean cost of items per patient per month
#'   \item UNIQ_MEDS_PPM. mean number of unique medicines per patient per month
#'   \item UNIQ_MEDS_FALLS_PPM. mean number of unique medicines with associated
#'     risk of falls per patient per month
#'   \item TOTAL_PATIENTS_GTE_SIX. total number of patients prescribed 6+ unique
#'     medicines
#'   \item PCT_PATIENTS_GTE_SIX_PPM. percentage of patients prescribed 6+ unique
#'     medicines per month
#'   \item TOTAL_PATIENTS_GTE_TEN. total number of patients prescribed 10+ unique
#'     medicines
#'   \item PCT_PATIENTS_GTE_TEN_PPM. percentage of patients prescribed 10+ unique
#'     medicines per month
#'   \item TOTAL_PATIENTS_ACB_6. total number of patients with an anticholinergic
#'     score of 6+
#'   \item PCT_PATIENTS_ACB_6_PPM. percentage of patients with an anticholinergic
#'     score of 6+ per month
#'   \item TOTAL_PATIENTS_DAMN. total number of patients prescribed 2 or more 
#'     medicines likely to cause kidney injury
#'   \item PCT_PATIENTS_ACB_6_PPM. percentage of patients prescribed 2 or more
#'     medicines likely to cause kidney injury per month
#'   \item TOTAL_PATIENTS_FALLS. total number of patients prescribed 3 or more 
#'     medicines with associated risk of falls
#'   \item PCT_PATIENTS_FALLS_PPM. percentage of patients prescribed 3 or more
#'     medicines with associated risk of falls per month
#' }
#'
#' @source { Data and Advanced Analytics Team at NHS BSA, using
#'   \href{https://www.cqc.org.uk/about-us/transparency/using-cqc-data}{CQC} and
#'   \href{https://beta.ordnancesurvey.co.uk/products/addressbase-plus}{AddressBase+}
#'   data in combination with internal data }
#'
#' @docType data
#' @keywords datasets
#' @name metrics_by_geo_and_ch_flag
#' @usage data(metrics_by_geo_and_ch_flag_df)
#' @format A data frame with 2148 rows and 20 variables
#' 
"metrics_by_geo_and_ch_flag_df"


#' Aggregated prescribing data broken down by financial year and carehome type
#'
#' \itemize{
#'   \item FY. financial year, one of 2020/21, 2021/22 or 2022/23
#'   \item CH_TYPE. one of Carehome, Non-carehome, Nursing Home, Residential Home
#'   \item TOTAL_PATIENTS. number of distinct patients
#'   \item ITEMS_PPM. mean number of distinct items per patient per month
#'   \item COST_PPM. mean cost of items per patient per month
#'   \item UNIQ_MEDS_PPM. mean number of unique medicines per patient per month
#'   \item UNIQ_MEDS_FALLS_PPM. mean number of unique medicines with associated
#'     risk of falls per patient per month
#'   \item TOTAL_PATIENTS_GTE_SIX. total number of patients prescribed 6+ unique
#'     medicines
#'   \item PCT_PATIENTS_GTE_SIX_PPM. percentage of patients prescribed 6+ unique
#'     medicines per month
#'   \item TOTAL_PATIENTS_GTE_TEN. total number of patients prescribed 10+ unique
#'     medicines
#'   \item PCT_PATIENTS_GTE_TEN_PPM. percentage of patients prescribed 10+ unique
#'     medicines per month
#'   \item TOTAL_PATIENTS_ACB_6. total number of patients with an anticholinergic
#'     score of 6+
#'   \item PCT_PATIENTS_ACB_6_PPM. percentage of patients with an anticholinergic
#'     score of 6+ per month
#'   \item TOTAL_PATIENTS_DAMN. total number of patients prescribed 2 or more 
#'     medicines likely to cause kidney injury
#'   \item PCT_PATIENTS_ACB_6_PPM. percentage of patients prescribed 2 or more
#'     medicines likely to cause kidney injury per month
#'   \item TOTAL_PATIENTS_FALLS. total number of patients prescribed 3 or more 
#'     medicines with associated risk of falls
#'   \item PCT_PATIENTS_FALLS_PPM. percentage of patients prescribed 3 or more
#'     medicines with associated risk of falls per month
#' }
#'
#' @source { Data and Advanced Analytics Team at NHS BSA, using
#'   \href{https://www.cqc.org.uk/about-us/transparency/using-cqc-data}{CQC} and
#'   \href{https://beta.ordnancesurvey.co.uk/products/addressbase-plus}{AddressBase+}
#'   data in combination with internal data }
#'
#' @docType data
#' @keywords datasets
#' @name metrics_by_ch_type
#' @usage data(metrics_by_ch_type_df)
#' @format A data frame with 12 rows and 17 variables
#' 
"metrics_by_ch_type_df"


#' GIS data for boundaries at Region, ICB and Local Authority level
#'
#'  \itemize{
#'     \item Region. \code{Featurecollection} of 7 \code{Feature}s
#'     \item ICB. \code{Featurecollection} of 42 \code{Feature}s
#'     \item Local Authority. \code{Featurecollection} of 309 \code{Feature}s
#' }
#'
#' @source { Data and Advanced Analytics Team at NHS BSA, using data from the
#'   API provided by \href{https://www.arcgis.com/index.html}{ArcGIS} }
#'
#' @docType data
#' @keywords datasets
#' @name geo_data
#' @usage data(geo_data)
#' @format A named \code{list}, created from \href{https://geojson.org/}{geojson}
#'   using \code{jsonlite::fromJSON(simplifyVector = FALSE)}
#' 
"geo_data"
