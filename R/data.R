#' Aggregated prescribing data broken down by financial year, geography and 
#' carehome or non-carehome
#'
#' \itemize{
#'   \item FY. financial year, one of 2020/21, 2021/22 or 2022/23
#'   \item GEOGRAPHY. Region, ICB or Local Authority
#'   \item SUB_GEOGRAPHY_CODE. region or organisational codes
#'   \item SUB_GEOGRAPHY_NAME. name in breakdown
#'   \item CH_FLAG. do metrics apply to carehomes?, \code{TRUE} or \code{FALSE}
#'   \item ITEMS_PPM. mean number of distinct items per patient month
#'   \item COST_PPM. mean cost of items per patient month
#'   \item UNIQ_MEDS_PPM. mean number of unique medicines from BNF Chapters 
#'     1-4, 6-10 per patient month
#'   \item UNIQ_MEDS_FALLS_PPM. mean number of unique medicines with associated
#'     risk of falls per patient month
#'   \item TOTAL_PM. total patient months
#'   \item TOTAL_PM_ACB. total number of patient months in which at least 2
#'     medicines of moderate to high anticholinergic burden were prescribed
#'   \item TOTAL_PM_DAMN. total number of patient months in which at least 2
#'     medicines likely to cause kidney injury were prescribed
#'   \item PCT_PM_GTE_SIX. percentage of patient months in which at least 6
#'     unique medicines from BNF Chapters 1-4, 6-10 were prescribed
#'   \item PCT_PM_GTE_TEN. percentage of patient months in which at least 10
#'     unique medicines from BNF Chapters 1-4, 6-10 were prescribed
#'   \item PCT_PM_ACB. percentage of patient months in which at least 2
#'     medicines of moderate to high anticholinergic burden were prescribed
#'   \item PCT_PM_DAMN. percentage of patient months in which at least 2
#'     medicines likely to cause kidney injury were prescribed
#'   \item PCT_PM_FALLS. percentage of patient months in which at least 3 
#'     unique medicines with associated risk of falls were prescribed
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
#' @format A data frame with 2148 rows and 17 variables
#' 
"metrics_by_geo_and_ch_flag_df"


#' Aggregated prescribing data broken down by financial year and carehome type
#'
#' \itemize{
#'   \item FY. financial year, one of 2020/21, 2021/22 or 2022/23
#'   \item CH_TYPE. one of Carehome, Non-carehome, Nursing Home, Residential Home
#'   \item ITEMS_PPM. mean number of distinct items per patient month
#'   \item COST_PPM. mean cost of items per patient month
#'   \item UNIQ_MEDS_PPM. mean number of unique medicines from BNF Chapters 
#'     1-4, 6-10 per patient month
#'   \item UNIQ_MEDS_FALLS_PPM. mean number of unique medicines with associated
#'     risk of falls per patient month
#'   \item TOTAL_PM. total patient months
#'   \item TOTAL_PM_ACB. total number of patient months in which at least 2
#'     medicines of moderate to high anticholinergic burden were prescribed
#'   \item TOTAL_PM_DAMN. total number of patient months in which at least 2
#'     medicines likely to cause kidney injury were prescribed
#'   \item PCT_PM_GTE_SIX. percentage of patient months in which at least 6
#'     unique medicines from BNF Chapters 1-4, 6-10 were prescribed
#'   \item PCT_PM_GTE_TEN. percentage of patient months in which at least 10
#'     unique medicines from BNF Chapters 1-4, 6-10 were prescribed
#'   \item PCT_PM_ACB. percentage of patient months in which at least 2
#'     medicines of moderate to high anticholinergic burden were prescribed
#'   \item PCT_PM_DAMN. percentage of patient months in which at least 2
#'     medicines likely to cause kidney injury were prescribed
#'   \item PCT_PM_FALLS. percentage of patient months in which at least 3 
#'     unique medicines with associated risk of falls were prescribed
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
#' @format A data frame with 12 rows and 14 variables
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
