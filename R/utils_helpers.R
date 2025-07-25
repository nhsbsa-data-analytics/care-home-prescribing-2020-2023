#' Define the breakdowns
#'
#' Define the labels of the breakdowns (in order of hierarchy) with the columns
#' that are used to aggregate
#'
#' @noRd
breakdowns <- list(
  "Overall" = c(SUB_BREAKDOWN_NAME = "OVERALL"),
  "Geographical - Region" = c(
    SUB_BREAKDOWN_CODE = "PCD_REGION_CODE",
    SUB_BREAKDOWN_NAME = "PCD_REGION_NAME"
  ),
  "Geographical - ICS" = c(
    SUB_BREAKDOWN_CODE = "PCD_ICB_CODE",
    SUB_BREAKDOWN_NAME = "PCD_ICB_NAME"
  ),
  "Geographical - Local Authority" = c(
    SUB_BREAKDOWN_CODE = "PCD_LAD_CODE",
    SUB_BREAKDOWN_NAME = "PCD_LAD_NAME"
  ),
  # "Geographical - PCN" = c(SUB_BREAKDOWN_NAME = "PRESCRIBER_PCN"),
  "Demographical - Gender" = c(SUB_BREAKDOWN_NAME = "GENDER"),
  "Demographical - Age Band" = c(SUB_BREAKDOWN_NAME = "AGE_BAND"),
  "Additional - Gender and Age Band" = c(
    GENDER = "GENDER",
    AGE_BAND = "AGE_BAND"
  ),
  "Additional - Care home type" = c(
    NURSING_HOME_FLAG = "NURSING_HOME_FLAG",
    RESIDENTIAL_HOME_FLAG = "RESIDENTIAL_HOME_FLAG"
  )
)


#' Define the geographies
#'
#' Extract them from the breakdowns.
#'
#' @noRd
geographies <- breakdowns %>%
  purrr::keep(
    .p = stringr::str_detect(
      string = names(.),
      pattern = "Overall|Geographical - "
    )
  ) %>%
  purrr::set_names(
    nm = stringr::str_replace(
      string = names(.),
      pattern = "Geographical - ",
      replacement = ""
    )
  )


#' Define the BNF levels
#'
#' Define the labels of the BNF (in order of hierarchy) with the columns
#' that are used to aggregate
#'
#' @noRd
bnfs <- list(
  "Chapter" = "CHAPTER_DESCR",
  "Section" = "SECTION_DESCR",
  "Paragraph" = "PARAGRAPH_DESCR",
  "Chemical Substance" = "CHEMICAL_SUBSTANCE_BNF_DESCR"
)


#' Format data-raw table
#'
#' Deal with factors and sort table.
#'
#' @param df Dataframe
#' @param vars Grouping variables
#'
#' @return Modified dataframe
#' @noRd
format_data_raw <- function(df, vars) {
  # Initially sort the factors
  df <- df %>%
    dplyr::arrange(
      dplyr::across(
        dplyr::any_of(
          c(
            "FY",
            "YEAR_MONTH",
            "SUB_BREAKDOWN_NAME",
            "SUB_GEOGRAPHY_NAME",
            "SUB_BNF_LEVEL_NAME",
            vars
          )
        )
      )
    )
  
  # Make some columns factors, with overall as first level (when present)
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(
          c("FY", "YEAR_MONTH", "SUB_BREAKDOWN_NAME", "SUB_GEOGRAPHY_NAME")
        ),
        .fns = ~ if("Overall" %in% .x) {
          forcats::fct_relevel(as.factor(.x), "Overall")
        } else {
          as.factor(.x)
        }
      )
    )
  
  # Breakdown is a hierarchy
  if ("BREAKDOWN" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        BREAKDOWN = forcats::fct_relevel(.data$BREAKDOWN, names(breakdowns))
      )
  }
  
  # Geography is a hierarchy
  if ("GEOGRAPHY" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        GEOGRAPHY = forcats::fct_relevel(.data$GEOGRAPHY, names(geographies))
      )
  }
  
  # BNF level is a hierarchy
  if ("BNF_LEVEL" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        BNF_LEVEL = forcats::fct_relevel(.data$BNF_LEVEL, names(bnfs))
      )
  }
  
  # Sort final dataframe by new factors
  df %>%
    dplyr::arrange(
      dplyr::across(
        dplyr::any_of(
          c(
            "FY",
            "YEAR_MONTH",
            "BREAKDOWN",
            "SUB_BREAKDOWN_NAME",
            "GEOGRAPHY",
            "SUB_GEOGRAPHY_NAME",
            "BNF_LEVEL",
            "SUB_BNF_LEVEL_NAME",
            vars
          )
        )
      )
    )
}

#' fontawesome save to datauri
#' taken from https://jkunst.com/highcharter/articles/fontawesome.html
#'
#' @param name fontawsome name
#' @param vars Grouping variables
#'
#' @return Data URI as a character string.
#' @noRd

fa_to_png_to_datauri <- function(name, ...) {
  tmpfl <- tempfile(fileext = ".png")
  
  fontawesome::fa_png(name, file = tmpfl, ...)
  
  knitr::image_uri(tmpfl)
}

#' Apply bespoke rounding logic
#' 
#' Numbers 1-5 rounded to 5, numbers 6-10 rounded to 10, all other numbers rounded to nearest 10.
#' 
#' The function is vectorised.
#'
#' @param x Number
#'
#' @return Rounded number.
#' @noRd
bespoke_round <- function(vec) {
  
  result <- rep(0L, length(vec)) # Empty vector for storing the result
  
  for (i in seq_along(vec)) {
    
    if (dplyr::between(vec[i], 1, 5)) {
      
      result[i] <- 5L
      
    } else if (dplyr::between(vec[i], 6, 10)) {
      
      result[i] <- 10L
      
    } else {
      
      result[i] <- janitor::round_half_up(vec[i], -1) |> as.integer()
      
    }
  }
  
  return(result)
  
}



# Drug lookups

# NSAID metric other drugs
other_drug_vec = c(
  "0601022B0",
  "0601023AD",
  "0601023AF",
  "0601023AH",
  "0601023AJ",
  "0601023AL",
  "0601023AP",
  "0601023AR",
  "0601023V0",
  "0601023W0",
  "0601023Z0"
)

# Medicines with moderate to high anticholinergic burde
acb_drugs <- c(
  "0102000AC",
  "0102000H0",
  "0102000J0",
  "0102000K0",
  "0102000N0",
  "0102000Y0",
  "0104020H0",
  "0304010AD",
  "0304010F0",
  "0304010G0",
  "0304010H0",
  "0304010J0",
  "0304010K0",
  "0304010N0",
  "0304010W0",
  "0304010Y0",
  "030902040",
  "0309020AB",
  "0309020AH",
  "0309020U0",
  "0402010C0",
  "0402010D0",
  "0402010I0",
  "0402010K0",
  "0402010L0",
  "0402010P0",
  "0402010Q0",
  "0402010W0",
  "0402010X0",
  "0403010B0",
  "0403010C0",
  "0403010F0",
  "0403010J0",
  "0403010L0",
  "0403010N0",
  "0403010R0",
  "0403010V0",
  "0403010Y0",
  "0403030D0",
  "0403030Z0",
  "0406000A0",
  "0406000AA",
  "0406000AC",
  "0406000H0",
  "0406000L0",
  "0406000N0",
  "0406000V0",
  "0407010P0",
  "0407020V0",
  "0409020C0",
  "0409020E0",
  "0409020N0",
  "0409020S0",
  "0704020AC",
  "0704020AD",
  "0704020G0",
  "0704020J0",
  "0704020N0",
  "0704020P0",
  "0704040G0",
  "1001040G0",
  "1002020T0"
)

# Falls section level drug groups
falls_section_vec = c(
  "Antidepressant drugs",
  "Antiepileptic drugs",
  "Diuretics",
  "Hypertension and heart failure",
  "Hypnotics and anxiolytics"
)

# Falls paragraph level drug groups
falls_paragraph_vec = c(
  "Alpha-adrenoceptor blocking drugs",
  "Antihistamines",
  "Antipsychotic depot injections",
  "Antipsychotic drugs",
  "Drugs for urinary frequency enuresis and incontinence",
  "Nitrates",
  "Opioid analgesics",
  "Opioid dependence",
  "Vasodilator antihypertensive drugs"
)

# Falls chem sub level groups
falls_chem_vec = c('Midazolam hydrochloride')

# Falls risk chem sub exclusions
falls_exclude_chem_vec = c(
  "Paraldehyde",
  "Mirabegron",
  "Mannitol",
  "Loratadine",
  "Desloratadine",
  "Bilastine",
  "Minoxidil",
  "Vibegron"
)

# Useful for defining character vectors. It orders elements alphabetically for
# ease of adding or removing entries. Output can be copied from console into definition.
order_char_vec <- function(x) {
  ordered <- sort(x)
  cat(paste(shQuote(ordered, type="cmd"), collapse=",\n"))
}

# Orders elements alphabetically and combine into html string for a listing.
char_vec_html <- function(x) {
  ordered <- sort(x)
  paste0(ordered, collapse = "<br>")
}

# This interpolates the values into the markdown
# 
# NOTE: the placeholders as written in the markdown are £> and <£
# When glue gets these it will be in HTML form, thus why different in this function
include_dynamic_md <- function(md_path) {
  HTML(
    glue::glue(
      shiny::includeMarkdown(md_path),
      .open = "£&gt;",
      .close = "&lt;£"
    )
  )
}
