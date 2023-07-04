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
  "Geographical - ICB" = c(
    SUB_BREAKDOWN_CODE = "PCD_ICB_CODE",
    SUB_BREAKDOWN_NAME = "PCD_ICB_NAME"
  ),
  "Geographical - Local Authority" = c(
    SUB_BREAKDOWN_CODE = "PCD_LAD_CODE",
    SUB_BREAKDOWN_NAME = "PCD_LAD_NAME"
  ),
  "Geographical - PCN" = c(SUB_BREAKDOWN_NAME = "PRESCRIBER_PCN"),
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
  
  # Move overall to the first category
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(
          c("FY", "YEAR_MONTH", "SUB_BREAKDOWN_NAME", "SUB_GEOGRAPHY_NAME")
        ),
        .fns = ~ forcats::fct_relevel(.x, "Overall")
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

# Drug lookups

# NSAID metric other drugs
other_drug_vec = c(
  '0601022B0', 
  '0601023AD',
  '0601023AF',
  '0601023AH',
  '0601023AJ',
  '0601023AL',
  '0601023AP',
  '0601023AR',
  '0601023V0',
  '0601023W0',
  '0601023Z0'
)

# ACB score of 1
acb1_drugs <- c(
  '0102000A0','0102000C0','0103010D0','0103010E0','0103010S0','0103010T0','0104020D0',
  '0104020L0','0104020P0','0105020B0','0105020C0','0105020D0','0105020E0','0105020F0',
  '0107010AB','0107010L0','0107020J0','0107020P0','0107040B0','0201010F0','0202010F0',
  '0202020L0','0202030W0','0202040U0','0202040V0','0202080E0','0202080K0','0203020F0',
  '0203020G0','0203020T0','0203020U0','0204000A0','0204000E0','0204000F0','0204000J0',
  '0204000K0','0204000U0','0204000W0','0205010J0','0205051F0','0206010I0','0206010K0',
  '0206010W0','0206020R0','0208020V0','0209000L0','0209000V0','0301030S0','0304010AB',
  '0304010AC','0304010D0','0304010I0','0304010Y0','0309010C0','0401020A0','0401020K0',
  '0401020V0','040201030','0402010AD','0402010AE','0402010J0','0402020AA','0402020AB',
  '0402020AD','0402020T0','0402030R0','0403010X0','0403030L0','0403040B0','0403040W0',
  '040702010','040702020','0407020A0','0407020C0','0407020N0','0407020P0','0407020Q0',
  '0410020A0','0603020J0','0603020L0','0603020M0','0603020N0','0603020Q0','0603020T0',
  '0603020V0','0603020W0','0603020X0','1001022G0','1001022N0','1001040G0'
)

# ACB score of 2
acb2_drugs <- c(
  '0102000H0',
  '0304010K0',
  '0309020C0',
  '0402010M0',
  '0402010R0',
  '0407010P0',
  '0408010C0', 
  '0408010D0',
  '0409010B0',
  '1002020H0',
  '1003020D0'
)

# ACB score of 3
acb3_drugs <- c(
  '010200030','0102000AB','0102000AC','0102000F0','0102000J0','0102000K0','0102000Q0',
  '0102000Y0','0301020A0','0301020B0','030401020','0304010F0','0304010G0','0304010H0',
  '0304010J0','0304010N0','0304010W0','030902040','030902050','030902060','0309020AB',
  '0309020U0','040201060','0402010AB','0402010C0','0402010D0','0402010Q0','0402010W0',
  '0402010X0','0402020AC','0403010B0','0403010C0','0403010E0','0403010F0','0403010H0',
  '0403010L0','0403010N0','0403010V0','0403010Y0','0403030P0','0406000AA','0406000AC',
  '0406000H0','0406000V0','0409020C0','0409020N0','0704010W0','0704020AB','0704020AC',
  '0704020AD','0704020G0','0704020J0','0704020N0','0704020P0','0704020Z0','0704040G0',
  '1002020S0','1002020V0'
)

# Falls section level drug groups
falls_section_vec = c(
  'Antidepressant drugs',
  'Antiepileptic drugs',
  'Diuretics',
  'Hypertension and heart failure'
)

# Falls paragraph level drug groups
falls_paragraph_vec = c(
  'Antipsychotic depot injections',
  'Antipsychotic drugs',
  'Opioid analgesics',
  'Opioid dependence',
  'Alpha-adrenoceptor blocking drugs',
  'Antihistamines',
  'Vasoconstrictor sympathomimetics',
  'Vasodilator antihypertensive drugs',
  'Drugs for urinary frequency enuresis and incontinence'
)

# Falls chem sub level groups
falls_chem_vec = c('Midazolam hydrochloride')