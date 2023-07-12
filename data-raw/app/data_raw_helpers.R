#' Get metrics for given groupings
#'
#' @param init_db A lazy table, either the raw base table or some modification
#'   of it. 
#' @param first_grouping A vector of col names to do initial grouping by.
#' @param second_grouping A vector of col names to do secondary grouping by.
#' @param comp_fill A named vector, names are col names and values are 
#'   replacements for any NAs.
#' @param nest_cols A vector of col names to use nesting on when completing.
#' @param num_parallel By default, query will ask for 8 parallel, but can ask 
#'   for different value if you think it is needed.
#'
#' @return
#'
#' @examples
#' get_metrics(
#'   init_db,
#'   first_grouping = c(
#'     "FY",
#'     "GEOGRAPHY",
#'     "SUB_GEOGRAPHY_CODE",
#'     "SUB_GEOGRAPHY_NAME",
#'     "YEAR_MONTH",
#'     "NHS_NO",
#'     "CH_FLAG"
#'   ),
#'   second_grouping = c(
#'     "FY",
#'     "GEOGRAPHY",
#'     "SUB_GEOGRAPHY_CODE",
#'     "SUB_GEOGRAPHY_NAME",
#'     "CH_FLAG"
#'   ),
#'   nest_cols = c(
#'     "GEOGRAPHY",
#'     "SUB_GEOGRAPHY_CODE",
#'     "SUB_GEOGRAPHY_NAME"
#'   ),
#'   num_parallel = 32
#' )
get_metrics <- function(init_db,
                        first_grouping,
                        second_grouping,
                        nest_cols = c(),
                        num_parallel = 24) {
  # Collect data and calculate raw metrics
  init_db %>% 
    mutate(
      across(all_of(first_grouping)),
      ITEM_COUNT,
      ITEM_PAY_DR_NIC,
      BNF_CHEMICAL_SUBSTANCE,
      CHEMICAL_SUBSTANCE_BNF_DESCR,
      CHAPTER_1_4_6_10_CAT = case_when(
        as.integer(
          substr(BNF_CHEMICAL_SUBSTANCE, 1, 2)
        ) %in% c(1:4, 6:10) ~ 1,
        TRUE ~ 0
      ),
      ACB_CAT = case_when(
        BNF_CHEMICAL_SUBSTANCE %in% acb_drugs ~ 1,
        TRUE ~ 0
      ),
      DAMN_CAT = case_when(
        REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^100101') > 0 ~ 1,
        REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205051') > 0 ~ 1,
        REGEXP_INSTR(BNF_CHEMICAL_SUBSTANCE, '^0205052') > 0 ~ 1,
        BNF_CHEMICAL_SUBSTANCE %in% other_drug_vec ~ 1,
        TRUE ~ 0
      ),
      FALLS_CAT = case_when(
        (SECTION_DESCR %in% falls_section_vec |
        PARAGRAPH_DESCR %in% falls_paragraph_vec |
        CHEMICAL_SUBSTANCE_BNF_DESCR %in% falls_chem_vec) &
        !CHEMICAL_SUBSTANCE_BNF_DESCR %in% falls_exclude_chem_vec ~ 1,
        TRUE ~ 0
      ),
      .keep = "none"
    ) %>% 
    group_by(across(all_of(first_grouping))) %>%
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT, na.rm = TRUE),
      TOTAL_COST =  0.01 * sum(ITEM_PAY_DR_NIC, na.rm = TRUE),
      UNIQUE_MEDICINES = n_distinct(
        case_when(
          CHAPTER_1_4_6_10_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      ),
      GTE_SIX = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[CHAPTER_1_4_6_10_CAT == 1]) >= 6 ~ 1L,
        TRUE ~ 0L 
      ),
      GTE_TEN = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[CHAPTER_1_4_6_10_CAT == 1]) >= 10 ~ 1L,
        TRUE ~ 0L 
      ),
      ANY_ACB = any(
        case_when(
          ACB_CAT > 0 ~ 1L,
          TRUE ~ 0L
        ),
        na.rm = TRUE
      ),
      ACB = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1]) >= 2 ~ 1L,
        TRUE ~ 0L
      ),
      ANY_DAMN = any(
        case_when(
          DAMN_CAT > 0 ~ 1L,
          TRUE ~ 0L
        ),
        na.rm = TRUE
      ),
      DAMN = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[DAMN_CAT == 1]) >= 2 ~ 1L,
        TRUE ~ 0L
      ),
      FALLS = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[FALLS_CAT == 1]) >= 3 ~ 1L,
        TRUE ~ 0L
      ),
      UNIQUE_MEDICINES_FALLS = n_distinct(
        case_when(
          FALLS_CAT == 1 ~ BNF_CHEMICAL_SUBSTANCE,
          TRUE ~ NA
        )
      )
    ) %>%
    ungroup() %>% 
    group_by(across(all_of(second_grouping))) %>%
    summarise(
      # Only needed if we use SDC
      TOTAL_PATIENTS      = n_distinct(NHS_NO),
      # Total patient months - Use for denominators, also used for SDC
      TOTAL_PM            = n(),
      TOTAL_PM_ACB        = sum(ANY_ACB, na.rm = TRUE),
      TOTAL_PM_DAMN       = sum(ANY_DAMN, na.rm = TRUE),
      # Items, cost and unique meds count
      ITEMS_PPM           = mean(TOTAL_ITEMS, na.rm = TRUE),
      COST_PPM            = mean(TOTAL_COST, na.rm = TRUE),
      UNIQ_MEDS_PPM       = mean(UNIQUE_MEDICINES, na.rm = TRUE),
      # Unique medicines numerators, also used for SDC
      RISK_PM_GTE_SIX     = sum(GTE_SIX, na.rm = TRUE),
      RISK_PM_GTE_TEN     = sum(GTE_TEN, na.rm = TRUE),
      # ACB numerator, also used for SDC
      RISK_PM_ACB         = sum(ACB, na.rm = TRUE),
      # DAMN numerator, also used for SDC
      RISK_PM_DAMN        = sum(DAMN, na.rm = TRUE),
      # Falls unique medicines
      UNIQ_MEDS_FALLS_PPM = mean(UNIQUE_MEDICINES_FALLS, na.rm = TRUE),
      # Falls numerator, also used for SDC
      RISK_PM_FALLS       = sum(FALLS, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    mutate(
      # Calculate % metrics - each denominator is restricted to patients on any
      # med that is also a condition to be included in numerator
      PCT_PM_GTE_SIX = 100 * case_when(TOTAL_PM == 0 ~ NA,
        TRUE ~ RISK_PM_GTE_SIX / TOTAL_PM
      ),
      PCT_PM_GTE_TEN = 100 * case_when(
        TOTAL_PM == 0 ~ NA,
        TRUE ~ RISK_PM_GTE_TEN / TOTAL_PM
      ),
      PCT_PM_ACB = 100 * case_when(
        TOTAL_PM_ACB == 0 ~ NA,
        TRUE ~ RISK_PM_ACB / TOTAL_PM_ACB
      ),
      PCT_PM_DAMN = 100 * case_when(
        TOTAL_PM_DAMN == 0 ~ NA,
        TRUE ~ RISK_PM_DAMN / TOTAL_PM_DAMN
      ),
      PCT_PM_FALLS = 100 * case_when(
        TOTAL_PM == 0 ~ NA,
        TRUE ~ RISK_PM_FALLS / TOTAL_PM
      )
    ) %>%
    nhsbsaR::collect_with_parallelism(num_parallel) %>%
    # Complete
    tidyr::complete(
      tidyr::nesting(!!!syms(nest_cols)),
      !!!syms(setdiff(second_grouping, nest_cols)),
      fill = list(
        TOTAL_PATIENTS  = 0L,
        TOTAL_PM        = 0L,
        TOTAL_PM_ACB    = 0L,
        TOTAL_PM_DAMN   = 0L,
        RISK_PM_GTE_SIX = 0L,
        RISK_PM_GTE_TEN = 0L,
        RISK_PM_ACB     = 0L,
        RISK_PM_DAMN    = 0L,
        RISK_PM_FALLS   = 0L
      )
    ) %>%
    # Statistical disclosure control (SDC)
    # mutate(
    #   SDC = ifelse(TOTAL_PATIENTS %in% 1:4, TRUE, FALSE),
    #   TOTAL_PATIENTS = ifelse(
    #     SDC,
    #     NA_integer_,
    #     janitor::round_half_up(TOTAL_PATIENTS, -1)
    #   ),
    #   ITEMS_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(ITEMS_PPM, 1)
    #   ),
    #   COST_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(COST_PPM)
    #   ),
    #   SDC = ifelse(TOTAL_PM_MED_CH_1_4_6_10 %in% 1:4, TRUE, FALSE),
    #   TOTAL_PM_MED_CH_1_4_6_10 = ifelse(
    #     SDC,
    #     NA_integer_,
    #     janitor::round_half_up(TOTAL_PM_MED_CH_1_4_6_10, -1)
    #   ),
    #   UNIQ_MEDS_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(UNIQ_MEDS_PPM, 1)
    #   ),
    #   # Applicable test for SDC is the number in risk, replace total with this
    #   SDC = ifelse(RISK_PATIENTS_GTE_SIX %in% 1:4, TRUE, FALSE),
    #   TOTAL_PM_GTE_SIX = ifelse(
    #     SDC,
    #     NA_integer_,
    #     janitor::round_half_up(RISK_PATIENTS_GTE_SIX, -1)
    #   ),
    #   PCT_PATIENTS_GTE_SIX_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(PCT_PATIENTS_GTE_SIX_PPM, 1)
    #   ),
    #   # Applicable test for SDC is the number in risk, replace total with this
    #   SDC = ifelse(RISK_PATIENTS_GTE_TEN %in% 1:4, TRUE, FALSE),
    #   TOTAL_PM_GTE_TEN = ifelse(
    #     SDC,
    #     NA_integer_,
    #     janitor::round_half_up(RISK_PATIENTS_GTE_TEN, -1)
    #   ),
    #   PCT_PATIENTS_GTE_TEN_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(PCT_PATIENTS_GTE_TEN_PPM, 1)
    #   ),
    #   # Applicable test for SDC is the number in risk, replace total with this
    #   SDC = ifelse(RISK_PATIENTS_ACB_6 %in% 1:4, TRUE, FALSE),
    #   TOTAL_PM_ACB_6 = ifelse(
    #     SDC,
    #     NA_integer_,
    #     janitor::round_half_up(RISK_PATIENTS_ACB_6, -1)
    #   ),
    #   PCT_PATIENTS_ACB_6_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(PCT_PATIENTS_ACB_6_PPM, 1)
    #   ),
    #   # Applicable test for SDC is the number in risk, replace total with this
    #   SDC = ifelse(RISK_PATIENTS_DAMN %in% 1:4, TRUE, FALSE),
    #   TOTAL_PM_DAMN = ifelse(
    #     SDC,
    #     NA_integer_,
    #     janitor::round_half_up(RISK_PATIENTS_DAMN, -1)
    #   ),
    #   PCT_PATIENTS_DAMN_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(PCT_PATIENTS_DAMN_PPM, 1)
    #   ),
    #   # Applicable test for SDC is total patients for this 'new' metric
    #   SDC = ifelse(TOTAL_PATIENTS %in% 1:4, TRUE, FALSE),
    #   UNIQ_MEDS_FALLS_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(UNIQ_MEDS_FALLS_PPM, 1)
    #   ),
    #   # Applicable test for SDC is the number in risk, replace total with this
    #   SDC = ifelse(RISK_PATIENTS_FALLS %in% 1:4, TRUE, FALSE),
    #   TOTAL_PM_FALLS = ifelse(
    #     SDC,
    #     NA_integer_,
    #     janitor::round_half_up(RISK_PATIENTS_FALLS, -1)
    #   ),
    #   PCT_PATIENTS_FALLS_PPM = ifelse(
    #     SDC,
    #     NA_real_,
    #     janitor::round_half_up(PCT_PATIENTS_FALLS_PPM, 1)
    #   )
    # ) %>%
    # select(-SDC, -starts_with("RISK")) %>% 
    select(-TOTAL_PATIENTS, -starts_with("RISK")) %>% 
    # Reorder columns so they are a bit tidier
    relocate(starts_with("TOTAL"), .after = last_col()) %>%
    relocate(starts_with("PCT"), .after = last_col())
}
