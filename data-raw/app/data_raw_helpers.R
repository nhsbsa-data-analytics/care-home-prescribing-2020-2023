#' Get metrics for given groupings
#'
#' @param init_db A lazy table, either the raw base table or some modification
#'   of it. 
#' @param first_grouping A vector of col names to do initial grouping by.
#' @param second_grouping A vector of col names to do secondary grouping by.
#' @param nest_cols A vector of col names to use nesting on when completing.
#' @param num_parallel By default, query will ask for 36 parallel, but can ask 
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
#'   num_parallel = 36
#' )
get_metrics <- function(init_db,
                        first_grouping,
                        second_grouping,
                        nest_cols = c(),
                        num_parallel = 36) {
  # Collect data and calculate raw metrics
  init_db %>% 
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
      # Total patient months - use for denominators
      TOTAL_PM            = n(),
      TOTAL_PM_ACB        = sum(ANY_ACB, na.rm = TRUE),
      TOTAL_PM_DAMN       = sum(ANY_DAMN, na.rm = TRUE),
      # Items, cost and unique meds count
      ITEMS_PPM           = mean(TOTAL_ITEMS, na.rm = TRUE),
      COST_PPM            = mean(TOTAL_COST, na.rm = TRUE),
      UNIQ_MEDS_PPM       = mean(UNIQUE_MEDICINES, na.rm = TRUE),
      # Unique medicines numerators
      RISK_PM_GTE_SIX     = sum(GTE_SIX, na.rm = TRUE),
      RISK_PM_GTE_TEN     = sum(GTE_TEN, na.rm = TRUE),
      # ACB numerator
      RISK_PM_ACB         = sum(ACB, na.rm = TRUE),
      # DAMN numerator
      RISK_PM_DAMN        = sum(DAMN, na.rm = TRUE),
      # Falls unique medicines
      UNIQ_MEDS_FALLS_PPM = mean(UNIQUE_MEDICINES_FALLS, na.rm = TRUE),
      # Falls numerator
      RISK_PM_FALLS       = sum(FALLS, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    mutate(
      # Calculate % metrics - each denominator is restricted to patients on any
      # med that is also a condition to be included in numerator
      PCT_PM_GTE_SIX = 100 * case_when(
        TOTAL_PM == 0 ~ NA,
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
        TOTAL_PM       = 0L,
        TOTAL_PM_ACB   = 0L,
        TOTAL_PM_DAMN  = 0L,
        ITEMS_PPM      = 0L,
        COST_PPM       = 0L,
        UNIQ_MEDS_PPM  = 0L,
        PCT_PM_GTE_SIX = 0L,
        PCT_PM_GTE_TEN = 0L,
        PCT_PM_ACB     = 0L,
        PCT_PM_DAMN    = 0L,
        PCT_PM_FALLS   = 0L
      )
    ) %>%
    select(-starts_with("RISK")) %>% 
    # Reorder columns so they are a bit tidier
    relocate(starts_with("TOTAL"), .after = last_col()) %>%
    relocate(starts_with("PCT"), .after = last_col())
}
