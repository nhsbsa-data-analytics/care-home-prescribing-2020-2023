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
#'   comp_fill = list(
#'     TOTAL_PATIENTS = 0L,
#'     ITEMS_PPM = NA_real_,
#'     COST_PPM = NA_real_,
#'     UNIQ_MEDS_PPM = NA_real_,
#'     PATIENTS_GTE_SIX = 0L,
#'     PCT_PATIENTS_GTE_SIX_PPM = NA_real_,
#'     PATIENTS_GTE_TEN = 0L,
#'     PCT_PATIENTS_GTE_TEN_PPM = NA_real_,
#'     PATIENTS_ACB_6 = 0L,
#'     PCT_PATIENTS_ACB_6_PPM = NA_real_,
#'     PATIENTS_DAMN = 0L,
#'     PCT_PATIENTS_DAMN_PPM = NA_real_,
#'     UNIQ_MEDS_FALLS_PPM = NA_real_,
#'     PATIENTS_FALLS = 0L,
#'     PCT_PATIENTS_FALLS_PPM = NA_real_
#'   ),
#'   nest_cols = c(
#'     "GEOGRAPHY",
#'     "SUB_GEOGRAPHY_CODE",
#'     "SUB_GEOGRAPHY_NAME"
#'   ),
#'   num_parallel = 24
#' )
get_metrics <- function(init_db,
                        first_grouping,
                        second_grouping,
                        comp_fill,
                        nest_cols = c(),
                        num_parallel = 8) {
  # Collect data and calculate raw metrics
  out <- init_db %>% 
    mutate(
      across(all_of(first_grouping)),
      ITEM_COUNT,
      ITEM_PAY_DR_NIC,
      BNF_CHEMICAL_SUBSTANCE,
      CHEMICAL_SUBSTANCE_BNF_DESCR,
      ACB_CAT = case_when(
        BNF_CHEMICAL_SUBSTANCE %in% acb1_drugs ~ 1,
        BNF_CHEMICAL_SUBSTANCE %in% acb2_drugs ~ 2,
        BNF_CHEMICAL_SUBSTANCE %in% acb3_drugs ~ 3,
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
        SECTION_DESCR %in% falls_section_vec ~ 1,
        PARAGRAPH_DESCR %in% falls_paragraph_vec ~ 1,
        CHEMICAL_SUBSTANCE_BNF_DESCR %in% falls_chem_vec ~ 1,
        TRUE ~ 0
      ),
      .keep = "none"
    ) %>%
    group_by(across(all_of(first_grouping))) %>%
    summarise(
      TOTAL_ITEMS = sum(ITEM_COUNT),
      TOTAL_COST = sum(ITEM_PAY_DR_NIC * 0.01),
      UNIQUE_MEDICINES = n_distinct(
        ifelse(
          as.integer(substr(BNF_CHEMICAL_SUBSTANCE, 1, 2)) %in% c(1:4, 6:10),
          CHEMICAL_SUBSTANCE_BNF_DESCR,
          NA_character_
        )
      ),
      ACB_6 = case_when( 
        (1 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 1])) + 
          (2 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 2])) + 
          (3 * n_distinct(BNF_CHEMICAL_SUBSTANCE[ACB_CAT == 3])) >= 6 ~ 1,
        TRUE ~ 0
      ),
      DAMN = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[DAMN_CAT == 1]) >= 2 ~ 1,
        TRUE ~ 0
      ),
      FALLS = case_when(
        n_distinct(BNF_CHEMICAL_SUBSTANCE[FALLS_CAT == 1]) >= 3 ~ 1,
        TRUE ~ 0
      ),
      UNIQUE_MEDICINES_FALLS = n_distinct(
        ifelse(
          FALLS_CAT == 1,
          CHEMICAL_SUBSTANCE_BNF_DESCR,
          NA_character_
        )
      )
    ) %>%
    ungroup() %>% 
    group_by(across(all_of(second_grouping))) %>%
    summarise(
      # Use total patients as denominator for all % metrics, and for SDC
      TOTAL_PATIENTS = n_distinct(NHS_NO),
      # Items and cost
      ITEMS_PPM = mean(TOTAL_ITEMS),
      COST_PPM = mean(TOTAL_COST),
      # Unique medicines numerators, also used for SDC
      UNIQ_MEDS_PPM = mean(UNIQUE_MEDICINES),
      PATIENTS_GTE_SIX = n_distinct(
        ifelse(
          UNIQUE_MEDICINES >= 6,
          NHS_NO,
          NA_integer_
        )
      ),
      PATIENTS_GTE_TEN = n_distinct(
        ifelse(
          UNIQUE_MEDICINES >= 10,
          NHS_NO,
          NA_integer_
        )
      ),
      # ACB numerator, also used for SDC
      PATIENTS_ACB_6 = n_distinct(
        ifelse(
          ACB_6 == 1,
          NHS_NO,
          NA_integer_
        )
      ),
      # DAMN numerator, also used for SDC
      PATIENTS_DAMN = n_distinct(
        ifelse(
          DAMN == 1,
          NHS_NO,
          NA_integer_
        )
      ),
      # Falls unique medicines
      UNIQ_MEDS_FALLS_PPM = mean(UNIQUE_MEDICINES_FALLS),
      # Falls numerator, also used for SDC
      PATIENTS_FALLS = n_distinct(
        ifelse(
          FALLS == 1,
          NHS_NO,
          NA_integer_
        )
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Calculate % metrics
      across(
        starts_with("PATIENTS_"),
        \(x) {
          if_else(
            TOTAL_PATIENTS == 0,
            NA_real_,
            x / TOTAL_PATIENTS * 100
          )
        },
        .names = "PCT_{.col}_PPM"
      )
    ) %>%
    nhsbsaR::collect_with_parallelism(num_parallel) %>% 
    # Complete
    complete(
      nesting(!!!syms(nest_cols)),
      !!!syms(setdiff(second_grouping, nest_cols)),
      fill = comp_fill
    ) %>%
    # Statistical disclosure control (SDC)
    mutate(
      SDC = ifelse(TOTAL_PATIENTS %in% 1:4, TRUE, FALSE),
      TOTAL_PATIENTS = ifelse(
        SDC,
        NA_integer_,
        round(TOTAL_PATIENTS, -1)
      ),
      ITEMS_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(ITEMS_PPM, 1)
      ),
      COST_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(COST_PPM)
      ),
      UNIQ_MEDS_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(UNIQ_MEDS_PPM, 1)
      ),
      SDC = ifelse(PATIENTS_GTE_SIX %in% 1:4, TRUE, FALSE),
      PATIENTS_GTE_SIX = ifelse(
        SDC,
        NA_integer_,
        round(PATIENTS_GTE_SIX, -1)
      ),
      PCT_PATIENTS_GTE_SIX_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(PCT_PATIENTS_GTE_SIX_PPM, 1)
      ),
      SDC = ifelse(PATIENTS_GTE_TEN %in% 1:4, TRUE, FALSE),
      PATIENTS_GTE_TEN = ifelse(
        SDC,
        NA_integer_,
        round(PATIENTS_GTE_TEN, -1)
      ),
      PCT_PATIENTS_GTE_TEN_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(PCT_PATIENTS_GTE_TEN_PPM, 1)
      ),
      SDC = ifelse(PATIENTS_ACB_6 %in% 1:4, TRUE, FALSE),
      PATIENTS_ACB_6 = ifelse(
        SDC,
        NA_integer_,
        round(PATIENTS_ACB_6, -1)
      ),
      PCT_PATIENTS_ACB_6_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(PCT_PATIENTS_ACB_6_PPM, 1)
      ),
      SDC = ifelse(PATIENTS_DAMN %in% 1:4, TRUE, FALSE),
      PATIENTS_DAMN = ifelse(
        SDC,
        NA_integer_,
        round(PATIENTS_DAMN, -1)
      ),
      PCT_PATIENTS_DAMN_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(PCT_PATIENTS_DAMN_PPM, 1)
      ),
      SDC = ifelse(TOTAL_PATIENTS %in% 1:4, TRUE, FALSE),
      UNIQ_MEDS_FALLS_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(UNIQ_MEDS_FALLS_PPM, 1)
      ),
      SDC = ifelse(PATIENTS_FALLS %in% 1:4, TRUE, FALSE),
      PATIENTS_FALLS = ifelse(
        SDC,
        NA_integer_,
        round(PATIENTS_FALLS, -1)
      ),
      PCT_PATIENTS_FALLS_PPM = ifelse(
        SDC,
        NA_integer_,
        janitor::round_half_up(PCT_PATIENTS_FALLS_PPM, 1)
      )
    ) %>%
    select(-SDC)
  
  # Get names of % metrics cols
  pct_cols <- out %>% select(starts_with("PCT")) %>% names()
  
  pct_cols %>%
    walk(
      \(x) {
        # The 'middle' is the substring w/o leading "PCT_" or trailing "_PPM"
        middle <- gsub("PCT_|_PPM", "", x)
        out <<- out %>%
          relocate(
            # Rename the col to start with "TOTAL_ ..."
            !!sym(glue("TOTAL_{middle}")) := {{ middle }},
            # ...and move it in front of its % metric col
            .before = {{ x }}
          )
      }
    )
  
  out
}
