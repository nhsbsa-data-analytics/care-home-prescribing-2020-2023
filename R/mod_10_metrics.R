#' 10_metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_10_metrics_ui <- function(id){
  tagList(
    include_dynamic_md("inst/markdown/10_metrics_1.md"),
    HTML(
      tibble::tribble(
        ~Patient, ~Month,      ~`Prescription items`,  ~`Unique medicines`,
        #--------|------------|-----------------------|-----------------
        "A",      "April",     4,                      2,
        "A",      "May",       4,                      2,
        "A",      "June",      4,                      4,
        "A",      "July",      6,                      3,
        "A",      "September", 6,                      3,
        "A",      "November",  6,                      5,
        "B",      "April",     13,                     10,
        "B",      "May",       13,                     10,
        "B",      "August",    10,                     8,
        "C",      "October",   4,                      4
      ) %>%
        knitr::kable("html", caption = "Patient-months example") %>%
        kableExtra::kable_styling(
          bootstrap_options = c("bordered", "striped", "hover", "condensed", "responsive")
        )
    ),
    include_dynamic_md("inst/markdown/10_metrics_2.md"),
    HTML(
      tibble::tribble(
        ~`BNF Level`,          ~Category,   ~Name,
        #---------------------|------------|--------------------
        "Section",             "Inclusion", char_vec_html(falls_section_vec),
        "Paragraph",           "Inclusion", char_vec_html(falls_paragraph_vec),
        "Sub-paragraph",       "Inclusion", "Benzodiazepines", # is this consistent with the code?
        "Chemical substance",  "Exclusion", char_vec_html(falls_exclude_chem_vec)
      ) %>% 
        knitr::kable("html", escape = FALSE, caption = "Falls risk medicine groups") %>%
        kableExtra::kable_styling(
          bootstrap_options = c("bordered", "striped", "hover", "condensed", "responsive")
        )
    ),
    include_dynamic_md("inst/markdown/10_metrics_3.md"),
    HTML(
      tibble::tribble(
        ~`Metric name`,                                      ~`Metric Type`,    ~Numerator,                                                                      ~Denominator,                                                    ~Description,
        #---------------------------------------------------|------------------|--------------------------------------------------------------------------------|----------------------------------------------------------------|-----------------
        "Mean prescription items PPM",                       "Volume and cost", "Sum of prescription items",                                                     "Total patient-months",                                          "Mean number of items prescribed per patient-month",                                           
        "Mean drug cost PPM",                                "Volume and cost", "Sum of drug cost",                                                              "Total patient-months",                                          "Mean cost of items prescribed per patient-month",
        "Mean unique medicines PPM",                         "Polypharmacy",    "Sum of patient-monthly unique medicines from BNF chapters 1-4, 6-10",           "Total patient-months",                                          "Mean number of unique medicines from BNF Chapters 1-4, 6-10 prescribed per patient-month",
        "% of patient-months with 6+ unique medicines",      "Polypharmacy",    "Count of patient-months with 6+ unique medicines from BNF chapters 1-4, 6-10",  "Total patient-months",                                          "Percentage of patient-months in which at least 6 unique medicines from BNF Chapters 1-4, 6-10 were prescribed",
        "% of patient-months with 10+ unique medicines",     "Polypharmacy",    "Count of patient-months with 10+ unique medicines from BNF chapters 1-4, 6-10", "Total patient-months",                                          "Percentage of patient-months in which at least 10 unique medicines from BNF Chapters 1-4, 6-10 were prescribed",
        "% of patient-months with 2+ ACB medicines",         "Polypharmacy",    "Count of patient-months with 2+ unique moderate to high ACB medicines",         "Count of patient-months with 1+ Moderate to High ACB medicine", "Percentage of patient-months with anticholinergic prescribing, in which at least 2 medicines of moderate to high anticholinergic burden were prescribed",
        "% of patient-months with 2+ DAMN medicines",        "Polypharmacy",    "Count of patient-months with 2+ unique DAMN medicines",                         "Count of patient-months with 1+ DAMN medicine",                 "Percentage of patient-months with DAMN prescribing, in which at least 2 medicines likely to cause kidney injury were prescribed",
        "% of patient-months with 2+ ACAP medicines",        "Polypharmacy",    "Count of patient-months with 2+ unique ACAP medicines",                         "Count of patient-months with 1+ ACAP medicine",                 "Percentage of patient-months with anticoagulant/antiplatelet prescribing, in which at least 2 anticoagulant/antiplatelet medicines were prescribed",
        "Mean unique falls risk medicines PPM",              "Falls",           "Sum of patient-monthly unique falls risk medicines",                            "Total patient-months",                                          "Mean number of unique medicines associated with falls risk prescribed per patient-month",
        "% of patient-months with 3+ falls risk medicines",  "Falls",           "Count of patient-months with 3+ unique falls risk medicines",                   "Total patient-months",                                          "Percentage of patient-months in which at least 3 unique medicines associated with falls risk were prescribed"
      ) %>% 
        knitr::kable("html", caption = "Prescribing metric definitions") %>%
        kableExtra::kable_styling(
          bootstrap_options = c("bordered", "striped", "hover", "condensed", "responsive")
        )
    ),
    include_dynamic_md("inst/markdown/10_metrics_4.md"),
    HTML(
      data.frame(
        `Year Month` = c(
          202304, 202305, 202306, 202307, 202307, 202308, 202309, 202310, 202311, 202311, 
          202312, 202401, 202401, 202402, 202403, 202304, 202305, 202306, 202307, 202308
        ),
        `Residence Type` = c(
          "Non-care home", "Non-care home", "Non-care home", "Non-care home", 
          "Care home", "Care home", "Care home", "Care home", "Care home", 
          "Non-care home", "Non-care home", "Non-care home","Care home", "Care home", 
          "Care home", "Care home", "Care home", "Care home", "Care home", "Care home"
        ),
        `Length of Stay` = c(0, 0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7),
        `Length of Stay Band` = c(
          "-", "-", "-", "-", "-", "1-3 Months", "1-3 Months", "1-3 Months", 
          "-", "-", "-", "-", "-", "1-3 Months", "1-3 Months", "1-3 Months", 
          "4-6 Months", "4-6 Months", "4-6 Months", "7-9 Months"
        ),
        check.names = FALSE
      ) %>% 
      kableExtra::kable("html", align = "r", caption = "Care home length of stay example") %>% 
        kableExtra::row_spec(c(6:8, 14:20), bold = TRUE) %>% 
        kableExtra::kable_styling(
          full_width = TRUE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed", "responsive")
        )
    ),
    include_dynamic_md("inst/markdown/10_metrics_5.md")
  )
}
    
#' 10_metrics Server Functions
#'
#' @noRd 
mod_10_metrics_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
  })
}
    
## To be copied in the UI
# mod_10_metrics_ui("10_metrics_1")
    
## To be copied in the server
# mod_10_metrics_server("10_metrics_1")
