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
    includeMarkdown("inst/markdown/10_metrics_1.md"),
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
    includeMarkdown("inst/markdown/10_metrics_2.md"),
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
    includeMarkdown("inst/markdown/10_metrics_3.md"),
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
        "Mean unique falls risk medicines PPM",              "Falls",           "Sum of patient-monthly unique falls risk medicines",                            "Total patient-months",                                          "Mean number of unique medicines associated with falls risk prescribed per patient-month",
        "% of patient-months with 3+ falls risk medicines",  "Falls",           "Count of patient-months with 3+ unique falls risk medicines",                   "Total patient-months",                                          "Percentage of patient-months in which at least 3 unique medicines associated with falls risk were prescribed"
      ) %>% 
        knitr::kable("html", caption = "Prescribing metric definitions") %>%
        kableExtra::kable_styling(
          bootstrap_options = c("bordered", "striped", "hover", "condensed", "responsive")
        )
    ),
    includeMarkdown("inst/markdown/10_metrics_4.md")
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
