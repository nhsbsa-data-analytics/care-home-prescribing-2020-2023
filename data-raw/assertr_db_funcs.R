has_all_names_db <- function (...) {
  check_this <- list(...)
  given_names <- dbplyr::op_vars(parent.frame()$.top_env$lazy_query)
  all(check_this %in% given_names)
}

nrow_db <- function(lt) {
  dplyr::tally(lt) %>%
    dplyr::collect() %>%
    dplyr::pull()
}
