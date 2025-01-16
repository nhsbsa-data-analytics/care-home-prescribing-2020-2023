has_all_names.alt <- function(...) {
  check_this <- substitute(list(...))[-1] %>%
    as.list() %>% 
    purrr::map(\(x) if(is.character(x)) x else deparse(x))
  parent <- parent.frame()
  
  given_names <- rlang::env_names(parent$.top_env)
  if("lazy_query" %in% given_names) {
    given_names <- dbplyr::op_vars(parent$.top_env$lazy_query)
  }
  
  all(check_this %in% given_names)
}


nrow.alt <- function(tbl) {
  if(inherits(tbl, "tbl_lazy")) {
    dplyr::tally(tbl) %>%
      dplyr::collect() %>%
      dplyr::pull()
  } else {
    nrow(tbl)
  }
}


make.assertr.assert.error.alt <- function(verb, name.of.predicate, col.name,
                                          num.violations, error_lt) {
  time.or.times <- if (num.violations == 1) "time" else "times"
  msg <- paste0(
    "Column '", col.name, "' violates assertion '", name.of.predicate, "' ",
    num.violations, " ", time.or.times)
  
  this_error <- list()
  
  this_error$error_df <- error_lt %>% 
    dplyr::mutate(
      verb = verb,
      redux_fn = NA,
      predicate = name.of.predicate,
      column = col.name
    ) %>%
    dplyr::relocate(index, value, .after = tidyr::last_col()) %>% 
    head(1000) %>% 
    dplyr::collect()
  
  this_error$message <- msg
  this_error$num.violations <- num.violations
  this_error$call <- name.of.predicate
  class(this_error) <- c("assertr_assert_error", "assertr_error", "error", "condition")
  
  return(this_error)
}


assert.alt <- function(tbl, predicate, ..., pred_args = list()) {
  keeper.vars <- substitute(list(...))[-1] %>%
    as.list() %>%
    purrr::map(\(x) if(is.character(x)) x else deparse(x))
  
  if (length(keeper.vars) == 0) {
    stop(
      "assert requires columns to be selected. Check number of arguments",
      call. = FALSE
    )
  }
  
  name.of.predicate <- rlang::expr_text(rlang::enexpr(predicate))
  # browser()
  
  pred_args <- as.list(substitute(pred_args))[-1]
  
  additional_arg_names <- names(pred_args)
  predicate_arg_names <- names(formals(predicate))
  do_args = list(tbl = tbl, cols = keeper.vars)
  for(arg in intersect(additional_arg_names, predicate_arg_names)) {
    do_args[[arg]] = pred_args[[arg]]
  }
  res <- do.call(predicate, do_args)
  
  is_successful <- res %>%
    dplyr::summarise(
      dplyr::across(dplyr::ends_with("_result"), \(x) sum(x, na.rm = TRUE))
    ) %>%
    {
      if(inherits(., "tbl_lazy")) {
        dplyr::collect(.)
      } else {
        .
      }
    } %>%
    rowSums() %>%
    as.logical() %>%
    magrittr::not()
  
  if (is_successful) return(tbl)
  
  errors <- purrr::map(
    keeper.vars,
    \(col.name) {
      num.violations <- res %>%
        dplyr::select(dplyr::starts_with(col.name) & dplyr::ends_with("_result")) %>%
        dplyr::summarise(
          dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE))
        ) %>%
        {
          if(inherits(., "tbl_lazy")) {
            dplyr::collect(.)
          } else {
            .
          }
        } %>%
        dplyr::pull()
      if(is.na(num.violations) || !num.violations) return(NULL)
      
      error_lt <- res %>%
        dplyr::select(index, dplyr::starts_with(col.name)) %>%
        dplyr::rename(value = 2, result = 3) %>%
        dplyr::filter(result == 1) %>%
        dplyr::select(-result)
      
      make.assertr.assert.error.alt(
        "assert",
        name.of.predicate,
        col.name,
        num.violations,
        error_lt
      )
    }
  )
  
  errors <- Filter(\(x) !is.null(x), errors)
  assertr::error_stop(errors, data = tbl)
}


is_uniq.alt <- function(tbl, cols, .by = NULL) {
  by <- as.list(substitute(.by))
  if (length(by) > 1) by <- by[-1]
  by <- purrr::map_chr(by, rlang::as_string)
  
  cols %>%
    purrr::reduce(
      \(x, y) {
        new_column <- paste0(y, "_result")
        dplyr::add_count(x, !!dplyr::sym(y), name = new_column) %>%
          dplyr::mutate(
            !!dplyr::sym(new_column) := ifelse(
              is.na(!!dplyr::sym(y)),
              NA,
              !!dplyr::sym(new_column)
            )
          )
      },
      .init = tbl %>%
        dplyr::select(!!!cols, dplyr::all_of(by)) %>%
        dplyr::mutate(.TEMP = 1) %>%
        {
          if(inherits(., "tbl_lazy")) {
            dbplyr::window_order(., .TEMP)
          } else {
            .
          }
        } %>%
        dplyr::mutate(index = dplyr::row_number()) %>%
        dplyr::group_by(!!!dplyr::syms(by))
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_result"),
        \(x) ifelse(is.na(x) | x == 1, 0, 1)
      )
    ) %>%
    dplyr::select(-.TEMP) %>%
    dplyr::ungroup()
}


not_na.alt <- function(tbl, cols) {
  cols %>%
    purrr::reduce(
      \(x, y) {
        new_column <- paste0(y, "_result")
        x %>% 
          dplyr::mutate(
            !!dplyr::sym(new_column) := ifelse(
              is.na(!!dplyr::sym(y)),
              1,
              0
            )
          )
      },
      .init = tbl %>%
        dplyr::mutate(.TEMP = 1) %>%
        {
          if(inherits(., "tbl_lazy")) {
            dbplyr::window_order(., .TEMP)
          } else {
            .
          }
        } %>%
        dplyr::mutate(index = dplyr::row_number())
    ) %>% 
    dplyr::select(-.TEMP)
}
