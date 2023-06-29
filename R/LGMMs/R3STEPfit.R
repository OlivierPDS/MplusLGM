R3STEPfit <- function(list_mpobj,
                      std = "unstd",
                      ref = NULL) {
  # Test arguments ----------------------------------------------------------
  # list_mpobj <- R3STEP_models
  # std <- "stdyx"
  # ref <- 1

  # Validate arguments & define values   ------------------------------------
  stopifnot(std %in% c("unstd", "stdyx", "stdy", "std"))

  std <- switch(std,
    "unstd" = "unstandardized",
    "stdyx" = "stdyx.standardized",
    "stdy" = "stdy.standardized",
    "std" = "std.standardized"
  )

  if (is.null(ref)) {
    ref <- purrr::pluck(list_mpobj, 1, "results", "summaries", "NLatentClasses")
  }
  stopifnot(ref %in% seq(ref))

  # Extract warnings and errors ---------------------------------------------
  warnings <- list_mpobj %>%
    purrr::map(pluck, "results", "warnings", .default = NULL)

  errors <- list_mpobj %>%
    purrr::map(pluck, "results", "errors", .default = NULL)

  warn_err <- purrr::map2(warnings, errors, \(x, y) dplyr::tibble(warnings = paste(x, collapse = " "), errors = paste(y, collapse = " "))) %>%
    purrr::map(~ mutate(.x, dplyr::across(tidyselect::everything(), ~ stringr::str_remove_all(.x, "[[:punct:]]")))) %>%
    purrr::map(~ mutate(.x, dplyr::across(tidyselect::everything(), ~ dplyr::na_if(.x, "")))) %>%
    purrr::imap(\(x, idx) dplyr::mutate(x, name = stringr::str_to_upper(idx))) %>%
    purrr::reduce(merge, all = TRUE)

  # Extract parameters or alternative parameterization ------------------------------------------------------
  sublist_mpobj <- list_mpobj %>%
    purrr::discard(purrr::map_vec(errors, ~ length(.x) > 0))

  if (ref == last(seq(ref))) {
    param <- purrr::map(sublist_mpobj, ~ purrr::pluck(.x, "results", "parameters", std, .default = NULL))
  } else {
    param <- purrr::map(sublist_mpobj, ~ purrr::pluck(.x, "results", "parameters", "unstandardized.alt", glue::glue("ref.cat.{ref}"), .default = NULL))
  }

  param <- param %>%
    purrr::imap(\(x, idx) dplyr::mutate(x, name = stringr::str_to_upper(idx))) %>%
    purrr::reduce(merge, all = TRUE) %>% # Error with bind_rows and list_rbind: "Can't combine `est_se` <double> and `est_se` <character>."
    dplyr::mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, param), paramHeader)) %>%
    dplyr::mutate(param = ifelse(stringr::str_detect(paramHeader, "^Intercepts"), name, param)) %>%
    dplyr::mutate(param = ifelse(stringr::str_detect(param, "N#\\d"), paste(name, param), param))

  # Compute odd ratio and confidence intervals -----------------------------
  or_ci <- param %>%
    dplyr::mutate(low2.5 = est - 1.96 * se, up2.5 = est + 1.96 * se) %>%
    dplyr::mutate(dplyr::across(c("est", "low2.5", "up2.5"),
      ~ ifelse(str_detect(paramHeader, "C#\\d.ON"),
        exp(.x),
        NA
      ),
      .names = "OR_{.col}"
    ))

  # Merge parameters, confidence intervals, warnings and errors  ------------
  table <- list(or_ci, warn_err) %>%
    purrr::reduce(full_join, by = "name") %>%
    dplyr::mutate(param = dplyr::coalesce(param, name)) %>%
    dplyr::filter(stringr::str_detect(paramHeader, "C#\\d") | !is.na(errors)) %>% # filter out irrelevant parameters
    dplyr::mutate(sig = dplyr::case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*"
    )) %>% # add significativity
    dplyr::mutate(dplyr::across(c(tidyselect::where(is.numeric), -pval), ~ round(.x, digits = 2))) %>%
    dplyr::select(paramHeader, param, est, se, pval, sig, tidyselect::starts_with("OR"), warnings, errors) %>%
    dplyr::arrange(paramHeader, -est, param)

  return(table)
}
