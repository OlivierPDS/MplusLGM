MixREGfit <- function(list_mpobj, std = "unstd") {
  # Test arguments ----------------------------------------------------------
  # list_mpobj <- MixREG_models
  # std <- "stdyx"

  # Validate arguments & define values   ------------------------------------
  stopifnot(std %in% c("unstd", "stdyx", "stdy", "std"))

  std <- switch(std,
    "unstd" = "unstandardized",
    "stdyx" = "stdyx.standardized",
    "stdy" = "stdy.standardized",
    "std" = "std.standardized"
  )

  # Extract warnings and errors ---------------------------------------------
  warnings <- list_mpobj %>%
    purrr::map(pluck, "results", "warnings", .default = NULL)

  errors <- list_mpobj %>%
    purrr::map(pluck, "results", "errors", .default = NULL)

  warn_err <- purrr::map2(warnings, errors, \(x, y) dplyr::tibble(warnings = paste(x, collapse = " "), errors = paste(y, collapse = " "))) %>%
    purrr::map(~ dplyr::mutate(.x, dplyr::across(tidyselect::everything(), ~ stringr::str_remove_all(.x, "[[:punct:]]")))) %>%
    purrr::map(~ dplyr::mutate(.x, dplyr::across(tidyselect::everything(), ~ dplyr::na_if(.x, "")))) %>%
    purrr::imap(\(x, idx) dplyr::mutate(x, name = stringr::str_to_upper(idx))) %>%
    purrr::reduce(merge, all = TRUE)

  sublist_mpobj <- list_mpobj %>%
    purrr::discard(purrr::map_vec(errors, ~ length(.x) > 0))

  # Extract parameters, confidence intervals and Wald Test-----------------------------
  param <- sublist_mpobj %>%
    purrr::map(pluck, "results", "parameters", std, .default = NULL)

  ci <- sublist_mpobj %>%
    purrr::map(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL)

  wt <- sublist_mpobj %>%
    purrr::map(pluck, "results", "summaries", .default = NULL)

  # var <- sublist_mpobj %>%
  #   purrr::map(pluck, "results", "sampstat", "univariate.sample.statistics", .default = NULL)

  test <- sublist_mpobj %>%
    purrr::map(pluck, "MODELTEST", .default = NULL) %>%
    purrr::map(~ stringr::str_split_i(.x, "\\s", 1)) %>%
    purrr::map(~ switch(.x,
      "I1" = c("I.ON", "IW"),
      "I2" = c("I.ON", "SW"),
      "I3" = c("I.ON", "QW"),
      "I4" = c("I.ON", "CUBW"),
      "S1" = c("S.ON", "IW"),
      "S2" = c("S.ON", "SW"),
      "S3" = c("S.ON", "QW"),
      "S4" = c("S.ON", "CUBW"),
      "iw1" = c("Means", "IW"),
      "sw1" = c("Means", "SW"),
      "qw1" = c("Means", "QW"),
      "cubw1" = c("Means", "CUBW")
    ))

  results <- list(param, ci, wt) %>%
    purrr::map(~ purrr::imap(.x, \(x, idx) dplyr::mutate(x, name = stringr::str_to_upper(idx)))) %>%
    purrr::pmap(\(x, y, z) purrr::reduce(list(x, y, z), ~ merge(.x, .y, all = TRUE))) %>% # plyr::rbind.fill()
    purrr::map2(test, \(x, y) dplyr::filter(x, paramHeader == y[[1]] & param == y[[2]])) %>%
    purrr::reduce(merge, all = TRUE)


  # Merge each data frame into one table ------------------------------------
  table <- list(results, warn_err) %>%
    purrr::reduce(merge, all = TRUE) %>%
    dplyr::mutate(param = ifelse(str_ends(param, "W"), paste(name, param, sep = "_"), param)) %>%
    dplyr::mutate(param = dplyr::coalesce(param, name)) %>%
    dplyr::mutate(sig = case_when(
      WaldChiSq_PValue < 0.001 ~ "***",
      WaldChiSq_PValue < 0.01 ~ "**",
      WaldChiSq_PValue < 0.05 ~ "*"
    )) %>%
    dplyr::mutate(dplyr::across(c(tidyselect::where(is.numeric), -pval, -dplyr::ends_with("PValue")), ~ round(.x, digits = 2))) %>%
    dplyr::select(LatentClass, paramHeader, param, est, se, pval, low2.5, up2.5, dplyr::starts_with("Wald"), sig, warnings, errors) %>%
    dplyr::arrange(LatentClass, paramHeader, param, desc(est))

  return(table)
}