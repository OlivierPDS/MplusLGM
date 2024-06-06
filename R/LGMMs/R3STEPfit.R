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

  k <- purrr::pluck(list_mpobj, 1, "results", "summaries", "NLatentClasses")
  
  if (is.null(ref)) {
    ref <- k
  }
  
  stopifnot(ref %in% seq(k))

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

  if (ref == last(seq(k))) {
    param <- purrr::map(sublist_mpobj, ~ purrr::pluck(.x, "results", "parameters", std, .default = NULL))
  } else {
    param <- purrr::map(sublist_mpobj, ~ purrr::pluck(.x, "results", "parameters", "unstandardized.alt", glue::glue("ref.cat.{ref}"), .default = NULL))
  }
  
  mean_est <- sublist_mpobj %>% 
    purrr::map(~ pluck(.x, "results", "tech7")) %>% 
    purrr::map_depth(2, ~ pluck(.x, "classSampMeans")) %>%
    purrr::map(~ as.data.frame(.x)) %>% 
    purrr::map(~ rename_with(.x, ~ paste0(c('mean_est'), seq_along(.x))))
  
  mean_obs <- sublist_mpobj %>% 
    purrr::map(~ pluck(.x, "rdata")) %>% 
    purrr::map(~ filter(.x, !is.na(N))) %>% 
    purrr::map(~ mutate(.x, across(everything(), as.numeric))) %>% 
    purrr::map(~ group_by(.x, N)) %>% 
    purrr::map(\(x) summarise(x, across(everything(), ~ mean(.x, na.rm = TRUE)))) %>% 
    purrr::imap(\(x, idx) select(x, c(N, all_of(idx)))) %>% 
    purrr::imap(\(x, idx) pivot_wider(x, 
                      names_from = N,
                      names_glue = 'mean_obs{N}',
                      values_from = idx))
  
  # desc <- sublist_mpobj %>%
  #   purrr::map(~ pluck(.x, "rdata")) %>%
  #   purrr::reduce(merge, all = TRUE) %>%
  #   tbl_summary(
  #     by = N,
  #     statistic = list(
  #       all_continuous() ~ "{mean} ({sd})",
  #       all_categorical() ~ "{n} ({p}%)"
  #     ),
  #     digits = list(
  #       all_continuous() ~ 1,
  #       all_categorical() ~ 0
  #     ),
  #     missing = "no"
  #   ) %>%
  #   as_tibble()
  
  results <- list(param, mean_obs, mean_est) %>%
    purrr::map(~ purrr::imap(.x, \(x, idx) dplyr::mutate(x, name = stringr::str_to_upper(idx)))) %>% 
    purrr::pmap(\(x, y, z) purrr::reduce(list(x, y, z), ~ merge(.x, .y,  all = TRUE))) %>% # plyr::rbind.fill()
    purrr::reduce(merge, all = TRUE) %>% 
    dplyr::mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, param), paramHeader)) %>%
    dplyr::mutate(param = ifelse(stringr::str_detect(paramHeader, "^Intercepts"), name, param)) %>%
    dplyr::mutate(param = ifelse(stringr::str_detect(param, "N#\\d"), paste(name, param), param))
  
  # Compute odd ratio and confidence intervals -----------------------------
  or_ci <- results %>%
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
    purrr::reduce(~ full_join(.x, .y, by = "name")) %>%
    dplyr::mutate(param = dplyr::coalesce(param, name)) %>%
    dplyr::filter(stringr::str_detect(paramHeader, "C#\\d") | !is.na(errors)) %>% # filter out irrelevant parameters
    dplyr::mutate(sig = dplyr::case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*"
    )) %>% # add significativity
    dplyr::mutate(dplyr::across(c(tidyselect::where(is.numeric), -pval), ~ round(.x, digits = 2))) %>%
    dplyr::select(paramHeader, param, est, low2.5, up2.5, se, pval, sig, tidyselect::starts_with("mean"), tidyselect::starts_with("OR"), warnings, errors) %>%
    dplyr::arrange(paramHeader, -est, param)

  return(table)
}
