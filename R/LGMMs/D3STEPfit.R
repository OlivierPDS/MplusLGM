D3STEPfit <- function(list_mpobj, std = "unstd") {
  
  # Test arguments ----------------------------------------------------------
  # list_mpobj <- D3STEP_models
  # std <- "stdyx"
  # ref <- 2
  
  # Validate arguments & define values   ------------------------------------
  stopifnot(std %in% c("unstd", "stdyx", "stdy", "std"))
  
  std <- switch(std,
    "unstd" = "unstandardized",
    "stdyx" = "stdyx.standardized",
    "stdy" = "stdy.standardized",
    "std" = "std.standardized"
  )
  
  # Extract parameters and confidence intervals -----------------------------
  param <- list_mpobj %>%
    purrr::map(pluck, "results", "parameters", std, .default = NULL) %>%
    purrr::map2_dfr(., names(.), ~ if (!is.null(.x)) {dplyr::mutate(.x, name = stringr::str_to_upper(.y))}) %>%
    dplyr::mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param), param))
  
  ci <- list_mpobj %>% 
    purrr::map(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL) %>%
    purrr::map2_dfr(., names(.), ~ if (!is.null(.x)) {dplyr::mutate(.x, name = stringr::str_to_upper(.y))}) %>%
    dplyr::mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param),param))
  
  param_ci <- merge(param, ci, all = TRUE)

  # Extract/compute probability scale and Wald test ---------------------------------
   probs <- list_mpobj %>%
    purrr::map(pluck, "results", "parameters", "probability.scale", .default = NULL) %>% 
    purrr::map2_dfr(., names(.), ~ if (!is.null(.x)) {dplyr::mutate(.x, name = .y)}) %>%
    tryCatch(
      expr = 
        dplyr::mutate(., low2.5 = est - 1.96 * se, up2.5 = est + 1.96 * se) %>%
        dplyr::mutate(dplyr::across(c('est', 'low2.5', 'up2.5'), .names = 'PRs_{.col}'), .keep = "unused") %>% 
        dplyr::mutate(param = paste0(param, "$", category), .keep = "unused") %>% 
        dplyr::mutate(name = stringr::str_to_upper(name)) %>%
        dplyr::select(-se, -est_se),
      error = function(e)
        .
    ) # return empty table if NULL
  
  wt <- list_mpobj %>%
    purrr::map_dfr(pluck, "results", "summaries", .default = NULL) %>%
    dplyr::select(dplyr::starts_with("Wald")) %>%
    dplyr::mutate(name = stringr::str_to_upper(names(list_mpobj)))
  
  # Extract warnings and errors ---------------------------------------------
  warnings <- list_mpobj %>%
    purrr::map(pluck, "results", "warnings", .default = NULL) %>%
    purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>%
    purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>%
    purrr::map(~ paste(.x, collapse = " ")) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_longer(cols = everything(), names_to = "name", values_to = "warnings") %>%
    dplyr::mutate(name = stringr::str_to_upper(name))
  
  errors <- list_mpobj %>%
    purrr::map(pluck, "results", "errors") %>%
    purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>%
    purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>%
    purrr::map(~ paste(.x, collapse = " ")) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_longer(cols = everything(), names_to = "name", values_to = "errors") %>%
    dplyr::mutate(name = stringr::str_to_upper(name))
  
  # Merge each data frame into one table ------------------------------------
  table <- list(param_ci, probs, wt, warnings, errors) %>%
    purrr::compact() %>%
    purrr::reduce(merge, all = TRUE) %>%
    tryCatch(
      expr = 
        dplyr::filter(., paramHeader == "Thresholds" | paramHeader == "New.Additional.Parameters" |
          (paramHeader == "Means" & stringr::str_detect(param, "[:alpha:]#\\d", negate = TRUE))) %>%
        dplyr::mutate(sig = dplyr::case_when(
          WaldChiSq_PValue < 0.001 ~ "***", 
          WaldChiSq_PValue < 0.01 ~ "**", 
          WaldChiSq_PValue < 0.05 ~ "*")) %>% 
        dplyr::select(LatentClass, param, paramHeader, est, se, pval, 
                      low2.5, up2.5, dplyr::starts_with("PRs"), dplyr::starts_with("Wald"), sig, warnings, errors) %>%
        dplyr::arrange(LatentClass, paramHeader, param),
      error = function(e)
        .
    )
  
  return(table)
  
}
