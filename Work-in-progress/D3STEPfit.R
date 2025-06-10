D3STEPfit <- function(list_mpobj, std = "unstd") {
  # Test arguments ----------------------------------------------------------
  # list_mpobj <- D3STEP_models
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
  
  # Extract parameters, confidence intervals, variances and Wald test -----------------------------
  param <- sublist_mpobj %>%
    purrr::map(pluck, "results", "parameters", std, .default = NULL)

  ci <- sublist_mpobj %>%
    purrr::map(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL)

  wt <- sublist_mpobj %>%
    purrr::map(pluck, "results", "summaries", .default = NULL)

  mean_obs <- sublist_mpobj %>% 
    purrr::map(~ pluck(.x, "rdata")) %>% 
    purrr::map(~ filter(.x, !is.na(N))) %>% 
    purrr::map(~ mutate(.x, across(everything(), as.numeric))) %>% 
    purrr::map(~ group_by(.x, N)) %>% 
    purrr::map(\(x) summarise(x, across(everything(), ~ mean(.x, na.rm = TRUE)))) %>% 
    purrr::imap(\(x, idx) select(x, c(N, all_of(idx)))) %>% 
    purrr::imap(\(x, idx) rename(x, LatentClass = N, mean_obs = idx))
    
  tech12 <- sublist_mpobj %>%
    purrr::discard(stringr::str_detect(purrr::map(sublist_mpobj, ~ purrr::pluck(.x, "VARIABLE")), "CATEGORICAL")) %>%
    purrr::map(pluck, "results", "tech12", .default = NULL) %>%
    purrr::map(as.data.frame) %>% 
    purrr::map(\(x) rename_with(x, ~ c("Observed Means", "Estimated Mixed Means", "Residuals for Mixed Means", 
                                    "Observed Covariances", "Estimated Mixed Covariances", "Residuals for Mixed Covariances", 
                                    "Observed Skewness", "Estimated Mixed Skewness", "Residuals for Mixed Skewness", 
                                    "Observed Kurtosis","Estimated Mixed Kurtosis", "Residuals for Mixed Kurtosis"), everything())) %>% 
    purrr::imap(\(x, idx) dplyr::mutate(x, name = stringr::str_to_upper(idx)))
  
  # probs <- sublist_mpobj %>%
  #   purrr::map(pluck, "results", "parameters", "probability.scale", .default = NULL) %>%
  #   purrr::map2_dfr(., names(.), ~ if (!is.null(.x)) {
  #     dplyr::mutate(.x, name = .y)
  #   }) %>%
  #   dplyr::mutate(., low2.5 = est - 1.96 * se, up2.5 = est + 1.96 * se) %>%
  #   dplyr::mutate(dplyr::across(c("est", "pval", "low2.5", "up2.5"), .names = "PRs_{.col}"), .keep = "unused") %>%
  #   dplyr::mutate(param = paste0(param, "$", category), .keep = "unused") %>%
  #   dplyr::mutate(name = stringr::str_to_upper(name)) %>%
  #   dplyr::select(-se, -est_se)

  results <- list(param, ci, wt, mean_obs) %>%
    purrr::map(~ purrr::imap(.x, \(x, idx) dplyr::mutate(x, name = stringr::str_to_upper(idx)))) %>% 
    purrr::pmap(\(w, x, y, z) purrr::reduce(list(w, x, y, z), ~ merge(.x, .y,  all = TRUE))) # plyr::rbind.fill()

  # Compute odds ratios -----------------------------------------------------
  OR <- results %>%
    purrr::keep(stringr::str_detect(purrr::map(sublist_mpobj, ~ purrr::pluck(.x, "VARIABLE")), "CATEGORICAL")) %>%
    purrr::map(~ dplyr::mutate(
      .x,
      dplyr::across(c(est, low2.5, up2.5),
        ~ ifelse(stringr::str_detect(param, "DIFF"),
          exp(-(.x)),
          NA
        ),
        .names = "OR_{.col}"
      )
    )) %>%
    purrr::map(~ dplyr::mutate(.x, OR_low2.5 = OR_up2.5, OR_up2.5 = .$OR_low2.5))

  # Compute standardized mean difference ------------------------------------
  SMD <- results %>%
    purrr::discard(stringr::str_detect(purrr::map(sublist_mpobj, ~ purrr::pluck(.x, "VARIABLE")), "CATEGORICAL")) %>%
    purrr::map(~ dplyr::mutate(.x, dplyr::across(tidyselect::where(is.character), ~ dplyr::na_if(.x, "*********")))) %>%
    purrr::map2(tech12, \(results, tech12) merge(results, tech12, all = TRUE)) %>% 
    purrr::map(~ dplyr::mutate(.x, dplyr::across(c(est, low2.5, up2.5),
      ~ ifelse(stringr::str_detect(param, "DIFF"),
        as.numeric(.x) / sqrt(`Estimated Mixed Covariances`),
        NA
      ),
      .names = "SMD_{.col}"
    )))

  OR_SMD <- c(OR, SMD) %>%
    purrr::imap(\(x, idx) dplyr::mutate(x, param = ifelse(paramHeader == "New.Additional.Parameters", paste(stringr::str_to_upper(idx), param), param))) %>%
    purrr::reduce(merge, all = TRUE)

  # Merge each data frame into one table ------------------------------------
  table <- list(OR_SMD, warn_err) %>%
    purrr::reduce(full_join, by = "name") %>%
    dplyr::mutate(param = dplyr::coalesce(param, name)) %>%
    dplyr::filter(!is.na(errors) | paramHeader == "New.Additional.Parameters" | (paramHeader == "Means" & stringr::str_detect(param, "[:alpha:]#\\d", negate = TRUE)) | paramHeader == "Thresholds") %>%
    dplyr::mutate(sig = dplyr::case_when(
      WaldChiSq_PValue < 0.001 ~ "***",
      WaldChiSq_PValue < 0.01 ~ "**",
      WaldChiSq_PValue < 0.05 ~ "*"
    )) %>%
    dplyr::mutate(dplyr::across(c(tidyselect::where(is.numeric), -pval, -dplyr::ends_with("PValue")), ~ round(.x, digits = 2))) %>%
    dplyr::select(LatentClass, param, paramHeader, mean_obs, est, se, pval, low2.5, up2.5, dplyr::starts_with("SMD"), dplyr::starts_with("OR"), dplyr::starts_with("Wald"), sig, warnings, errors) %>%
    dplyr::arrange(param, LatentClass, desc(est), paramHeader)

  return(table)
}
