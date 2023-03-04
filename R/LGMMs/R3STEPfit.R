R3STEPfit <- function(list_mpobj, 
                      std = "unstd", 
                      ref = 1) {
  
# Test arguments ----------------------------------------------------------
  # list_mpobj <- R3STEP_models
  # std <- "stdyx"
  # ref <- 2


# Validate arguments & define values   ------------------------------------
  stopifnot(std %in% c("unstd", "stdyx", "stdy", "std"))
  
  std <- switch(std,
                "unstd" = "unstandardized",
                "stdyx" = "stdyx.standardized", 
                "stdy" = "stdy.standardized",
                "std" = "std.standardized")
  
  stopifnot(ref %in% c("1", "2"))
  
# Extract parameters and confidence intervals -----------------------------
  param <- list_mpobj %>%
    purrr::map(pluck, "results", "parameters", std, .default = NULL) %>% 
    purrr::map2_dfr(., names(list_mpobj), \(x, y) if(!is.null(x)){dplyr::mutate(x, name = stringr::str_to_upper(y))}) %>% 
    dplyr::mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, param), paramHeader)) %>%
    dplyr::mutate(param = ifelse(str_detect(paramHeader, "^Intercepts"), name, param)) %>% 
    dplyr::mutate(param = ifelse(str_detect(param, "N#\\d"), paste(name, param), param))
  
  ci <- list_mpobj %>% 
    purrr::map(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL) %>% 
    purrr::map2_dfr(., names(list_mpobj), \(x, y) if(!is.null(x)){dplyr::mutate(x, name = stringr::str_to_upper(y))}) %>% 
    dplyr::mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, param), paramHeader)) %>%
    dplyr::mutate(param = ifelse(str_detect(paramHeader, "^Intercepts|^N#\\d"), name, param)) %>% 
    dplyr::mutate(param = ifelse(str_detect(param, "N#\\d"), paste(name, param), param))
  
  param_ci <- merge(param, ci, all = TRUE)

# Extract warnings and errors ---------------------------------------------
  warnings <- list_mpobj %>% 
    purrr::map(pluck, "results", "warnings") %>% 
    purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
    purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
    purrr::map(~ paste(.x, collapse = " ")) %>% 
    purrr::map_dfc(pluck) %>% 
    tidyr::pivot_longer(cols = everything(), names_to = "param", values_to = 'warnings') %>% 
    dplyr::mutate(param = stringr::str_to_upper(param))
  
  errors <- list_mpobj %>% 
    purrr::map(pluck, "results", "errors") %>% 
    purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
    purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
    purrr::map(~ paste(.x, collapse = " ")) %>% 
    purrr::map_dfc(pluck) %>% 
    tidyr::pivot_longer(cols = everything(), names_to = "param", values_to = 'errors') %>% 
    dplyr::mutate(param = stringr::str_to_upper(param))
    


# Merge parameters, confidence intervals, warnings and errors  ------------
  table <- list(param_ci, warnings, errors) %>%
    purrr::reduce(merge, all = TRUE)
  
# Alternative parameterization using reference class 1 or 2 --------------------
  
  if (ref == 1) {
    table <- table %>%
      tryCatch(
        expr =
          dplyr::mutate(., 
                        dplyr::across(c('low2.5', 'est', 'up2.5'), 
                                      ~ ifelse(str_detect(paramHeader, "C#\\d.ON"), 
                                               round(exp(.x), 2), 
                                               NA), 
                                      .names = 'ORs_{.col}'
          )),
        error = function(e)
          .
      )
  } else if (ref == 2) {
    table <- table %>%
      tryCatch(
        expr =
          dplyr::mutate(., 
                        dplyr::across(c('low2.5', 'est', 'up2.5'), 
                                      ~ ifelse(str_detect(paramHeader, "C#\\d.ON"), 
                                               round(exp(-.x), 2), 
                                               NA), 
                                      .names = 'ORs_{.col}'
                        )) %>%
          mutate(
            paramHeader = str_replace(paramHeader, "1", "2"),
            est = -est,
            ORs_low2.5 = ORs_up2.5,
            ORs_up2.5 = .$ORs_low2.5
          ),
        error = function(e)
          .
      )
  }
  
# Final table --------------------------------------------------------
  table <- table %>%
    tryCatch(
      expr = 
        filter(., if (std == "unstandardized") {
          stringr::str_detect(LatentClass, "Categorical.Latent.Variables")
        } else{
          paramHeader == "Means" |
            stringr::str_detect(param, "N#\\d")
        }) %>%  # filter out irrelevant parameters
        dplyr::mutate(sig = dplyr::case_when(pval < 0.001 ~ "***",
                                             pval < 0.01 ~ "**",
                                             pval < 0.05 ~ "*")) %>%  #add significativity
        dplyr::select(paramHeader, param, est, se, pval, sig, dplyr::starts_with("ORs"), warnings, errors) %>%
        dplyr::arrange(paramHeader, param),
      error = function(e)
        .
    )
  
  return(table)
  
}
