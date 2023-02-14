R3STEPfit <- function(list_mpobj, std = "unstd") {
  
# Test arguments ----------------------------------------------------------
  # list_mpobj <- R3STEP_models
  # std <- "stdyx"


# Validate arguments & define values   ------------------------------------
  stopifnot(std %in% c("unstd", "stdyx", "stdy", "std"))
  
  std <- switch(std,
                "unstd" = "unstandardized",
                "stdyx" = "stdyx.standardized", 
                "stdy" = "stdy.standardized",
                "std" = "std.standardized")
  
# Extract parameters and confidence intervals -----------------------------
  param <- list_mpobj %>%
    purrr::map(pluck, "results", "parameters", std, .default = NULL) %>% 
    purrr::map2_dfr(., names(list_mpobj), ~ dplyr::mutate(.x, name = .y)) %>% 
    dplyr::mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, name), paramHeader), .keep = "unused")
  
  ci <- list_mpobj %>% 
    purrr::map(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL) %>% 
    purrr::map2_dfr(., names(list_mpobj), ~ dplyr::mutate(.x, name = .y)) %>% 
    dplyr::mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, name), paramHeader), .keep = "unused")
  
  param_ci <- merge(param, ci, all = TRUE) %>%
    tryCatch(
      expr = filter(., str_detect(LatentClass, "Categorical.Latent.Variables")) %>%
        dplyr::select(., !matches("^(low|up)\\.?5")),
      error = function(e)
        .
    ) 

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

# Alternative parametrization ---------------------------------------------
  #     line1_alt <-  output_alt %>% stringr::str_which("C#\\d\\s+ON") #get lines of regression coefs and ORs
  #     line2_alt <-  output_alt %>% stringr::str_which("Intercepts") #get lines of regression coefs and ORs
  #     
  #     coef_alt[[i]] <-
  #       output_alt[line1_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))] + 1] %>% #Manual R3STEP
  #       as.data.frame() %>%
  #       setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
  #       stringr::str_split('[:space:]+', simplify = TRUE) %>%
  #       dplyr::mutate(class = stringr::str_c('C#', parse_number(output_alt[line_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))]]))) %>% 
  #       dplyr::mutate(ref = stringr::str_c('Ref C#', rep(seq(k-1), each=k-1)))
  # 
  #     intercepts_alt[[i]] <-
  #       output_alt[sort(as.vector(outer(line2_alt, seq_along(line2_alt), '+')))] %>% #Manual R3STEP
  #       stringr::str_split('[:space:]+', simplify = TRUE) %>%
  #       as.data.frame() %>%
  #       dplyr::select(-1) %>%
  #       setNames(c('class', 'estimate', 'se', 'tval', 'pval')) %>%
  #       dplyr::mutate(class = stringr::str_replace(class, 'C#', 'intercept')) %>% 
  #       dplyr::mutate(ref = stringr::str_c('Ref C#', rep(seq(k-1), each=k-1))) %>% 
  #       dplyr::mutate('cov' = stringr::str_to_upper(i))
  #     
  #     ORs_alt[[i]] <-
  #       output_alt[line1_alt[c(rep(FALSE, (k-1)*(k-1)), rep(TRUE, (k-1)*(k-1)))] + 1] %>% #Manual R3STEP
  #       stringr::str_split('[:space:]+', simplify = TRUE) %>%
  #       as.data.frame() %>%
  #       setNames(c('class', 'cov', 'OR', 'OR_se', '[OR_CI', 'OR_CI]')) %>%
  #       dplyr::mutate(class = stringr::str_c('C#', parse_number(output_alt[line_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))]]))) %>% 
  #       dplyr::mutate(ref = stringr::str_c('Ref C#', rep(seq(k-1), each=k-1)))
  #     
  # table_alt <- list(coef_alt, intercepts_alt, ORs_alt, warn_err) %>%
  #   purrr::map(purrr::reduce, merge, all = TRUE) %>%
  #   purrr::reduce(merge, all = TRUE) %>%
  #   dplyr::select('cov', everything()) %>%
  #   dplyr::arrange(ref, cov)
  

# Merge parameters, confidence intervals, warnings and errors into --------
  table <- list(param_ci, warnings, errors) %>%
    purrr::reduce(merge, all = TRUE) %>%
    tryCatch(
      expr = 
        dplyr::select(., paramHeader, param, everything(), -LatentClass) %>%
        dplyr::arrange(., paramHeader, param) %>% 
        dplyr::mutate(sig = dplyr::case_when(pval < 0.001 ~ "***",
                               pval < 0.01 ~ "**",
                               pval < 0.05 ~ "*")),
      error = function(e)
        .
    )
  
  return(table)
}
