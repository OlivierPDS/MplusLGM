MixREGfit <- function(list_mpobj, std = "unstd") {

# Test arguments ----------------------------------------------------------
# list_mpobj <- TVCreg_models
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
  purrr::map2(., names(.), ~ if (!is.null(.x)) {dplyr::mutate(.x, name = stringr::str_to_upper(.y))}) %>%
  plyr::rbind.fill() %>%  #dplyr::bind_rows fail if est_se residual variance = ****** (can't combine character and double)
  dplyr::mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param), param))
  
ci <- list_mpobj %>% 
  purrr::map(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL) %>% 
  purrr::map2_dfr(., names(.), ~ if (!is.null(.x)) {dplyr::mutate(.x, name = stringr::str_to_upper(.y))}) %>%
  dplyr::mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param), param))

param_ci <- merge(param, ci, all = TRUE) %>% 
  dplyr::mutate(param = ifelse(str_ends(param, "W"), paste(name, param, sep = "_"), param))

# Extract Wald test -------------------------------------------------------
wt <- list_mpobj %>%
  purrr::map_dfr(pluck, "results", "summaries", .default = NULL) %>%
  dplyr::select(dplyr::starts_with("Wald")) %>%
  dplyr::mutate(name = stringr::str_to_upper(names(list_mpobj)))

# Extract warnings and errors ---------------------------------------------
warnings <- list_mpobj %>% 
  purrr::map(pluck, "results", "warnings") %>% 
  purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
  purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
  purrr::map(~ paste(.x, collapse = " ")) %>% 
  purrr::map_dfc(pluck) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "name", values_to = 'warnings')  

errors <- list_mpobj %>% 
  purrr::map(pluck, "results", "errors") %>% 
  purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
  purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
  purrr::map(~ paste(.x, collapse = " ")) %>% 
  purrr::map_dfc(pluck) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "name", values_to = 'errors')

# Merge each data frame into one table ------------------------------------
table <- list(param_ci, wt, warnings, errors) %>%
  purrr::reduce(merge, all = TRUE) %>%
  tryCatch(
    expr =
      dplyr::filter(., paramHeader == "New.Additional.Parameters" | stringr::str_detect(paramHeader, ".+ON")) %>%
      mutate(sig = ifelse(exists("WaldChiSq_PValue"), 
                          dplyr::case_when(WaldChiSq_PValue < 0.001 ~ "***",
                                           WaldChiSq_PValue < 0.01 ~ "**",
                                           WaldChiSq_PValue < 0.05 ~ "*"),
                          NA)) %>% 
      dplyr::mutate(dplyr::across(c(tidyselect::where(is.numeric), -pval, -dplyr::ends_with("PValue")), ~ round(.x, digits = 2))) %>%
      dplyr::select(LatentClass, paramHeader, param, est, se, pval, low2.5, up2.5, dplyr::starts_with("Wald"), sig, warnings, errors) %>%
      dplyr::arrange(LatentClass, paramHeader, param, -est),
    error = function(e)
      .
  )

return(table)
}