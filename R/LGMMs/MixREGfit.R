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
              "std" = "std.standardized")

# Extract parameters and confidence intervals -----------------------------
param <- list_mpobj %>%
  purrr::map(pluck, "results", "parameters", std, .default = NULL) %>% 
  plyr::rbind.fill() #dplyr::bind_rows fail if est_se residual variance = ****** (can't combine character and double)
  
ci <- list_mpobj %>% 
  purrr::map_dfr(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL) 

param_ci <- merge(param, ci, all = TRUE)

# Extract warnings and errors ---------------------------------------------
warnings <- list_mpobj %>% 
  purrr::map(pluck, "results", "warnings") %>% 
  purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
  purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
  purrr::map(~ paste(.x, collapse = " ")) %>% 
  purrr::map_dfc(pluck) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "param", values_to = 'warnings')  

errors <- list_mpobj %>% 
  purrr::map(pluck, "results", "errors") %>% 
  purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
  purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
  purrr::map(~ paste(.x, collapse = " ")) %>% 
  purrr::map_dfc(pluck) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "param", values_to = 'errors')


# Merge each data frame into one table ------------------------------------
table <- list(param_ci, warnings, errors) %>%
  purrr::reduce(merge, all = TRUE) %>%
  tryCatch(
    expr =
      dplyr::filter(., (stringr::str_detect(paramHeader, ".+ON") | !is.na(warnings) | !is.na(errors))) %>% 
      dplyr::mutate(
        sig = dplyr::case_when(pval < 0.001 ~ "***",
                               pval < 0.01 ~ "**",
                               pval < 0.05 ~ "*")) %>%
      dplyr::select(LatentClass, paramHeader, param, est, se, est_se, pval, sig, low2.5, up2.5, warnings, errors) %>%
      dplyr::arrange(LatentClass, param),
    error = function(e)
      .
  )

return(table)
}