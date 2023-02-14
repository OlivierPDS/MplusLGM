D3STEPfit <- function(list_mpobj, std = "std"){
 
 # Validate arguments & define values   ------------------------------------
stopifnot(std %in% c("unstd", "stdyx", "stdy", "std"))

std <- switch(std,
"unstd" = "unstandardized",
"stdyx" = "stdyx.standardized", 
"stdy" = "stdy.standardized",
"std" = "std.standardized")
  
# Extract parameters and confidence intervals -----------------------------
param <- list_mpobj %>%
   purrr::discard( ~ stringr::str_detect(.[["VARIABLE"]], "CATEGORICAL")) %>% 
   purrr::map(pluck, "results", "parameters", std, .default = NULL) %>% 
   purrr::map2_dfr(., names(.), ~ if(!is.null(.x)) {dplyr::mutate(.x, name = .y)}) %>% 
   dplyr::mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param), param))
  
ci <- list_mpobj %>%
  purrr::discard( ~ stringr::str_detect(.[["VARIABLE"]], "CATEGORICAL")) %>% 
  purrr::map(pluck, "results", "parameters", glue::glue("ci.{std}"), .default = NULL) %>% 
  purrr::map2_dfr(., names(.), ~ if(!is.null(.x)) {dplyr::mutate(.x, name = .y)}) %>% 
  dplyr::mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param), param))
  
param_ci <- merge(param, ci, all = TRUE) %>% 
  tryCatch(
    expr = 
      dplyr::filter(., stringr::str_detect(param, "[:alpha:]#\\d", negate = TRUE)) %>%
      dplyr::select(., !matches("^(low|up)\\.?5")),
    error = function(e)
      .
  ) 
  
probs <- list_mpobj %>%
  purrr::map_dfr(pluck, "results", "parameters", "probability.scale", .default = NULL) %>%
  tryCatch(
    expr = dplyr::mutate(., paramHeader = stringr::str_c("Category", category), .keep = "unused"),
    error = function(e)
      .
  ) # return empty table if NULL
  
wt <- list_mpobj %>% 
  purrr::map_dfr(pluck, "results", "summaries", .default = NULL) %>% 
  dplyr::select(starts_with("Wald")) %>% 
  dplyr::mutate(param = stringr::str_to_upper(names(list_mpobj)))

# Extract warnings and errors ---------------------------------------------
warnings <- list_mpobj %>% 
  purrr::map(pluck, "results", "warnings", .default = NULL) %>% 
  purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
  purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
  purrr::map(~ paste(.x, collapse = " ")) %>% 
  dplyr::bind_rows() %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "param", values_to = "warnings") %>% 
  dplyr::mutate(param = stringr::str_to_upper(param))

errors <- list_mpobj %>% 
  purrr::map(pluck, "results", "errors") %>% 
  purrr::modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
  purrr::map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
  purrr::map(~ paste(.x, collapse = " ")) %>% 
  dplyr::bind_rows() %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "param", values_to = "errors") %>% 
  dplyr::mutate(param = stringr::str_to_upper(param))

# Merge each data frame into one table ------------------------------------
table <- list(param_ci, probs, wt, warnings, errors) %>%
  purrr::compact() %>% 
  purrr::reduce(merge, all = TRUE) %>%
  tryCatch(
    expr = dplyr::select(., param, paramHeader, LatentClass, est, se, est_se, pval, low2.5, up2.5,
                           WaldChiSq_Value, WaldChiSq_DF, WaldChiSq_PValue, warnings, errors) %>%
      dplyr::arrange(., param, LatentClass) %>% 
      dplyr::mutate(sig = dplyr::case_when(WaldChiSq_PValue < 0.001 ~ "***",
                             WaldChiSq_PValue < 0.01 ~ "**",
                             WaldChiSq_PValue < 0.05 ~ "*")),
    error = function(e)
      .
  ) 
  
return(table)
}
