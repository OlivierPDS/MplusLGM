D3STEPfit <- function(list_mpobj){
  
  unstd <- list_mpobj %>%
    discard( ~ str_detect(.[["VARIABLE"]], "CATEGORICAL")) %>% 
    map(pluck, "results", "parameters", "unstandardized", .default = NULL) %>% 
    map2_dfr(., names(.), ~ mutate(.x, name = .y)) %>% 
    mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param), param))
  
  ci <- list_mpobj %>%
    discard( ~ str_detect(.[["VARIABLE"]], "CATEGORICAL")) %>% 
    map(pluck, "results", "parameters", "ci.unstandardized", .default = NULL) %>% 
    map2_dfr(., names(.), ~ mutate(.x, name = .y)) %>% 
    mutate(param = ifelse(paramHeader == "New.Additional.Parameters", paste(name, param), param))
    
  unstd_ci <- merge(unstd, ci, all = TRUE) %>% 
    tryCatch(
      expr = filter(., str_detect(param, "[:alpha:]#\\d", negate = TRUE)) %>% select(., !matches("^(low|up)\\.?5")),
      error = function(e)
        .
    ) 
    
  probs <- list_mpobj %>%
    map_dfr(pluck, "results", "parameters", "probability.scale", .default = NULL) %>% 
    tryCatch(
      expr = mutate(., paramHeader = str_c("Category", category), .keep = "unused"),
      error = function(e)
        .
    ) 
    
  wt <- list_mpobj %>% 
    map_dfr(pluck, "results", "summaries", .default = NULL) %>% 
    select(starts_with("Wald")) %>% 
    mutate(param = str_to_upper(names(list_mpobj)))
  
  warnings <- list_mpobj %>% 
    map(pluck, "results", "warnings", .default = NULL) %>% 
    modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
    bind_rows() %>% 
    pivot_longer(cols = everything(), names_to = "param", values_to = "warnings") %>% 
    mutate(param = str_to_upper(param))
  
  errors <- list_mpobj %>% 
    map(pluck, "results", "errors") %>% 
    modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
    bind_rows() %>% 
    pivot_longer(cols = everything(), names_to = "param", values_to = "errors") %>% 
    mutate(param = str_to_upper(param))

  table <- list(unstd_ci, probs, wt, warnings, errors) %>%
    reduce(merge, all = TRUE) %>%
    tryCatch(
      expr = 
        select(., param, paramHeader, LatentClass, everything()) %>%
        arrange(., param, LatentClass)%>% 
        mutate(sig = case_when(WaldChiSq_PValue < 0.001 ~ "***",
                               WaldChiSq_PValue < 0.01 ~ "**",
                               WaldChiSq_PValue < 0.05 ~ "*")),
      error = function(e)
        .
    ) 
    

  return(table)
}
