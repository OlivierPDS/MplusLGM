MixREGfit <- function(list_mpobj) {
  
      stdyx <- list_mpobj %>%
        map_dfr(pluck, "results", "parameters", "stdyx.standardized", .default = NULL)
      
      ci <- list_mpobj %>% 
        map_dfr(pluck, "results", "parameters", "ci.stdyx.standardized", .default = NULL)
      
      stdyx_ci <- merge(stdyx, ci, all = TRUE) %>%
        tryCatch(
          expr = filter(., str_detect(paramHeader, ".+ON")) %>%
            select(., !matches("^(low|up)\\.?5")),
          error = function(e)
            .
        ) 
        
      # mutate(ORs = case_when(!is.na(estimate) ~ exp(as.numeric(estimate)))) %>%  #compute OR and 95%CI
    
      warnings <- list_mpobj %>% 
        map(pluck, "results", "warnings") %>% 
        modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
        map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
        map(~ paste(.x, collapse = " ")) %>% 
        map_dfc(pluck) %>% 
        pivot_longer(cols = everything(), names_to = "param", values_to = 'warnings')  
      
      errors <- list_mpobj %>% 
        map(pluck, "results", "errors") %>% 
        modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
        map(~ gsub("[^[:alnum:][:space:]]", "", .x)) %>% 
        map(~ paste(.x, collapse = " ")) %>% 
        map_dfc(pluck) %>% 
        pivot_longer(cols = everything(), names_to = "param", values_to = 'errors')
  
  # Merge each data frame into one table
      table <- list(stdyx_ci, warnings, errors) %>%
        reduce(merge, all = TRUE) %>%
        tryCatch(
          expr = 
            select(., paramHeader, param, LatentClass, everything()) %>%
            arrange(., param, LatentClass)%>% 
            mutate(sig = case_when(pval < 0.001 ~ "***",
                                   pval < 0.01 ~ "**",
                                   pval < 0.05 ~ "*")),
          error = function(e)
            .
        )
             
  return(table)
}