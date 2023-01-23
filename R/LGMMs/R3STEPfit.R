R3STEPfit <- function(list_mpobj) {
  
  unstd <- list_mpobj %>%
    map(pluck, "results", "parameters", "unstandardized", .default = NULL) %>% 
    map2_dfr(., names(list_mpobj), ~ mutate(.x, name = .y)) %>% 
    mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, name), paramHeader), .keep = "unused")
  
  ci <- list_mpobj %>% 
    map(pluck, "results", "parameters", "ci.unstandardized", .default = NULL) %>% 
    map2_dfr(., names(list_mpobj), ~ mutate(.x, name = .y)) %>% 
    mutate(paramHeader = ifelse(paramHeader == "Intercepts", paste(paramHeader, name), paramHeader), .keep = "unused")
  
  unstd_ci <- merge(unstd, ci, all = TRUE) %>%
    tryCatch(
      expr = filter(., str_detect(LatentClass, "Categorical.Latent.Variables")) %>%
        select(., !matches("^(low|up)\\.?5")),
      error = function(e)
        .
    ) 
  
  warnings <- list_mpobj %>% 
    map(pluck, "results", "warnings") %>% 
    modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
    map_dfc(pluck) %>% 
    pivot_longer(cols = everything(), names_to = "param", values_to = 'warnings') %>% 
    mutate(param = str_to_upper(param))
  
  errors <- list_mpobj %>% 
    map(pluck, "results", "errors") %>% 
    modify_if(~ length(.x) > 0, paste, .else = ~ NA) %>% 
    map_dfc(pluck) %>% 
    pivot_longer(cols = everything(), names_to = "param", values_to = 'errors') %>% 
    mutate(param = str_to_upper(param))
  
  
  #     line1_alt <-  output_alt %>% str_which("C#\\d\\s+ON") #get lines of regression coefs and ORs
  #     line2_alt <-  output_alt %>% str_which("Intercepts") #get lines of regression coefs and ORs
  #     
  #     
  #     coef_alt[[i]] <-
  #       output_alt[line1_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))] + 1] %>% #Manual R3STEP
  #       as.data.frame() %>%
  #       setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
  #       str_split('[:space:]+', simplify = TRUE) %>%
  #       mutate(class = str_c('C#', parse_number(output_alt[line_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))]]))) %>% 
  #       mutate(ref = str_c('Ref C#', rep(seq(k-1), each=k-1)))
  # 
  #     intercepts_alt[[i]] <-
  #       output_alt[sort(as.vector(outer(line2_alt, seq_along(line2_alt), '+')))] %>% #Manual R3STEP
  #       str_split('[:space:]+', simplify = TRUE) %>%
  #       as.data.frame() %>%
  #       select(-1) %>%
  #       setNames(c('class', 'estimate', 'se', 'tval', 'pval')) %>%
  #       mutate(class = str_replace(class, 'C#', 'intercept')) %>% 
  #       mutate(ref = str_c('Ref C#', rep(seq(k-1), each=k-1))) %>% 
  #       mutate('cov' = str_to_upper(i))
  #     
  #     ORs_alt[[i]] <-
  #       output_alt[line1_alt[c(rep(FALSE, (k-1)*(k-1)), rep(TRUE, (k-1)*(k-1)))] + 1] %>% #Manual R3STEP
  #       str_split('[:space:]+', simplify = TRUE) %>%
  #       as.data.frame() %>%
  #       setNames(c('class', 'cov', 'OR', 'OR_se', '[OR_CI', 'OR_CI]')) %>%
  #       mutate(class = str_c('C#', parse_number(output_alt[line_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))]]))) %>% 
  #       mutate(ref = str_c('Ref C#', rep(seq(k-1), each=k-1)))
  #     
  # }
  
  # table_alt <- list(coef_alt, intercepts_alt, ORs_alt, warn_err) %>%
  #   map(reduce, merge, all = TRUE) %>%
  #   reduce(merge, all = TRUE) %>%
  #   select('cov', everything()) %>%
  #   arrange(ref, cov)
  
  # Merge each data frame into one table
  table <- list(unstd_ci, warnings, errors) %>%
    reduce(merge, all = TRUE) %>%
    tryCatch(
      expr = 
        select(., paramHeader, param, everything(), -LatentClass) %>%
        arrange(., paramHeader, param) %>% 
        mutate(sig = case_when(pval < 0.001 ~ "***",
                               pval < 0.01 ~ "**",
                               pval < 0.05 ~ "*")),
      error = function(e)
        .
    )
  
  return(table)
}
