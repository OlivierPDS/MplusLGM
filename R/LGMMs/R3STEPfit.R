R3STEPfit <- function(list_mpobj,
                      cov) {
  
  # Create empty lists to store for loop outputs
  coef <- list()
  intercepts <- list()
  ORs <- list()
  warn_err <- list()
  coef_alt <- list()
  intercepts_alt <- list()
  ORs_alt <- list()
  
  for (i in cov) {
    k <- list_mpobj %>%  purrr::pluck(i, "results", "summaries", "NLatentClasses")
    output <- pluck(list_mpobj, i, 'results', 'output')
    
    start <-  output %>% 
      str_which("Categorical Latent Variables") %>% 
      first()
    
    end <-  output %>% 
      str_which("ALTERNATIVE PARAMETERIZATIONS") %>% 
      first()
    
    # Get Mplus warnings and errors 
    warning <- pluck(list_mpobj, i, 'results', 'warnings')
    error <- pluck(list_mpobj, i, 'results', 'errors')
    
    warn_err[[i]] <-
      data.frame(
        errors = ifelse(is.null(error), NA, paste(error)),
        warnings = ifelse(is.null(warning), NA, paste(warning))
      ) %>%
      mutate('cov' = str_to_upper(i))
    
    # return NULL if Mplus warning or errors
    if (is.null(error) == FALSE) {
      
      coef[[i]] <- NULL
      coef_alt[[i]] <- NULL
      intercepts[[i]] <- NULL
      intercepts_alt[[i]] <- NULL
      ORs[[i]] <- NULL
      ORs_alt[[i]] <- NULL
    
      } else {
      
      output_ref <- output[start:end]
      output_alt <- output[-(1:end-1)]
      
      line1 <-  output_ref %>% str_which("C#\\d\\s+ON") #get lines of regression coefs and ORs
      line2 <-  output_ref %>% str_which("Intercepts") #get lines of regression coefs and ORs
      
      line1_alt <-  output_alt %>% str_which("C#\\d\\s+ON") #get lines of regression coefs and ORs
      line2_alt <-  output_alt %>% str_which("Intercepts") #get lines of regression coefs and ORs
      
      # Get coef, ORs and intercepts values 
      coef[[i]] <-
        output_ref[line1[c(rep(TRUE, k-1), rep(FALSE, k-1))] + 1] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_c("C#", 1:(k-1))) %>% 
        mutate(ref = str_c('Ref C#', k))
      
      coef_alt[[i]] <-
        output_alt[line1_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))] + 1] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_c('C#', parse_number(output_alt[line_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))]]))) %>% 
        mutate(ref = str_c('Ref C#', rep(seq(k-1), each=k-1)))
  
      intercepts[[i]] <-
        output_ref[line2 + 1:(k-1)] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        select(-1) %>%
        setNames(c('class', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_c("intercept", 1:(k-1))) %>%
        mutate('cov' = str_to_upper(i)) %>% 
        mutate(ref = str_c('Ref C#', k))
      
      intercepts_alt[[i]] <-
        output_alt[sort(as.vector(outer(line2_alt, seq_along(line2_alt), '+')))] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        select(-1) %>%
        setNames(c('class', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_replace(class, 'C#', 'intercept')) %>% 
        mutate(ref = str_c('Ref C#', rep(seq(k-1), each=k-1))) %>% 
        mutate('cov' = str_to_upper(i))
      
      ORs[[i]] <-
        output_ref[line1[c(rep(FALSE, k-1), rep(TRUE, k-1))] + 1] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'OR', 'OR_se', '[OR_CI', 'OR_CI]')) %>%
        mutate(class = str_c("C#", 1:(k-1))) %>% 
        mutate(ref = str_c('Ref C#', k))
      
      ORs_alt[[i]] <-
        output_alt[line1_alt[c(rep(FALSE, (k-1)*(k-1)), rep(TRUE, (k-1)*(k-1)))] + 1] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'OR', 'OR_se', '[OR_CI', 'OR_CI]')) %>%
        mutate(class = str_c('C#', parse_number(output_alt[line_alt[c(rep(TRUE, (k-1)*(k-1)), rep(FALSE, (k-1)*(k-1)))]]))) %>% 
        mutate(ref = str_c('Ref C#', rep(seq(k-1), each=k-1)))
    }
  }
  
  # Merge each data frame into one table
  table <- list(coef, intercepts, ORs, warn_err) %>%
    map(reduce, merge, all = TRUE) %>%
    reduce(merge, all = TRUE) %>%
    select('cov', everything()) %>%
    arrange(cov)
  
  table_alt <- list(coef_alt, intercepts_alt, ORs_alt, warn_err) %>%
    map(reduce, merge, all = TRUE) %>%
    reduce(merge, all = TRUE) %>%
    select('cov', everything()) %>%
    arrange(ref, cov)
  
  list_fit <- list('Parameterization using last reference class' = table, 'Alternative parameterization' = table_alt) 
  
  return(list_fit)
}
