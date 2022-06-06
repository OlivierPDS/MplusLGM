fitR3STEP <- function(list_mpobj,
                      cov) {
  
  # Create empty lists to store for loop outputs
  coef <- list()
  intercepts <- list()
  ORs <- list()
  warn_err <- list()
  
  for (i in cov) {
    
    k <-  list_mpobj %>%  purrr::pluck(i, "results", "summaries", "NLatentClasses")
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
        errors = ifelse(!is.null(error), paste(error), NA),
        warnings = ifelse(is.null(warning), paste(warning), NA)
      ) %>%
      mutate('cov' = str_to_upper(i))
    
    # return NULL if Mplus warning or errors
    if (is.null(error) == FALSE | is.null(warning) == FALSE) {
      
      coef[[i]] <- NULL
      intercepts[[i]] <- NULL
      ORs[[i]] <- NULL
    
      } else {
      
      output_ref <- output[start:end]
      output_alt <- output[-(1:end-1)]
      
      line1 <-  output_ref %>% str_which("C#\\d\\s+ON") #get lines of regression coefs and ORs
      line2 <-  output_ref %>% str_which("Intercepts") #get lines of regression coefs and ORs
      
      # line_alt <-  output_alt %>% str_which("C#\\d\\s+ON") #get lines of regression coefs and ORs
      
      # Get coef, ORs and intercepts values 
      coef[[i]] <-
        output_ref[line1[c(rep(TRUE, k-1), rep(FALSE, k-1))] + 1] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_c("C#", 1:(k-1)))
      
      # coef_alt[[i]] <-
      #   output_alt[line_alt[c(rep(TRUE, k-1), rep(FALSE, k-1))] + 1] %>% #Manual R3STEP
      #   str_split('[:space:]+', simplify = TRUE) %>%
      #   as.data.frame() %>%
      #   setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
      #   mutate(class = str_subset(output_alt, "C#\\d\\s+ON")[c(rep(TRUE, (k-1)*2), rep(FALSE, (k-1)*2))])
      
      intercepts[[i]] <-
        output_ref[line2 + 1:(k-1)] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        select(-1) %>%
        setNames(c('class', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_c("intercept", 1:(k-1))) %>%
        mutate('cov' = str_to_upper(i))
      
      ORs[[i]] <-
        output_ref[line1[c(rep(FALSE, k-1), rep(TRUE, k-1))] + 1] %>% #Manual R3STEP
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'OR', 'OR_se', '[OR_CI', 'OR_CI]')) %>%
        mutate(class = str_c("C#", 1:(k-1)))
    }
  }
  
  # Merge each data frame into one table
  table <- list(coef, intercepts, ORs, warn_err) %>%
    map(reduce, merge, all = TRUE) %>%
    reduce(merge, all = TRUE) %>%
    select('cov', everything()) %>%
    arrange(cov)
  
  return(table)
}
