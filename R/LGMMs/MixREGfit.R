MixREGfit <- function(list_mpobj,
         cov){
  
  # Create empty lists to store for loop outputs
  coef <- list()
  intercepts <- list()
  ORs <- list()
  warn_err <- list()
  
  for (i in cov) {
    
    k <-  list_mpobj %>%  purrr::pluck(i, "results", "summaries", "NLatentClasses")
    output <- pluck(list_mpobj, i, 'results', 'output')
    
    start <-  output %>% 
      str_which("Latent Class 1") %>% 
      first()
    
    end <-  output %>% 
      str_which("QUALITY OF NUMERICAL RESULTS") %>% 
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
      
    } else {
      
      output_ref <- output[start:end]
      
      line1 <-  output_ref %>% str_which("S\\s+ON") #get lines of regression coefs
      
      # Get coef
      coef[[i]] <-
        output_ref[line1 + 1] %>% 
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(GF_S = str_c("C#", 1:(k))) %>% 
        mutate(ORs = case_when(!is.na(estimate) ~ exp(as.numeric(estimate)))) %>%  #compute OR and 95%CI
        mutate(LL_CI = exp(as.numeric(estimate) - 1.96 * as.numeric(se))) %>% 
        mutate(UL_CI = exp(as.numeric(estimate) + 1.96 * as.numeric(se))) %>% 
        mutate(across(c(ORs, LL_CI, UL_CI), round, 2))
    }
  }
  
  # Merge each data frame into one table
  table <- list(coef, warn_err) %>%
    map(reduce, merge, all = TRUE) %>%
    reduce(merge, all = TRUE) %>%
    select('cov', everything()) %>%
    arrange(cov)
  
  return(table)
}