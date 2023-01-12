D3STEPfit <- function(list_mpobj,
                      cov){
  
  list_mpobj <- D3STEPm_models
  cov <- c('PSR_24', 'SANS_24')
  
  # Create empty lists to store for loop outputs
  coef <- list()
  WT <- list()
  ORs <- list()
  warn_err <- list()
  
  for (i in cov) {
   i <- 'PSR_24'
     
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
      
      if (is.factor(df[[i]])) {
        start <- output %>% str_which("RESULTS IN PROBABILITY SCALE")
        output_ref <- output[start:length(output)]
        line1 <-  output_ref %>% str_which('Category') #get lines of regression coefs  
        line2 <-  output %>% str_which('Wald Test') #get lines of regression coefs  
        n <- nlevels(df[[i]])
      }
      
      #add if (!is.factor(df[[i]]))
      
      # Get coef
      coef[[i]] <-
        output_ref[line1[1:(n*k)]] %>%
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        unite(cov, c(V2, V3), sep = "", remove=TRUE) %>% 
        mutate(cov = str_c({i}, " ", cov)) %>% 
        setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = rep(str_c("C#", 1:k), each=n))
      
      ORs[[i]] <-
        output_ref[tail(line1, n-1)] %>%
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        unite(cov, c(V2:V4), sep="", remove=TRUE) %>% 
        mutate(cov = str_c({i}, " ", cov)) %>% 
        setNames(c('class', 'cov', 'OR', 'OR_se', '[OR_CI', 'OR_CI]')) %>%
        mutate(class = str_c("C#", 1:(k-1)))
      
      WT[[i]] <- 
        output[(line2+2):(line2+4)] %>%
        str_split('([:space:]+)(?![:alpha:])', simplify = TRUE) %>% 
        as.data.frame() %>% 
        pivot_wider(names_from = V2, values_from = V3) %>% 
        setNames(c('cov', 'WTval', 'df', 'pval')) %>% 
        mutate(cov = {i})
    }
  }
  
  # Merge each data frame into one table
  table <- list(coef, ORs, WT, warn_err) %>%
    map(reduce, merge, all = TRUE) %>%
    reduce(merge, all = TRUE) %>%
    select('cov', everything()) %>%
    arrange(class)
  
  return(table)
}