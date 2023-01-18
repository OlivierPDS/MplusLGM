D3STEPfit <- function(list_mpobj,
                      cov){

  # Create empty lists to store for loop outputs
  coef <- list()
  mean <- list ()
  var <- list()
  WT <- list()
  ORs <- list()
  warn_err <- list()
  diff <- list()
  
  for (i in cov) {
     
    k <- purrr::pluck(list_mpobj, i, "results", "summaries", "NLatentClasses")
    df <- purrr::pluck(list_mpobj, i, "rdata")
    output <- purrr::pluck(list_mpobj, i, 'results', 'output')
    
    # Get Mplus warnings and errors 
    warning <- purrr::pluck(list_mpobj, i, 'results', 'warnings')
    error <- purrr::pluck(list_mpobj, i, 'results', 'errors')
    
    warn_err[[i]] <-
      data.frame(
        errors = ifelse(is.null(error), NA, paste(error)),
        warnings = ifelse(is.null(warning), NA, paste(warning))
      ) %>% 
      mutate(cov = str_to_upper(i))
    
    # return NULL if Mplus warning or errors
    if (is.null(error) == FALSE) {
      
      coef[[i]] <- NULL
      WT[[i]] <- NULL
      ORs[[i]] <- NULL
      mean[[i]] <- NULL
      var[[i]] <- NULL
      
    } else {
      
      if (is.factor(df[[i]])) {
        start <- output %>% str_which("RESULTS IN PROBABILITY SCALE")
        output_ref <- output[start:length(output)]
        line1 <-  output_ref %>% str_which('Category') #get lines of regression coefs  
        line2 <-  output %>% str_which('Wald Test') #get lines of regression coefs  
        n <- nlevels(df[[i]])
        
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
      }
      
      if (!is.factor(df[[i]])) {
        start <- output %>% str_which("MODEL RESULTS")
        output_ref <- output[start:length(output)]
        line1 <-  output_ref %>% str_which(glue('{i}')) #get lines of regression coefs  
        line2 <-  output %>% str_which('Wald Test') 
        line3 <-  output %>% str_which('New/Additional Parameters') 
        
        # Get coef
        mean[[i]] <-
          output_ref[line1[c(TRUE, FALSE)]] %>%
          str_split('[:space:]+', simplify = TRUE) %>%
          as.data.frame() %>%
          setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
          mutate(cov = str_c(cov, ' ', 'mean')) %>% 
          mutate(class = str_c("C#", 1:k))
        
        var[[i]] <-
          output_ref[line1[c(FALSE, TRUE)]] %>%
          str_split('[:space:]+', simplify = TRUE) %>%
          as.data.frame() %>%
          setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
          mutate(cov = str_c(cov, ' ', 'variance')) %>% 
          mutate(class = str_c('C#', 1:k))
        
        diff[[i]] <-
          output[(line3+1):(line3+k-1)] %>%
          str_split('[:space:]+', simplify = TRUE) %>%
          as.data.frame() %>%
          unite(cov, c(V1, V2), sep=" ", remove=TRUE) %>% 
          setNames(c('cov', 'estimate', 'se', 'tval', 'pval')) %>% 
          mutate(cov = str_c(i, cov))
      }
      
      WT[[i]] <- 
        output[(line2+2):(line2+4)] %>%
        str_split('([:space:]+)(?![:alpha:])', simplify = TRUE) %>% 
        as.data.frame() %>% 
        pivot_wider(names_from = V2, values_from = V3) %>% 
        setNames(c('cov', 'WTval', 'df', 'pval')) %>% 
        mutate(cov = str_to_upper(i))
    }
  }
  
  # Merge each data frame into one table
  table <- list(coef, WT, ORs,warn_err, mean, var, diff) %>%
    map(reduce, merge, all = TRUE) %>%
    reduce(merge, all = TRUE) %>%
    select(cov, class, estimate, se, tval, pval, everything()) %>%
    arrange(cov, class)
  
  return(table)
}