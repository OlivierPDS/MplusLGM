fitR3STEP <- function(list_mpobj,
                      cov,
                      manual_R3STEP = FALSE) {
  
  # Create empty lists to store for loop outputs
  coef <- list()
  intercepts <- list()
  ORs <- list()
  warn_err <- list()
  
  for (i in cov) {
    output <- pluck(list_mpobj, i, 'results', 'output')
    line <- grep("C#\\d\\s+ON", output) #get lines of regression coefs and ORs
    
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
      
      # Get coef, ORs and intercepts values 
      coef[[i]] <-
        if (manual_R3STEP == FALSE) {
          output[line[1:(length(line) / 2)] + 1] #Auxiliary R3STEP
        } else {
          output[line[c(TRUE, FALSE)] + 1] #Manual R3STEP
        } %>%
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_c("C#", 1:(length(line) / 2)))
      
      intercepts[[i]] <-
        if (manual_R3STEP == FALSE) {
          output[line[1:(length(line) / 2)] + 4] #Auxiliary R3STEP
        } else {
          output[line[c(TRUE, FALSE)] + 4] #Manual R3STEP
        } %>%
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        select(-1) %>%
        setNames(c('class', 'estimate', 'se', 'tval', 'pval')) %>%
        mutate(class = str_c("intercept", 1:(length(line) / 2))) %>%
        mutate('cov' = str_to_upper(i))
      
      ORs[[i]] <-
        if (manual_R3STEP == FALSE) {
          output[line[(length(line) / 2 + 1):length(line)] + 1] #Auxiliary R3STEP
        } else {
          output[line[c(FALSE, TRUE)] + 1] #Manual R3STEP
        } %>%
        str_split('[:space:]+', simplify = TRUE) %>%
        as.data.frame() %>%
        setNames(c('class', 'cov', 'OR', 'OR_se', '[OR_CI', 'OR_CI]')) %>%
        mutate(class = str_c("C#", 1:(length(line) / 2)))
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
