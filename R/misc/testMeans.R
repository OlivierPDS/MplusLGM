#' @title testMeans
#' @description Run one-way ANOVA or chi-squared tests
#' @param list_vars List of variables to test
#' @param group_var Variable to group by
#' @param df A data frame
#' @param correct Apply Holm correction
#' @return A data frame
#' @importFrom car leveneTest
#' @export
testMeans <- function(list_vars, group_var, df, correct = TRUE) {
  
  # Print initial message
  cat('\n\n-----------------------------------------------------------------\n')
  cat('RESULTS FOR MEANS TESTS')
  cat('\n-----------------------------------------------------------------\n\n')
  
  list_res <- list()
  list_pvals <- list()
  
  # Iterate through all test variables
  for (var in list_vars) {
    
    # If it is a factor, run a chi-square test
    if (is.factor(df[[var]])) {
      
      # Print message
      cat(toupper(paste0('Chi-Square Test: ', var, ' x ', group_var, '\n')))
      
      # Run Test
      list_res[[var]] <- with(df, chisq.test(get(group_var), get(var)))
      list_res[[var]][['data.name']] <- paste(group_var, var, sep=' and ')
      
      list_pvals[[var]] <- list_res[[var]][["p.value"]]
      
      # Print results
      print(list_res[[var]])
      
    }
    
    # If it is not a factor, run ANOVA
    else {
      
      # Print message
      cat(toupper(paste0('One-Way ANOVA: ', var, ' x ', group_var, '\n\n')))
      
      # Run Test
      fml <- as.formula(paste(var, group_var, sep=' ~ '))
      list_res[[var]] <- aov(fml, data = df)
      
      # Print results
      print(summary(list_res[[var]]))
      
      list_pvals[[var]] <- summary(list_res[[var]])[[1]][["Pr(>F)"]][[1]]
      
      cat('\n')
      print(car::leveneTest(list_res[[var]]))
      
    }
    
    # Print line to divide
    cat('\n-----------------------------------------------------------------\n')
    
    
  }
  
  list_pvals <- lapply(list_pvals, round, 3)
  corrected_pvals <- p.adjust(list_pvals, method = "holm", n = length(list_pvals))
  uncorrected_pvals <- p.adjust(list_pvals, method = "none", n = length(list_pvals))
  
  cat('\nUncorrected P-Values:\n')
  print(uncorrected_pvals) # Used adjustment function for format
  
  cat('\nCorrected P-Values:\n')
  print(corrected_pvals)
  
  return(list_res)
  
}