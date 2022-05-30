#' @title refinePolynomial
#' @description Given an MplusModel, refines the growth factors of each class in
#'     the model.
#' @param model An MplusObject
#' @param df A data frame containing all user variables and the ID variable
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector
#' @param idvar A character vector containing the ID variable in the data frame
#' @param working_dir The directory where the results folder will be created
#' @return An MplusObject
#' @export
refinePolynomial <- function(
  model, 
  df, 
  usevar, 
  timepoints,
  idvar, 
  working_dir = getwd()
  ) {
  
  # Get necessary model information
  k <- readr::parse_number(model[["results"]][["input"]][["variable"]][["classes"]])
  p <- readr::parse_number(stringr::str_split(model[["TITLE"]], '_')[[1]][[2]])
  m <- stringr::str_split(model[['TITLE']], '_')[[1]][[1]]
  
  # Initialize list of all current growth factors for classes
  factorsClass <- list()
  
  for (i in 1:k) {
    if (p == 3) {
      factorsClass[[i]] <- c("I","S","Q","CUB")
    } else if (p == 2) {
      factorsClass[[i]] <- c("I","S","Q")
    } else if (p == 1) {
      return(model) # Cannot further refine if already linear
    } else {
      stop('invalid polynomial order of model')
    }
  }
  
  # Create a list of all current factors to be checked for all classes
  currentFactorClass <- .create.list.currentFactor(factorsClass, k)
  
  # Create a list that contains the indexes of the mean p values of the current factors for all classes
  indexPvalueClass <- .create.list.indexPvalue(currentFactorClass, model, k)
  
  # Get the p values using the indexes
  pValClass <- .create.list.pValues(model, indexPvalueClass, k)
  
  # While any P value for a class growth factor is non-significant
  while (any(pValClass > .05)) {
    
    # Check each class
    for (i in 1:k) {
      # If the growth factor is not significant, and not the slope, remove it
      if (pValClass[[i]] > 0.05 && head(factorsClass[[i]]) != "S") {
        factorsClass[[i]] = head(factorsClass[[i]], -1) # Remove from counter
      }
    }
    
    # Get the growth factors for each class as a number
    gf <- c()
    for (i in 1:k) {
      gf <- c(gf, length(factorsClass[[i]]) - 1)  # Subtract for I
    }
    
    # If any growth factor is zero, break as cannot have an intercept only model
    if (0 %in% gf) {
      break
    }
    
    # Run model with updated growth factors
    model <- runModel(df, usevar, timepoints, idvar, k, p, m, working_dir, 4000, gf)
    
    # Update current highest growth factor
    currentFactorClass <- .create.list.currentFactor(factorsClass, k)
    
    # Update indices of class p-values
    indexPvalueClass <- .create.list.indexPvalue(currentFactorClass, model, k)
    
    # Get the new class p values
    pValClass <- .create.list.pValues(model, indexPvalueClass, k)
    
  }
  
  return(model)
  
}
  
  
#' Create a list of all current factors (= the last ones) to be checked for all classes
.create.list.currentFactor <- function(factorsClass, k) {
  currentFactorClass <- list()
  for (i in 1:k){
    currentFactorClass[[i]] <- tail(factorsClass[[i]], n=1)
  }
  return (currentFactorClass)
}


#' index of the mean p value of the current factor that we check for each class
.create.list.indexPvalue <- function(currentFactorClass, model, k){
  
  # Read the list of parameters to find the position of the means and find the p values associated with the means
  listeParam <- model$results$parameters$unstandardized$param
  
  indexPvalueClass <- list()
  j <- 1 # the index of the p value of the 1st class would be the first occurrence of the current factor
  for (i in 1:k){
    indexPvalueClass[[i]] <- grep(glue::glue("^{currentFactorClass[[i]]}$"), listeParam)[j]
    # We skip the factors related to the variances 
    # j = 1: first occurrence of the factor (mean of class 1), j = 2: second occurrence of the factor (variance of class 1)
    # j = 3: third occurrence of the factor (mean of class 2)...
    j <- j + 2
  }
  return(indexPvalueClass)
}


#' Get the p values using the indexes
.create.list.pValues <- function(mplusObject,indexPvalueClass, k){
  pValClass <- list()
  for (i in 1:k){
    pValClass[[i]] <- mplusObject$results$parameters$unstandardized$pval[indexPvalueClass[[i]]]
  }
  return (pValClass)
}