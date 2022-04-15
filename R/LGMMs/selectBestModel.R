#' @title selectBestModel
#' @description Selects the best model from a list of MplusObjects based on a
#'     specified method. Current selection methods are "BIC" (select model with 
#'     the lowest BIC) and "BIC_LRT" (select model with the lowest BIC that also 
#'     has a significant LRT p-value).
#' @param list_models A list containing MplusObjects
#' @param selection_method A character vector representing the method to use for
#'     model selection
#' @return An MplusObject
#' @export
selectBestModel <- function(
  list_models, 
  selection_method = 'BIC_LRT'
  ) {
  
  # Input validation
  stopifnot(is.list(list_models), selection_method %in% c('BIC', 'BIC_LRT'))
  
  # Assume the best model is the first one
  best_model <- list_models[[1]]
  best_bic <- best_model[["results"]][["summaries"]][["BIC"]]
  
  # Attempt to replace the best model
  for (test_model in list_models) {
    
    # Get BIC from this model
    test_bic <- test_model[["results"]][["summaries"]][["BIC"]]
    
    # If it is less than the best model
    if (best_bic > test_bic) {
      
      # If the user selected to also need a significant LRT
      test_LRT <- test_model[["results"]][["summaries"]][["T11_LMR_PValue"]]
      
      # If not using LRT, LRT is null (k=1), or LRT is significant, replace
      if (selection_method != 'BIC_LRT' || is.null(test_LRT) || test_LRT < .05) {
        best_model <- test_model
        best_bic <- test_bic
        
      }
    }
  }
  
  return(best_model)
  
}