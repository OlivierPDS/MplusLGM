#' @title fitGBTM 
#' @description Fits GBTM models from a specified minimum class to a maximum class
#'     using the runModel function
#' @param df A data frame containing all user variables and the ID variable
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector
#' @param idvar A character vector containing the ID variable in the data frame
#' @param working_dir The directory where the results folder will be created
#' @param min_k The minimum class structure of the model
#' @param max_k The maximum class structure of the model
#' @param overall_polynomial A numeric value representing the polynomial order for 
#'     the overall model (note that only linear, quadratic, and cubic models are 
#'     supported)
#' @return An MplusObject
#' @export
fitGBTM <- function(
  df, 
  usevar, 
  timepoints, 
  idvar, 
  working_dir = getwd(),
  min_k = 1, 
  max_k = 6,
  overall_polynomial = 3
  ) {
  
  # Input validation
  stopifnot(max_k <= 6, overall_polynomial <= length(timepoints))
  
  # Run GBTM for K = min - max
  list_gbtm_results <- list()
  for (k in min_k:max_k) {
    list_gbtm_results[[k]] <- runModel(
      df, usevar, timepoints, idvar, k, overall_polynomial, 'GBTM', working_dir, 4000)
  }
  
  return(list_gbtm_results)
  
}
