#' @title fitLCGA
#' @param df A data frame containing all user variables and the ID variable
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector
#' @param idvar A character vector containing the ID variable in the data frame
#' @param classes A numeric value representing the number of classes in the model
#' @param working_dir The directory where the results folder will be created
#' @param overall_polynomial A numeric value representing the polynomial order for 
#'     the overall model (note that only linear, quadratic, and cubic models are 
#'     supported)
#' @param ref_model An optional reference model to add to the list returned
#' @return An MplusObject
#' @export
fitLCGA <- function(
  df, 
  usevar, 
  timepoints, 
  idvar, 
  classes, 
  working_dir = getwd(),
  overall_polynomial = 3, 
  ref_model = NULL
  ) {
  
  # Initiate list and count
  list_rv_results <- list()
  cnt <- 1
  
  # If a reference model is specified, add it to the list
  if (!is.null(ref_model)) {
    list_rv_results[[cnt]] <- ref_model
    cnt <- cnt + 1
  }
  
  # Fit LCGA models
  for (m in c('LCGA1', 'LCGA2', 'LCGA3')) {
    list_rv_results[[cnt]] <- runModel(
      df, 
      usevar, 
      timepoints, 
      idvar, 
      classes, 
      overall_polynomial, 
      m, 
      working_dir, 
      4000)
    
    cnt <- cnt + 1
    
  }
  
  return(list_rv_results)
  
}
