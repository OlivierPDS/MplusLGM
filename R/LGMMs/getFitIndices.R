#' @title getFitIndices
#' @description Returns a selection of fit indices for a list of MplusObjects
#' @param list_models A list containing MplusObjects
#' @return A data frame
#' @export
#' @import MplusAutomation
getFitIndices <- function(list_models) {
  
  # Iterate through all models in list to extract results from path
  model_res_list <- list()
  count <- 1
  
  n_sample <- NULL  # the sample size for all models must be the same for CAIC
  for (model in list_models) {
    
    # Get the filepath of this model's output
    path_datafile <- model[["results"]][["input"]][["data"]][["file"]]
    path_dir <- strsplit(path_datafile, '.dat')[[1]]
    path_out <- paste0(path_dir, '.out')
    
    # Read this model and append it to list
    model_res <- MplusAutomation::readModels(path_out, what = 'summaries')
    model_res_list[[count]] <- model_res
    count <- count + 1
    
    # check sample size for all models must be the same for CAIC
    if (is.null(n_sample)) {
      n_sample <- model_res[["summaries"]][["Observations"]]
    } else {
      stopifnot(n_sample == model_res[["summaries"]][["Observations"]])
    }
  }
  
  # Create table of these models and return
  models_sum <- MplusAutomation::SummaryTable(
    model_res_list, 
    keepCols = c("Title", "Parameters", "LL", "AIC", "AICC", "BIC", "Entropy", 
                 "T11_LMR_Value", "T11_LMR_PValue"))
  
  models_sum <- models_sum %>% 
    mutate(CAIC = -2 * LL + Parameters * (log(n_sample) + 1)) %>% 
    select("Title", "Parameters", "LL", "BIC", "AIC", "AICC", "CAIC", "Entropy", 
           "T11_LMR_Value", "T11_LMR_PValue")
  
  return(models_sum)
  
}