#' @title getFitIndices
#' @description Returns a selection of fit indices for a list of MplusObjects
#' @param list_models A list containing MplusObjects
#' @return A data frame
#' @export
#' @import MplusAutomation
getFitIndices <- function(list_models) {
  
  list_depth <- list_models %>% purrr::vec_depth()

  # While loop until 1-level depth list of Mplus Object  
  while (list_depth != 7) {
    if (list_depth > 7) {
      list_models <- list_models %>% purrr::flatten()
    } else {
      list_models <- list(list_models)
    }
    list_depth <- list_models %>% purrr::vec_depth()
  }

  # Get model parameters
  n <- purrr::map(list_models, purrr::pluck, "results", "summaries", "Observations") %>%
       plyr::ldply(rbind) %>%
       dplyr::select("1") %>% 
       data.table::setnames("n")
  
  k <- purrr::map(list_models, purrr::pluck, "results", "summaries", "NLatentClasses") %>%
       plyr::ldply(rbind) %>%
       dplyr::select("1") %>% 
       max()
  
  # Get model errors & warnings
  models_err <- list_models %>% purrr::map(purrr::pluck, 'results', 'errors') %>%
    purrr::map_depth(2, purrr::keep, stringr::str_detect, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY") %>% 
    purrr::map_depth(1, purrr::flatten_chr) %>% 
    purrr::modify_if(~ length(.) == 0, ~ NA_character_) %>% 
    plyr::ldply(rbind) %>% 
    dplyr::select("1") %>% 
    data.table::setnames("errors")
  
  models_warn <- list_models %>% purrr::map(purrr::pluck, 'results', 'warnings') %>%
    purrr::map_depth(2, purrr::keep, stringr::str_detect, "WARNING:") %>% 
    purrr::map_depth(1, purrr::flatten_chr) %>% 
    purrr::modify_if(~ length(.) == 0, ~ NA_character_) %>% 
    plyr::ldply(rbind) %>%
    dplyr::select("1") %>% 
    data.table::setnames("warnings")
  
  # Get Average posterior probabilities
  APPA <- list_models %>% 
    purrr::map(purrr::pluck, 'results', 'class_counts', 'avgProbs.mostLikely') %>% #.default = NULL/NA
    purrr::modify_if(~!is.null(.x), ~ diag(.x)) %>% 
    purrr::modify_if(~is.null(.x), ~NA) %>% 
    plyr::ldply(rbind) %>% 
    dplyr::select(stringr::str_c(seq(k))) %>% 
    data.table::setnames(stringr::str_c('APPA', seq(k)))
  
  # Get class counts & proportions
  models_cc <- list_models %>% 
    purrr::map(purrr::pluck, 'results', 'class_counts', 'mostLikely', .default = NA_character_) %>% 
    #purrr::modify_if(~is.null(.x), ~NA) %>% 
    purrr::modify_if(~!anyNA(.x), ~ tidyr::pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
    tryCatch(expr = reduce(., rbind), error=function(e) reduce(., rbind.fill)) # because rbind returns error when df have different ncol 
  
    
  # Create table of model summaries and bind tables together
  models_sum <- MplusAutomation::SummaryTable(
    list_models,
    keepCols = c(
      "Title",
      "Parameters",
      "LL",
      "AIC",
      "AICC",
      "BIC",
      "Entropy",
      "T11_LMR_Value",
      "T11_LMR_PValue"
    )
  ) %>%
    dplyr::mutate(CAIC = -2 * LL + Parameters * (log(n) + 1)) %>%
    cbind(models_warn, models_err, n, APPA, models_cc) %>% 
    dplyr::select("Title", "n", "Parameters", "LL", "AIC", "AICC", "CAIC", "BIC", starts_with("APPA"), everything())
  
  return(models_sum)
  
}
