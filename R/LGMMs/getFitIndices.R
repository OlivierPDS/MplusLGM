#' @title getFitIndices
#' @description Returns a selection of fit indices for a list of MplusObjects
#' @param list_models A list containing MplusObjects
#' @return A data frame
#' @export
#' @import MplusAutomation


# Test arguments/function -------------------------------------------------
# list_models <- list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best)
# list_models <- list(FINAL_dist)




getFitIndices <- function(list_models) {

  title <- list_models %>% 
    map(~ pluck(.x, "TITLE"))
  
  # Get model errors & warnings
  warnings <- list_models %>%
    purrr::map(pluck, "results", "warnings", .default = NULL) %>% 
    purrr::map_depth(2, ~ paste(.x, collapse = " ")) %>% 
    purrr::map_depth(2, ~ purrr::keep(.x, stringr::str_detect(.x,  "WARNING:"))) %>% 
    purrr::map(~ compact(.x))
  
  errors <- list_models %>%
    purrr::map(pluck, "results", "errors", .default = NULL)
  
  warn_err <-list(title, warnings, errors) %>% 
    purrr::pmap(\(title, warnings, errors) dplyr::tibble( Title = trimws(title), Warnings = paste(warnings, collapse = " "), Errors = paste(errors, collapse = " "))) %>%
    purrr::map(~ dplyr::mutate(.x, dplyr::across(c('Warnings', 'Errors'), ~ stringr::str_remove_all(.x, "[[:punct:]]")))) %>%
    purrr::map_dfr(~ dplyr::mutate(.x, dplyr::across(c('Warnings', 'Errors'), ~ dplyr::na_if(.x, "")))) 
    # purrr::imap_dfr(\(x, idx) dplyr::mutate(x, Title = stringr::str_to_upper(idx)))
  
  sublist_models <- list_models %>%
    purrr::discard(purrr::map_vec(errors, ~ length(.x) > 0))
  
  # Get Average posterior probabilities
  APPA <- sublist_models %>% 
    purrr::map(purrr::pluck, 'results', 'class_counts', 'avgProbs.mostLikely', .default = NULL) %>%
    purrr::map_if(~ !is.null(.x), ~ diag(.x), .else = ~ NA_character_) %>%
    purrr::map(\(x) as.data.frame(matrix(x, nrow = 1, byrow = FALSE))) %>% 
    purrr::map(\(x) rename_with(x, ~ paste0('APPA', seq_along(.x))))
  
  # Get class counts & proportions
  cc <- sublist_models %>% 
    purrr::map(purrr::pluck, 'results', 'class_counts', 'mostLikely', .default = NULL) %>% 
    purrr::map_if(~ !is.null(.x), ~ tidyr::pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion')))
  
  # Create table of model summaries and bind tables together
  summary <- sublist_models %>%  
    purrr::map(purrr::pluck, "results", "summaries")
  
  results <- list(summary, APPA, cc) %>% 
    purrr::pmap_dfr(\(x, y, z) purrr::reduce(list(x, y, z), ~ merge(.x, .y, all = TRUE))) %>% 
    dplyr::mutate(Title = trimws(Title))
    
  # Merge each data frame into one table ------------------------------------
  table <- full_join(results, warn_err, by = 'Title', multiple = "any") %>% 
    dplyr::mutate(dplyr::across(dplyr::starts_with("proportion"), ~ round(.x * 100, digits = 2))) %>% 
    dplyr::mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
    dplyr::select("Title", "Observations", "Parameters", "NLatentClasses", "LL", "AIC", "AICC", "CAIC", "BIC", starts_with(c("T11_LMR", "count", "proportion", "APPA")), "Entropy", "Warnings", "Errors")
  
  return(table)
  
}
