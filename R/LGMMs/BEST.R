# models = list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best)
# table = list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best) %>% getFitIndices()

BEST <- function(models, table) {
  best_name <- table %>%
    dplyr::filter(is.na(Errors)) %>%
    dplyr::filter(is.na(Warnings)) %>%
    dplyr::mutate(ncrit = dplyr::case_when(dplyr::if_any(tidyselect::starts_with("proportion"), ~ .x <= 5) ~ 0, .default = 1)) %>% 
    dplyr::filter(ncrit != 0) %>% 
    dplyr::filter(T11_LMR_PValue <= 0.05) %>%
    dplyr::filter(BIC == min(BIC)) %>%
    dplyr::pull(Title) %>%
    dplyr::first() %>% # if 2 models with = BIC
    trimws()

  best_model <- purrr::keep(models, ~ stringr::str_like(purrr::pluck(.x, "TITLE"), best_name))

  # best_title <- pluck(models, type.convert(best_name, as.is = TRUE), "TITLE")
  # best_model <- pluck(models, type.convert(best_name, as.is = TRUE))
  # best_model <- models[map_lgl(models, ~ str_like(pluck(.x, 'TITLE'), best_name))]

  print(best_name)
  # print(best_title)

  return(best_model[[1]])
}
