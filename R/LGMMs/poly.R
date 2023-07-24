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
poly <- function(model,
                             df,
                             usevar,
                             p = c(1, 2, 3)) {

  # model = GMMcv_best
  # df = SANS_df
  # usevar = 'SANS'
  # p = 3
  
  # Get necessary model information
  k <-  model %>%  purrr::pluck("results", "summaries", "NLatentClasses")
  
  # Initialize list of all current growth factors for classes
  gf <- switch(p,
  "1" = c("I", "S"),
  "2" = c("I", "S", "Q"),
  "3" = c("I", "S", "Q", "CUB")
) %>% {map(seq(k), \(x) .)}
    
  # Create a list of all current factors to be checked for all classes
  gf_current <- gf %>%  
    map(tail, 1)
  
  param_df <- model %>% 
    pluck("results", "parameters", "unstandardized") %>% 
    filter(str_detect(paramHeader, 'Means') & param %in% c("I", "S", "Q", "CUB")) %>%
    select(LatentClass, param, pval)
  
  # Get the p values
  pval_lst <- map2(seq(k), gf_current, \(k, gf_current) filter(param_df, param == gf_current & LatentClass == k)) %>%
  map(~ pluck(.x, "pval"))
  
  gf0 <- map2(seq(k), gf_current, \(k, gf_current) filter(param_df, param == gf_current & LatentClass == k)) %>%
    map(~ mutate(.x, gf0 = if_else(pval > 0.05,
                                   paste0(param, '@0'), 
                                   param))) %>% 
    map(~ pluck(.x, "gf0"))

  # While any P value for a class growth factor is non-significant
  while (any(pval_lst > .05)) {
    
    # Get current growth factor index and replace it with appropriate growth factor
    gf <- list(gf, gf0, gf_current) %>% 
    pmap(\(gf, gf0, gf_current) map_chr(gf, \(gf) str_replace(gf, glue("{gf_current}$"), gf0)))
    
    # Get the growth factors for each class as a number
    gf_n <- gf %>% map(~ str_ends(.x, "@0", negate = TRUE)) %>%
      map(~ sum(.x) - 1) %>%
      str_c(collapse = "")
    
    # If any growth factor is zero, break as cannot have an intercept only model
    if (0 %in% str_split(gf_n, '')) {
      break
    }
    
    gf_mean <- map_chr(gf, ~ glue("[{paste(str_to_lower(.x), collapse = ' ')}];"))
    
    # Update Mplus Object with appropriate growth factors
    mpobj <- model %>% 
      modify_in('TITLE', ~ glue(.x, "_{gf_n}"))
    
    for (n in 1:k) {
      mpobj[["MODEL"]] <- mpobj[["MODEL"]] %>%
        str_replace(
          glue("%C#{n}%([:space:]\\[.*\\];)?"), #error here "%C#{n}%([:space:]\\[.*\\])?") 
          glue("%C#{n}%
               {gf_mean[[n]]}")
        )
      
    }
    
    # Create directory for results if does not already exist
    model_dir <- glue::glue(getwd(), '{usevar}', 'Results', 'FINAL', .sep = "/")
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
    }
    
    # Run model
    model <- mplusModeler(
      object = mpobj,
      dataout = glue(getwd(), "/{usevar}/Results/FINAL/{gf_n}.dat"),
      modelout = glue(getwd(), "/{usevar}/Results/FINAL/{gf_n}.inp"),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )
    
    # Update model's parameters
    param_df <- model %>% 
      pluck("results", "parameters", "unstandardized") %>% 
      filter(str_detect(paramHeader, 'Means') & param %in% c("I", "S", "Q", "CUB")) %>%
      select(LatentClass, param, pval)
    
    # Update current growth factor
    gf_current <- map2(gf, gf_current, \(gf, gf_current)
        head_while(gf, ~ str_starts(.x, gf_current, negate = TRUE))) %>% 
        map(tail, 1)
  
    # Update the p values
    pval_lst <- map2(seq(k), gf_current, \(k, gf_current) filter(param_df, param == gf_current & LatentClass == k)) %>%
      map(~ pluck(.x, "pval"))
    
    gf0 <- map2(seq(k), gf_current, \(k, gf_current) filter(param_df, param == gf_current & LatentClass == k)) %>%
      map(~ mutate(.x, gf0 = if_else(pval > 0.05,
                                     paste0(param, '@0'), 
                                     param))) %>% 
      map(~ pluck(.x, "gf0"))
    
  }
  return(model)
}
