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
  
  
  # Get necessary model information
  k <-  model %>%  purrr::pluck("results", "summaries", "NLatentClasses")
  
  # Initialize list of all current growth factors for classes
  gf <-
    if (p == 3) {
      map(1:k, ~ c("I", "S", "Q", "CUB"))
    } else if (p == 2) {
      map(1:k, ~ c("I", "S", "Q"))
    } else if (p == 1) {
      return(model) # Cannot further refine if already linear
    } else {
      stop('invalid polynomial order of model')
    }
  
  # Create a list of all current factors to be checked for all classes
  gf_current <- gf %>%  map(tail, 1)
  
  param_df <-
    model[["results"]][["parameters"]][["unstandardized"]] %>%
    filter(str_detect(paramHeader, 'Means')) %>%
    filter(str_detect(param, "C#[:digit:]+", negate = TRUE)) %>%
    select(LatentClass, param, pval)
  
  # Get the p values
  pval_lst <- list()
  for (n in 1:k) {
    pval_lst[[n]] <- param_df %>%
      filter(str_detect(param, gf_current[[n]]) &
               LatentClass == n) %>%
      pluck("pval")
  }
  
  gf0 <- list()
  for (n in 1:k) {
    gf0[[n]] <- param_df %>%
      filter(str_detect(param, gf_current[[n]]) &
               LatentClass == n) %>%
      mutate(gf0 = if_else(pval > 0.05,
                           str_c(gf_current[[n]], "@0"),
                           str_c(gf_current[[n]]))) %>%
      pluck("gf0")
  }
  
  # While any P value for a class growth factor is non-significant
  while (any(pval_lst > .05)) {
    # Get current growth factor index and replace it with appropriate growth factor
    for (n in 1:k) {
      j <- grep(gf_current[[n]], gf[[n]])
      gf[[n]] <- gf[[n]] %>% assign_in(j, gf0[[n]])
    }
    
    # Get the growth factors for each class as a number
    gf_n <- gf %>% map(~ str_ends(.x, "@0", TRUE)) %>%
      map(~ sum(.x) - 1) %>%
      str_c(collapse = "")
    
    
    # If any growth factor is zero, break as cannot have an intercept only model
    if (0 %in% gf_n) {
      break
    }
    
    # Update Mplus Object with appropriate growth factors
    mpobj <- update(
      model,
      TITLE = as.formula(glue(
        "~ 'FINAL_{str_c(gf_n, collapse = '')};'"
      )),
      OUTPUT = as.formula(glue(
        "~ 'TECH1 SAMPSTAT STANDARDIZED;'",
      )),
      SAVEDATA = as.formula(
        glue(
          "~ '
        FILE = FINAL_{str_c(gf_n, collapse = '')}.dat;
        SAVE = CPROBABILITIES;'"
        )
      ),
      autov = FALSE,
      rdata = df
    )
    
    for (n in 1:k) {
      mpobj[["MODEL"]] <- mpobj[["MODEL"]] %>%
        str_replace(
          glue("%c#{n}%([:space:]\\[.*\\];)?"),
          glue("%c#{n}%
                   [", "{str_c(gf[[n]], collapse=' ')}", "];")
        )
      
    }
    
    # Create directory for results if does not already exist
    model_dir <-
      glue::glue(getwd(), '{usevar}', 'Results', 'FINAL', .sep = "/")
    if (!dir.exists(model_dir)) {
      dir.create(model_dir, recursive = TRUE)
    }
    
    # Run model
    model <- mplusModeler(
      object = mpobj,
      dataout = glue(getwd(), "/{usevar}/Results/FINAL/{str_c(gf_n, collapse = '')}.dat"),
      modelout = glue(getwd(), "/{usevar}/Results/FINAL/{str_c(gf_n, collapse = '')}.inp"),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )
    
    # Update model's parameters
    param_df <-
      model[["results"]][["parameters"]][["unstandardized"]] %>%
      filter(str_detect(paramHeader, 'Means')) %>%
      filter(str_detect(param, "C#[:digit:]+", negate = TRUE)) %>%
      select(LatentClass, param, pval)
    
    # Update current growth factor
    j <- j - 1
    gf_current <- map(gf, pluck, j)
    
    # Update the p values
    for (n in 1:k) {
      gf0[[n]] <- param_df %>%
        filter(str_detect(param, gf_current[[n]]) &
                 LatentClass == n) %>%
        mutate(gf0 = if_else(pval > 0.05,
                             str_c(gf_current[[n]], "@0"),
                             str_c(gf_current[[n]]))) %>%
        pluck("gf0")
    }
    
    for (n in 1:k) {
      pval_lst[[n]] <- param_df %>%
        filter(str_detect(param, gf_current[[n]]) &
                 LatentClass == n) %>%
        pluck("pval")
    }
    
  }
  return(model)
}
