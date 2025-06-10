#' @title Perform the manual 3-steps method for testing the association of covariates in latent growth modeling.

#' @description Perform the manual 3-steps method for testing the association of covariates in latent growth modeling.

#' @param data A data frame containing all covariates to test.
#' @param outvar A character vector specifying the outcome variables at different times.
#' @param idvar A character string specifying the ID variable.
#' @param starting_val A numeric value specifying the number of random starting values to generate for the initial optimization stage.
#' Note that the number of final stage optimizations will be set as equal to half of this value.
#' @param output A character vector specifying the requested Mplus output options for the model.
#' @param plot A character string specifying the requested Mplus plot options for the model.
#' @param lgm_object An `mplusObject` with predefined random starting values (`STARTS`) in the ANALYSIS section.
#' @param wd A character string specifying the directory where the results folder will be created for saving the Mplus input, output, and data files.
#' Default is the current working directory.

#' @return A list of class `mplusObject` with elements specifying sections of an Mplus input file for conducting latent growth modeling.

#' @details The `LGMobject` function facilitates and automates the appropriate model specification for conducting latent growth modeling in Mplus.
#' It creates the relevant sections of an Mplus input file, including: TITLE, VARIABLE, ANALYSIS, MODEL, OUTPUT, PLOT, and SAVEDATA.
#'
#' This function builds upon the capabilities of the \code{\link[MplusAutomation]{mplusObject}}function
#' from the MplusAutomation package.

#' @seealso
#' \code{\link[MplusAutomation]{mplusObject}} for creating an mplusObject.
#' \code{\link{runLGM}} for conducting latent growth modelling with an mplusObject.

#' @importFrom

#' @export

#' @examples
#' # Example usage:
R3STEP_sx <- R3STEP(
  lgm_object = GAF3_CTRL_poly,
  data = TRAJ5_CTRL_df,
  outvar = paste("GAF", seq(from = 24, to = 60, by = 12), sep = "_"),
  idvar = "id",
  covar = covar_R3STEP,
  start_val = 0,
  output = c("SAMPSTAT", "STANDARDIZED", "CINTERVAL", "TECH7", "TECH12"),
  wd = "Results"
  )

R3STEP <- function(data,
                   outvar,
                   idvar,
                   covar,
                   start_val = 0,
                   lgm_object,
                   output = c("SAMPSTAT", "STANDARDIZED", "CINTERVAL", "TECH7", "TECH12"),
                   wd = "Results") {

  # Validate Arguemtns ---------------------------------------------------------


  ## Classes -------------------------------------------------------------------
  k <- lgm_object %>%
    purrr::pluck("results", "summaries", "NLatentClasses")

  ## Update mplusObject --------------------------------------------------------
  ### Update title (covar) -----------------------------------------------------
  title <- lgm_object %>%
    purrr::pluck("TITLE") %>%
    stringr::str_replace("S\\d+", covar)

  lgm_object <- lgm_object %>%
    purrr::modify_in("TITLE",
                     \(title) stringr::str_replace(title, "S\\d+", covar))

  # Create R3STEP Mplus object -------------------------------------------------
  R3STEP_object <- MplusAutomation::mplusObject(
    TITLE = title,
    VARIABLE = .getVariable(covar, k),
    MODEL = model,
    ANALYSIS = .getAnalysis(),
    OUTPUT = .getOutput(output),
    rdata = .getRdata(lgm_object, data, covar, idvar),
    usevariables = colnames(rdata),
    autov = FALSE
  )

  # Create directory for Mplus data, inputs and outputs ---------------------
  path_dir <- .createModelDir(lgm_object, wd)

  # Run Mplus models --------------------------------------------------------
  model_lst <- purrr::map(R3STEP_object, \(object)
  MplusAutomation::mplusModeler(
    object = object,
    dataout = path_dir$data,
    modelout = path_dir$input,
    run = 1,
    check = FALSE,
    writeData = "always",
    hashfilename = FALSE,
    quiet = TRUE
  ))

  names(model_lst) <- covar

  return(model_lst)
}

# Helper functions -------------------------------------------------------------
## .getVariable --------------------------------------------------------------
.getVariable <- function(covar, k) {

  usevar <- glue::glue("USEVAR = N {covar}")

  nominal <- "NOMINAL = N"

  classes <- glue::glue("CLASSES = c({k})")

  variable <- c(usevar, nominal, classes)

  return(.format(variable))

}

## .getAnalysis --------------------------------------------------------------
.getAnalysis <- function() {
  type <- "TYPE = MIXTURE"
  algorithm <- "ALGORITHM = INTEGRATION"
  integration <- "INTEGRATION = MONTECARLO"
  start_val <- "STARTS = 0"
  processors <- glue::glue("PROCESSORS = {detectCores()}")
  analysis <-  c(type, algorithm, integration, start_val, processors)

  return(.format(analysis))
}


## .getModel ------------------------------------------------------------------
.getModel <- function(lgm_object, k, covar) {

  ### Logits
  logits_df <- lgm_object %>%
    purrr::pluck("results", "class_counts", "logitProbs.mostLikely")

  class_spec <- purrr::map(1:k, ~ glue::glue("%C#{.x}%")) # class sections

  logits <- purrr::map(1:k, \(x) purrr::map2(x, 1:k, \(x, y) logits_df[x, y])) %>%
    unlist() %>%
    purrr::map2(rep(1:k, k), ., \(x, y) glue::glue("[N#{x}@{y}]")) %>%
    split(rep(1:k, each = k)) %>%
    purrr::map(\(x) utils::head(x, -1)) # logits specification

  reg <- purrr::map(covar, \(x) glue::glue("C ON {x}")) # regressions specification

  ### Model specification
  model1 <- list("%OVERALL%", reg, covar) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y)))

  model2 <- list(class_spec, logits) %>%
    purrr::reduce(~ purrr::map2(.x, .y, ~ c(.x, .y)))

  model3 <- map(model1, \(x) append(x, model2)) %>%
    purrr::map(~ MplusAutomation::parseMplus(unlist(.x), add = TRUE)) %>%
    purrr::map(~ gsub("%;", "%", .x)) # remove semicolon after model sections
}

## .getRdata ---------------------------------------------------------------------
.getRdata <- function(lgm_object, data, covar, idvar) {

  savedata <- lgm_object %>%
    pluck("results", "savedata")

  rdata <- left_join(
    x = savedata,
    y = dplyr::select(data, all_of(c(idvar, covar))), #validate arguments covar !%in% outvar
    by = join_by(stringr::str_to_upper(idvar) = idvar)
  ) %>%
    dplyr::rename(N = "C")

  return(rdata)
}

