#' @title Create Mplus model objects for Latent Growth Modelling (LGM)

#' @description Provide flexibility for specifying Mplus LGM objects with various latent class and residual variance structures, and capturing individual differences in growth trajectories.
#' Support Growth Curve Models (GCM), Growth-Based Trajectory Models (GBTM) and Latent Class Growth Analysis (LCGA).
#' Once created, the model can be estimated using the `runLGM` function.

#' @param data A data frame containing all variables for the trajectory analysis.
#' @param outvar A character vector specifying the outcome variables at different times.
#' @param catvar A logical value indicating whether the outcome variable is categorical. Default is `FALSE`.
#' @param idvar A character string specifying the ID variable.
#' @param k An integer specifying the number of latent classes for the model.
#' @param starting_val A numeric value specifying the number of random starting values to generate for the initial optimization stage.
#' Note that the number of final stage optimizations will be set as equal to half of this value.
#' @param estimator A character string to specify the estimator to use in the analysis. Default is 'MLR'.
#' @param transformation A character string to specify the latent response variable transformation to use when the outcome variable is categorical. Default is `LOGIT`.
#' @param lgm_type A character string specifying the residual variance structure of the growth model. Options include:
#' \itemize{
#'    \item - "gcm" (relaxed residual variance across time),
#'    \item - "gbtm" (fixed residual variance across time and class),
#'    \item - "lcga_t" (relaxed residual variance across time),
#'    \item - "lcga_c" (relaxed residual variance across class),
#'    \item - "lcga_tc" (relaxed residual variance across both time and class).
#'    }
#' @param polynomial An integer specifying the order of the polynomial used to model trajectories. Supported values are:
#' 1 (linear), 2 (quadratic), 3 (cubic). Default is 1.
#' @param timescores A numeric vector specifying the time scores for the model. If `timescores_indiv = TRUE`,
#' a character vector should be used to specify variables with individually varying times of observation.
#' @param timescores_indiv A logical value indicating whether to use individually varying times of observation for the outcome variable. Default is `FALSE`.
#' @param output A character vector specifying the requested Mplus output options for the model.
#' @param plot A character string specifying the requested Mplus plot options for the model.
#' @param save A character string specifying the type of results to be saved by Mplus.

#' @return A list of class `mplusObject` with elements specifying sections of an Mplus input file for conducting latent growth modeling.

#' @details The `LGMobject` function facilitates and automates the appropriate model specification for conducting latent growth modeling in Mplus.
#' It creates the relevant sections of an Mplus input file, including: TITLE, VARIABLE, ANALYSIS, MODEL, OUTPUT, PLOT, and SAVEDATA.
#'
#' This function builds upon the capabilities of the \code{\link[MplusAutomation]{mplusObject}}function
#' from the MplusAutomation package.

#' @seealso
#' \code{\link[MplusAutomation]{mplusObject}} for creating an mplusObject.
#' \code{\link{runLGM}} for conducting latent growth modelling with an mplusObject.

#' @importFrom glue glue glue_collapse
#' @importFrom stringr str_remove_all str_split
#' @importFrom dplyr select first last
#' @importFrom purrr map imap_chr
#' @importFrom parallel detectCores
#' @importFrom MplusAutomation mplusObject

#' @export

#' @examples
#' # Example usage:
#' GBTM_object <- LGMobject(
#'   data = symptoms,
#'   outvar = paste("sx", seq(from = 0, to = 24, by = 6), sep = "_"),
#'   idvar = "id",
#'   catvar = FALSE,
#'   k = 3L,
#'   starting_val = 500,
#'   lgm_type = "gbtm",
#'   polynomial = 3,
#'   timescores = seq(from = 0, to = 24, by = 6),
#'   timescores_indiv = FALSE,
#'   output = c("TECH1", "TECH14", "SAMPSTAT", "STANDARDIZED"),
#'   plot = "PLOT3",
#'   save = "FSCORES"
#'   )

# LGMobject function ------------------------------------------------------
# Create Mplus Object for LGM with helper functions for each section of the Mplus input file.

LGMobject <- function(data,
                      outvar,
                      catvar = FALSE,
                      idvar,
                      k,
                      starting_val,
                      estimator = c("MLR", "ML", "WLSMV", "WLS"),
                      transformation = c("LOGIT", "PROBIT"),
                      lgm_type = c("gcm", "gbtm", "lcga_t", "lcga_c", "lcga_tc"),
                      polynomial = 1,
                      timescores,
                      timescores_indiv = FALSE,
                      output,
                      plot,
                      save) {

  ## Validate arguments --------------------------------------------------------
  ### Argument class
  stopifnot(
    is.data.frame(data),
    is.logical(c(catvar, timescores_indiv)),
    is.numeric(starting_val),
    is.integer(k),
    is.vector(outvar),
    is.character(c(outvar, idvar, output, plot, save))
  )

  ### Argument value
  stopifnot(
    length(idvar) == 1,
    length(outvar) == length(timescores),
    c(idvar, outvar) %in% names(data),
    k > 0,
    starting_val >= 0,
    polynomial %in% 1:3,
    polynomial < length(timescores)
  )

  ## Create Mplus model object -------------------------------------------------
  lgm_object <- MplusAutomation::mplusObject(
    TITLE = .getTitle(lgm_type, polynomial, k, starting_val),
    VARIABLE = .getVariable(timescores_indiv, outvar, catvar, idvar, lgm_type, k),
    ANALYSIS = .getAnalysis( lgm_type, timescores_indiv, catvar, estimator, transformation, starting_val, output),
    MODEL = .getModel(polynomial, timescores_indiv, outvar, timescores, k, lgm_type),
    OUTPUT = .getOutput(output),
    PLOT = .getPlot(outvar, plot),
    SAVEDATA = .getSaveData(lgm_type, polynomial, k, starting_val, save),
    usevariables = colnames(dplyr::select(data, dplyr::any_of(c(idvar, outvar, timescores)))),
    rdata = dplyr::select(data, tidyselect::all_of(c(idvar, outvar)), dplyr::any_of(timescores)),
    autov = FALSE
  )

  return(lgm_object)
}

# Helper functions -------------------------------------------------------------
## Format Mplus input ------------------------------------------------------------
# Formats input section by collapsing a character vector or list into a single character string, adding a semicolon at end of each line, adding newlines after each section,  wrapping each line under 90 characters.

.format <- function(section) {
  formated_section <- unlist(section) %>%
    MplusAutomation::parseMplus(add = TRUE) %>%
    stringr::str_remove_all("(?<=%);") %>%
    stringr::str_split("\n", simplify = TRUE) %>%
    strwrap(width = 90, exdent = 6) %>%
    paste0(collapse = "\n") %>%
    paste0("\n")

  return(formated_section)
}

## getTitle --------------------------------------------------------------------
# Creates the title section of an mplusObject.

.getTitle <- function(lgm_type, polynomial, k, starting_val) {
  title <- paste0(stringr::str_to_upper(lgm_type),
                  "_P",
                  polynomial,
                  "_K",
                  k,
                  "_S",
                  starting_val)

  return(.format(title))
}

## getVariable -----------------------------------------------------------------
# Creates the variable section of an mplusObject.

.getVariable <- function(timescores_indiv,
                         outvar,
                         catvar,
                         idvar,
                         lgm_type,
                         k) {
  usevar <- glue::glue("USEVAR = {paste(c(outvar, if (timescores_indiv == TRUE) timescores), collapse = ' ')}")

  categorical <- if (catvar == TRUE)
    glue::glue("CATEGORICAL = {paste(outvar, collapse = ' ')}")

  idvar <- glue::glue("IDVAR = {idvar}")

  classes <- if (lgm_type != 'gcm')
    glue::glue("CLASSES = c({k})")

  tscores <- if (timescores_indiv == TRUE)
    glue::glue("TSCORES = {paste(timescores, collapse = ' ')}")

  variable <- c(usevar, categorical, idvar, classes, tscores)

  return(.format(variable))
}

## getAnalysis -----------------------------------------------------------------
# Creates the analysis section of an mplusObject.

.getAnalysis <- function(lgm_type,
                         timescores_indiv,
                         catvar,
                         estimator,
                         transformation,
                         starting_val,
                         output) {
  analysis_type <- if (lgm_type == 'gcm') {
    "TYPE = GENERAL"
  } else if (timescores_indiv == TRUE) {
    "TYPE = MIXTURE RANDOM"
  } else {
    "TYPE = MIXTURE"
  }

  estimator <- if (catvar == TRUE) {
    glue::glue("ESTIMATOR = {match.arg(estimator)}")
  }

  link <- if (catvar == TRUE) {
    glue::glue("LINK = {match.arg(transformation)}")
  }

  starts <- glue::glue("STARTS = {paste(starting_val, if(lgm_type != 'gcm') starting_val/4)}")

  k1starts <- if ("TECH11" %in% output | "TECH14" %in% output) {
    glue::glue("K-1STARTS = {starting_val} {starting_val/4}")
  }

  lrtstarts <- if ("TECH14" %in% output) {
    glue::glue("LRTSTARTS = 0 0 {starting_val} {starting_val/4}")
  }

  processors <- glue::glue("PROCESSORS = {parallel::detectCores()}")

  analysis <- c(analysis_type, estimator, link, starts, k1starts, lrtstarts,  processors)

  return(.format(analysis))
}

## getModel --------------------------------------------------------------------
# Creates the model section of an mplusObject.

.getModel <- function(polynomial,
                      timescores_indiv,
                      outvar,
                      timescores,
                      k,
                      lgm_type) {
  ### %OVERALL%
  #### Growth factors
  gf_mean_var <- switch(
    polynomial,
    "1" = c("i s", "[i s]", "i-s@0"),
    "2" = c("i s q", "[i s q]", "i-q@0"),
    "3" = c("i s q cub", "[i s q cub]", "i-cub@0")
  )
  #### Growth model
  growth_model <- if (timescores_indiv == TRUE) {
    glue::glue(
      "{gf_mean_var[[1]]} | {first(outvar)} - {last(outvar)} AT {first(timescores)} - {last(timescores)}"
    )

  } else {
    outvar_timescores <- stringr::str_c(outvar, timescores, sep = "@") %>% # Generate list of user variables @ timescores
      paste(collapse = " ")

    glue::glue("{gf_mean_var[[1]]} | {outvar_timescores}")

  }

  ### Residual variance
  resvar_fix <- glue("{first(outvar)} - {last(outvar)} (1)")

  resvar_c <- purrr::map(seq(k),
                         \(k) glue::glue("{first(outvar)} - {last(outvar)} ({k})"))

  resvar_t <- purrr::imap_chr(outvar, \(x, idx) glue::glue("{x} ({idx})")) %>%
    rep(k) %>%
    split(rep(1:k, each = length(outvar)))

  resvar_tc <- rep(outvar, k) %>%
    purrr::imap_chr(\(x, idx) glue::glue_collapse(glue::glue("{x} ({idx})"))) %>%
    split(rep(1:k, each = length(outvar)))

  residual_var <- switch(
    lgm_type,
    "gcm" = resvar_t,
    "gbtm" = resvar_fix,
    "lcga_t" = resvar_t,
    "lcga_c" = resvar_c,
    "lcga_tc" = resvar_tc
  )

  ### %CLASS%
  class_gf_mean <- rep(gf_mean_var[[2]], k)
  class_gf_var <- rep(gf_mean_var[[3]], k)

  class_model <- list(glue::glue("%C#{1:k}%"),
                      class_gf_mean,
                      class_gf_var,
                      residual_var) %>%
    purrr::pmap(\(c, class_gf_mean, class_gf_var, residual_var)
                list(c, class_gf_mean, class_gf_var, residual_var))

  ### %OVERAL% & %CLASS%
  model <- if (lgm_type == 'gcm') {
    c(growth_model, gf_mean_var[[2]], gf_mean_var[[1]])

  } else {
    c("%OVERALL%",
      growth_model,
      gf_mean_var[[2]],
      gf_mean_var[[3]],
      class_model)
  }

  return(.format(c(model)))
}

## getOutput -------------------------------------------------------------------
# Creates the ouput section of an mplusObject.

.getOutput <- function(output) {
  output <- paste(output, collapse = " ")

  return(.format(output))
}

## getPlot ---------------------------------------------------------------------
# Creates the plot section of an mplusObject.

.getPlot <- function(outvar, plot) {
  plot_type <- glue::glue("TYPE = {plot}")

  series <- glue::glue("SERIES = {dplyr::first(outvar)}-{dplyr::last(outvar)} (*)")

  plot <- c(plot_type, series)

  return(.format(plot))
}

## getSaveData -----------------------------------------------------------------
# Creates the save data section of an mplusObject.

.getSaveData <- function(lgm_type,
                         polynomial,
                         k,
                         starting_val,
                         save) {
  file <- glue::glue("FILE = {toupper(lgm_type)}_P{polynomial}_K{k}_S{starting_val}_res.dat")

  save <- glue::glue("SAVE = {paste(save, collapse = ' ')}")

  savedata <- c(file, save)

  return(.format(savedata))
}
