#' @title Fit Growth Curve Models (GCM)

#' @description Customize and execute GCM in Mplus, offering flexibility in model configuration and parameter estimation.

#' @param data A data frame containing all variables for the trajectory analysis.
#' @param outvar A character vector specifying the outcome variables at different times.
#' @param catvar A logical value indicating whether the outcome variable is categorical. Default is `FALSE`.
#' @param idvar A character string specifying the ID variable.
#' @param starting_val A numeric value specifying the number of random starting values to generate for the initial optimization stage.
#' Note that the number of final stage optimizations will be set as equal to half of this value.
#' @param estimator A character string to specify the estimator to use in the analysis. Default is 'MLR'.
#' @param transformation A character string to specify the latent response variable transformation to use when the outcome variable is categorical. Default is `LOGIT`.
#' @param polynomial An integer specifying the order of the polynomial used to model trajectories. Supported values are:
#' 1 (linear), 2 (quadratic), 3 (cubic). Default is 1.
#' @param timescores A numeric vector specifying the time scores for the model. If `timescores_indiv = TRUE`,
#' a character vector should be used to specify variables with individually varying times of observation.
#' @param timescores_indiv A logical value indicating whether to use individually varying times of observation for the outcome variable. Default is `FALSE`.
#' @param output A character vector specifying the requested Mplus output options for the model.
#' @param plot A character string specifying the requested Mplus plot options for the model.
#' @param save A character string specifying the type of results to be saved by Mplus.
#' @param wd A character string specifying the directory where the results folder will be created for saving Mplus input, output, and data files.
#' Default is the current working directory.

#' @details The `fitGCM` function automates the process of specifying, customizing and fitting GCM in Mplus.
#'
#' This function builds upon the capabilities of the \code{\link[MplusAutomation]{mplusObject}} and \code{\link[MplusAutomation]{mplusModeler}} functions
#' from the MplusAutomation package.

#' @return A list of class `mplusObject`s including results for the fitted GCM.

#' @seealso
#' \code{\link{LGMobject}} for creating the mplusObject of a latent growth model.
#' \code{\link{runLGM}} for conducting latent growth modelling with an mplusObject.

#' @examples
#' \donttest{
#' # Example usage:
#' GCM_model <- fitGCM(
#'   data = symptoms,
#'   outvar = paste("sx", seq(from = 0, to = 24, by = 6), sep = "_"),
#'   catvar = FALSE,
#'   idvar = "id",
#'   starting_val = 500,
#'   polynomial = 3,
#'   timescores = seq(from = 0, to = 24, by = 6),
#'   timescores_indiv = FALSE,
#'   output = c("TECH1", "SAMPSTAT", "STANDARDIZED"),
#'   plot = "PLOT3",
#'   save = "FSCORES",
#'   wd = file.path("Results", "Trajectories")
#'   )
#' }

#' @export

# FitGCM function -----------------------------------------------------------
fitGCM <- function(data,
                   outvar,
                   catvar = FALSE,
                   idvar,
                   starting_val = 500,
                   polynomial = 1,
                   timescores = timescores,
                   timescores_indiv = FALSE,
                   estimator = c("MLR", "ML", "WLSMV", "WLS"),
                   transformation = c("LOGIT", "PROBIT"),
                   output = c("TECH1", "SAMPSTAT", "STANDARDIZED"),
                   plot = "PLOT3",
                   save = "FSCORES",
                   wd = "Results") {

## Validate Arguments  ---------------------------------------------------------
### Arguments class
  stopifnot(
    is.data.frame(data),
    is.logical(c(catvar, timescores_indiv)),
    is.numeric(starting_val),
    is.vector(outvar),
    is.character(c(idvar, outvar, output, plot, save, wd))
  )

### Arguments value
  stopifnot(
    length(idvar) == 1,
    length(outvar) == length(timescores),
    polynomial < length(timescores),
    polynomial %in% 1:3,
    starting_val >= 0
  )

## Get GCM Mplus object --------------------------------------------------------
  GCM_object <- LGMobject(
    data = data,
    outvar = outvar,
    catvar = catvar,
    idvar = idvar,
    k = 1L,
    starting_val = starting_val,
    lgm_type = "gcm",
    residuals = "time",
    polynomial = polynomial,
    timescores = timescores,
    timescores_indiv = timescores_indiv,
    estimator = estimator,
    transformation = transformation,
    output = output,
    plot = plot,
    save = save
  )

  ## Run GCM Mplus object ------------------------------------------------------
  GCM_model <- runLGM(
    lgm_object = GCM_object[[1]],
    wd = wd
  )

  return(GCM_model)
}

