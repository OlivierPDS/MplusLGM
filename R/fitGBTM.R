#' @title Fit Group-Based Trajectory Models (GBTM) for class enumeration.

#' @description Perform class enumeration by fitting a series of GBTM in Mplus across a predetermined range of classes,
#' and returning a list of fitted models for evaluation and comparison.

#' @param data A data frame containing all variables for the trajectory analysis.
#' @param outvar A character vector specifying the outcome variables at different times.
#' @param catvar A logical value indicating whether the outcome variable is categorical. Default is `FALSE`.
#' @param idvar A character string specifying the ID variable.
#' @param min_k An integer specifying the minimum number of latent classes to evaluate. Default is 2.
#' @param max_k An integer specifying the maximum number of latent classes to evaluate. Default is 6.
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
#' @param plot A character string specifying the requested Mplus plot options for the model. Default is PLOT3.
#' @param save A character string specifying the type of results to be saved by Mplus. Default is FSCORES.
#' @param wd A character string specifying the directory where the results folder will be created for saving Mplus input, output, and data files.
#' Default is the current working directory.

#' @return A list of `mplusObject` including the fitted GBTM models for each class specification.

#' @details The `fitGBTM` function automates the process of fitting GBTM, iterating through an increasing number of class.
#' This function is designed for conducting class enumeration and help identifying the optimal number of latent classes.
#' GBTM should converge the quickest to a solution given its lower number of free parameters when compared to other LGM.
#'
#' The function operates as follows:
#' \itemize{
#'   \item 1. Iterate over an increasing number of classes, ranging from `min_k` to `max_k`.
#'   \item 2. Create GBTM `mplusObject` with appropriate class specification using the `LGMobject` function.
#'   \item 3. Fit models using the `runLGM` function, ensuring convergence by increasing the number of random starting values until the best log-likelihood is replicated.
#'   \item 4. Return a list of `mplusObject` including results for the fitted GBTM models with each class structures.
#' }
#'
#' The function automates the procedure outlined for model selection in:
#' Van Der Nest et al,. (2020). "An overview of mixture modelling for latent evolutions in longitudinal data: Modelling approaches, fit statistics and software."
#' Advances in Life Course Research 43: 100323.
#'
#' This function builds upon the capabilities of the \code{\link[MplusAutomation]{mplusObject}} and \code{\link[MplusAutomation]{mplusModeler}} functions
#' from the MplusAutomation package.

#' @seealso
#' \code{\link{LGMobject}} for creating the mplusObject of a latent growth model.
#' \code{\link{runLGM}} for conducting latent growth modelling with an mplusObject.

#' @examples
#' \donttest{
#' # Example usage:
#' GBTM_models <- fitGBTM(
#'   data = symptoms,
#'   outvar = paste("sx", seq(from = 0, to = 24, by = 6), sep = "_"),
#'   catvar = FALSE,
#'   idvar = "id",
#'   starting_val = 500,
#'   min_k = 2L,
#'   max_k = 6L,
#'   timescores = seq(from = 0, to = 24, by = 6),
#'   timescores_indiv = FALSE,
#'   polynomial = 1,
#'   output = c("TECH1", 'TECH14', "SAMPSTAT", "STANDARDIZED"),
#'   plot = "PLOT3",
#'   save = "FSCORES",
#'   wd = file.path("Results", "Trajectories")
#' )
#'
#' # Accessing the model:
#' GBTM2 <- GBTM_models[[1]] #with 2 latent classes
#' GBTM3 <- GBTM_models[[2]] #with 3 latent classes
#' GBTM4 <- GBTM_models[[3]] #with 4 latent classes
#' }

#' @importFrom purrr compact

#' @export

# fitGBTM function -------------------------------------------------------------
fitGBTM <- function(data,
                    outvar,
                    catvar = FALSE,
                    idvar,
                    min_k = 2L,
                    max_k = 6L,
                    starting_val = 500,
                    polynomial = 1,
                    timescores,
                    timescores_indiv = FALSE,
                    estimator = c("MLR", "ML", "WLSMV", "WLS"),
                    transformation = c("LOGIT", "PROBIT"),
                    output = c("TECH1", "TECH11", "SAMPSTAT", "STANDARDIZED"),
                    plot = "PLOT3",
                    save = "FSCORES",
                    wd = "Results") {

  ## Validate arguments --------------------------------------------------------
  ### Argument class
  stopifnot(
    is.data.frame(data),
    is.logical(c(catvar, timescores_indiv)),
    is.numeric(starting_val),
    is.integer(c(min_k, max_k)),
    is.vector(outvar),
    is.character(c(outvar, idvar, output, plot, save, wd))
  )

  ### Argument value
  stopifnot(
    length(idvar) == 1,
    length(outvar) == length(timescores),
    starting_val >= 0,
    (min_k & max_k) > 0,
    polynomial %in% 1:3,
    polynomial < length(timescores)
  )

  ## Run GBTM  --------------------------------------------------------------------
  ### Class enumeration from classes = min_k to classes = max_k.

  GBTM_models <- list()

  for (k in min_k:max_k) {
    GBTM_object <- LGMobject(
      data = data,
      outvar = outvar,
      catvar = catvar,
      idvar = idvar,
      k = k,
      starting_val = starting_val,
      lgm_type = "gbtm",
      residuals = "fix",
      polynomial = polynomial,
      timescores = timescores,
      timescores_indiv = timescores_indiv,
      estimator = estimator,
      transformation = transformation,
      mplus_model = NULL,
      output = output,
      plot = plot,
      save = save
    )

    GBTM_models[[k]] <- runLGM(
      GBTM_object[[1]],
      wd = wd)

  }

  return(purrr::compact(GBTM_models))

}
