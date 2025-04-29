#' @title Fit Growth Mixture Models (GMM) for estimating random effects.
#'
#' @description Add class-variant and class-invariant random eﬀect variances stepwise,
#' by fitting a series of GMM models and returning a list of fitted models for evaluation and comparison.

#' @param data A data frame containing all variables for the trajectory analysis.
#' @param outvar A character vector specifying the outcome variables at different times.
#' @param catvar A logical value indicating whether the outcome variable is categorical. Default is `FALSE`.
#' @param idvar A character string specifying the ID variable.
#' @param k An integer specifying the number of latent classes for the model.
#' @param starting_val A numeric value specifying the number of random starting values to generate for the initial optimization stage.
#' Note that the number of final stage optimizations will be set as equal to half of this value.
#' @param estimator A character string to specify the estimator to use in the analysis. Default is 'MLR'.
#' @param transformation A character string to specify the latent response variable transformation to use when the outcome variable is categorical. Default is `LOGIT`.
#' @param residuals A character string specifying the structure of the residual variance (variability of the errors at each time point).
#' Options include:
#' \itemize{
#'    \item "fix" (fixed residual variance across time and class),
#'    \item "time" (free residual variance across time),
#'    \item "class" (free residual variance across class),
#'    \item "both" (free residual variance across both time and class).
#'    }
#' @param random_effect A character string specifying the structure of the random effects.
#' Options include:
#' \itemize{
#'   \item "class_invariant" – Random effect variances are constrained to be equal across latent classes.
#'   \item "class_variant" – Random effect variances are freely estimated for each latent class.
#' }
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

#' @return A list of `mplusObject` including results for the fitted GMM models.

#' @details The `fitGMM` function automates the process of fitting GMM models, iterating through 3 varying residual variance specifications:
#' \itemize{
#'   \item - Relaxed residual variance across time
#'   \item - Relaxed residual variance across class
#'   \item - Relaxed residual variance across both time and class
#' }
#' This function is designed to help identify the optimal residual variance structure while examining convergence issues as model complexity increases.
#'
#' The function operates as follows:
#' \itemize{
#'   \item 1. Iterate over the 3 residual variance specifications
#'   \item 2. Create LCGA `mplusObject` with appropriate residual variance specification using the `LGMobject` function.
#'   \item 3. Fit models using the `runLGM` function, ensuring convergence by increasing the number of random starting values until the best log-likelihood is replicated.
#'   \item 4. Return a list of `mplusObject` including results for the fitted LCGA models with each residual variance structures
#' }
#'
#' The function automates the procedure outlined for model selection in:
#' Van Der Nest et al., (2020). "An overview of mixture modelling for latent evolutions in longitudinal data: Modelling approaches, fit statistics and software."
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
#' GMM_models <- fitGMM(
#'  data = symptoms,
#'  outvar = paste('sx', seq(from = 0, to = 24, by = 6), sep = "_"),
#'  catvar = FALSE,
#'  idvar = "id",
#'  starting_val = 500,
#'  polynomial = 3,
#'  k = 3L,
#'  timescores = seq(from = 0, to = 24, by = 6),
#'  timescores_indiv = FALSE,
#'  residuals = c("fix", "time", "class", "both"),
#'  random_effect = "class_invariant",
#'  output = c('TECH1', 'TECH14', 'SAMPSTAT', 'STANDARDIZED'),
#'  wd = file.path('Results', 'Trajectories')
#')
#'
#' # Accessing the models:
#' LCGA_t <- LCGA_models[[1]] #with relaxed residual variance across time
#' LCGA_c <- LCGA_models[[2]] #with relaxed residual variance across class
#' LCGA_tc <- LCGA_models[[3]] #with relaxed residual variance across time and class
#' }

#' @export

# GMM function -----------------------------------------------------------
fitGMM <- function(data,
                   outvar,
                   catvar = FALSE,
                   idvar,
                   k,
                   starting_val = 500,
                   polynomial = 1,
                   timescores,
                   timescores_indiv = FALSE,
                   estimator = c("MLR", "ML", "WLSMV", "WLS"),
                   transformation = c("LOGIT", "PROBIT"),
                   residuals = c("fix", "time", "class", "time_class"),
                   random_effect = c("class_invariant", "class_variant"),
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
    is.integer(k),
    is.vector(outvar),
    is.character(c(outvar, idvar, output, plot, save, wd))
  )

  ### Argument value
  stopifnot(
    length(idvar) == 1,
    length(outvar) == length(timescores),
    starting_val >= 0,
    k > 0,
    polynomial %in% 1:3,
    polynomial < length(timescores)
  )

  ## Run GMM -------------------------------------------------------------------

  GMM_models <- list()

  for (m in residuals) {
    GMM_objects <- LGMobject(
      data = data,
      outvar = outvar,
      catvar = catvar,
      idvar = idvar,
      k = k,
      starting_val = starting_val,
      lgm_type = "gmm",
      residuals = m,
      random_effect = random_effect,
      polynomial = polynomial,
      timescores = timescores,
      timescores_indiv = timescores_indiv,
      estimator = estimator,
      transformation = transformation,
      output = output,
      plot = plot,
      save = save
    )

    GMM_models[[m]] <- GMM_objects %>%
      map(\(obj) runLGM(obj, wd = wd))

  }
  return(GMM_models)
}
