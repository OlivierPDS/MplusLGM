#' @title Create Mplus model objects for Latent Growth Modelling (LGM)

#' @description Facilitates the specification of Mplus LGM objects, allowing control over the number of latent classes, polynomial growth terms, residual variance structures, and inclusion of random effects.
#' Support Growth Curve Models (GCM), Growth-Based Trajectory Models (GBTM), Latent Class Growth Analysis (LCGA) and Growth Mixture Models (GMM).
#' Once specified, the model object can be estimated using the `runLGM()` function.

#' @param data A data frame containing all variables for the trajectory analysis.
#' @param outvar A character vector specifying the outcome variables at different times.
#' @param catvar A logical value indicating whether the outcome variable is categorical. Default is `FALSE`.
#' @param idvar A character string specifying the ID variable.
#' @param k An integer specifying the number of latent classes for the model.
#' @param starting_val A numeric value specifying the number of random starting values to generate for the initial optimization stage.
#' Note that the number of final stage optimizations will be set as equal to half of this value.
#' @param estimator A character string to specify the estimator to use in the analysis. Default is 'MLR'.
#' @param transformation A character string to specify the latent response variable transformation to use when the outcome variable is categorical. Default is `LOGIT`.
#' @param lgm_type A character string specifying the type of latent growth model to build. Options include: gcm, gbtm, lcga and gmm.
#' @param residuals A character string specifying the structure of the residual variance (variability of the errors at each time point).
#' Options include:
#' \itemize{
#'    \item "fix" (fixed residual variance across time and class),
#'    \item "time" (free residual variance across time),
#'    \item "class" (free residual variance across class),
#'    \item "time_class" (free residual variance across both time and class).
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
#' @param mplus_model A character string specifying a custom model using Mplus language. Default is `NULL`.
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
#'   lgm_type = "gmm",
#'   residuals = c("fix", "time", "class", "time_class"),
#'   random_effect = "class_invariant",
#'   polynomial = 3,
#'   timescores = seq(from = 0, to = 24, by = 6),
#'   timescores_indiv = FALSE,
#'   output = c("TECH1", "TECH14", "SAMPSTAT", "STANDARDIZED"),
#'   plot = "PLOT3",
#'   save = "FSCORES"
#' )
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
                      lgm_type = c("gcm", "gbtm", "lcga", "gmm"),
                      residuals = c("fix", "time", "class", "time_class"),
                      random_effect = c("class_invariant", "class_variant"),
                      polynomial = 1,
                      timescores,
                      timescores_indiv = FALSE,
                      mplus_model = NULL,
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

  if (!is.null(mplus_model)) {
    stopifnot(is.character(mplus_model))
  }

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
  lgm_object <- .getModel(polynomial, lgm_type, residuals, timescores_indiv, outvar, timescores, k, random_effect, mplus_model) %>%
    imap(\(model, idx) MplusAutomation::mplusObject(
      TITLE = .getTitle(lgm_type, random_effect, idx, residuals, polynomial, k, starting_val),
      VARIABLE = .getVariable(timescores_indiv, outvar, catvar, idvar, lgm_type, k),
      ANALYSIS = .getAnalysis(lgm_type, timescores_indiv, catvar, estimator, transformation, starting_val, output),
      MODEL = model,
      OUTPUT = .getOutput(output),
      PLOT = .getPlot(outvar, plot),
      SAVEDATA = .getSaveData(lgm_type, polynomial, k, starting_val, save),
      usevariables = colnames(dplyr::select(data, dplyr::any_of(c(idvar, outvar, timescores)))),
      rdata = dplyr::select(data, tidyselect::all_of(c(idvar, outvar)), dplyr::any_of(timescores)),
      autov = FALSE
    ))
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

.getTitle <- function(lgm_type, random_effect, idx, residuals, polynomial, k, starting_val) {

    model <- lgm_type %>%
      stringr::str_to_upper()

    res_initials <- residuals %>%
      stringr::str_to_upper() %>%
      stringr::str_split("_", simplify = TRUE) %>%
      stringr::str_sub(1, 1) %>%
      stringr::str_c(collapse = "")

    rand_initials <- if (lgm_type == "gmm") {
      random_effect %>%
        stringr::str_to_upper() %>%
        stringr::str_split("_", simplify = TRUE) %>%
        stringr::str_sub(1, 1) %>%
        stringr::str_c(collapse = "") %>%
        stringr::str_c("_", ., idx)
    } else {
      ""
    }

    title <- str_c(model, "_K", k, "_P", polynomial, "_", res_initials, rand_initials, "_S", starting_val)

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

  categorical <- if (catvar == TRUE) {
    glue::glue("CATEGORICAL = {paste(outvar, collapse = ' ')}")
  }

  idvar <- glue::glue("IDVAR = {idvar}")

  classes <- if (lgm_type != "gcm") {
    glue::glue("CLASSES = c({k})")
  }

  tscores <- if (timescores_indiv == TRUE) {
    glue::glue("TSCORES = {paste(timescores, collapse = ' ')}")
  }

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
  analysis_type <- if (lgm_type == "gcm") {
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

  analysis <- c(analysis_type, estimator, link, starts, k1starts, lrtstarts, processors)

  return(.format(analysis))
}

## getModel --------------------------------------------------------------------
# Creates the model section of an mplusObject.

.getModel <- function(polynomial,
                      lgm_type,
                      residuals,
                      timescores_indiv,
                      outvar,
                      timescores,
                      k,
                      random_effect,
                      mplus_model) {
  ### Growth factors
  #### Mean / Fixed effects
  gf <- switch(polynomial,
    "1" = c("i", "s"),
    "2" = c("i", "s", "q"),
    "3" = c("i", "s", "q", "cub")
  )

  gf_mean <- glue::glue("[{paste(gf, collapse = ' ')}]")

  #### Variance / Random effects
  gf_var <- purrr::map_chr(seq(0, polynomial + 1), ~ stringr::str_c(ifelse(
    seq_along(gf) <= .x, gf, str_c(gf, "@0")
  ), collapse = " "))

  gf_var <- switch(lgm_type,
    "gcm" = utils::tail(gf_var, 1),
    "gbtm" = utils::head(gf_var, 1),
    "lcga" = utils::head(gf_var, 1),
    "gmm" = utils::tail(gf_var, -1)
  )

  ### Residual variance
  resvar_fix <- glue::glue("{first(outvar)} - {last(outvar)} (1)")

  resvar_c <- purrr::map(
    seq(k),
    \(k) glue::glue("{first(outvar)} - {last(outvar)} ({k})")
  )

  resvar_t <- purrr::imap_chr(outvar, \(x, idx) glue::glue("{x} ({idx})")) %>%
    rep(k) %>%
    split(rep(1:k, each = length(outvar)))

  resvar_tc <- rep(outvar, k) %>%
    purrr::imap_chr(\(x, idx) glue::glue_collapse(glue::glue("{x} ({idx})"))) %>%
    split(rep(1:k, each = length(outvar)))

  residual_var <- switch(residuals,
    "fix" = resvar_fix,
    "time" = resvar_t,
    "class" = resvar_c,
    "time_class" = resvar_tc,
  )

  ### Growth model
  growth_model <- if (timescores_indiv == TRUE) {
    glue::glue(
      "{paste(gf, collapse = ' ')} | {first(outvar)} - {last(outvar)} AT {first(timescores)} - {last(timescores)}"
    )
  } else {
    outvar_timescores <- stringr::str_c(outvar, timescores, sep = "@") %>% # Generate list of user variables @ timescores
      paste(collapse = " ")

    glue::glue("{paste(gf, collapse = ' ')} | {outvar_timescores}")
  }

  ### %OVERALL%
  overall_model <- gf_var %>%
    purrr::map(
      \(gf_var) c(
        if (lgm_type == "gcm") {
          NULL
        } else {
          "%OVERALL%"
        },
        growth_model,
        gf_mean,
        gf_var
      )
    )

  ### %CLASS%
  class_model <- if (lgm_type == "gcm") {
    NULL
  } else if (lgm_type %in% c("gbtm", "lcga") || (lgm_type == "gmm" && random_effect == "class_invariant")) {
    list(
      glue::glue("%C#{1:k}%"),
      rep(gf_mean, k),
      # rep(head(gf_var, 1), k),
      residual_var
    ) %>%
      purrr::pmap(
        \(c,
          gf_mean,
          # gf_var,
          residual_var)

      list(
        c,
        gf_mean,
        # gf_var,
        residual_var
      ))
  } else if (lgm_type == "gmm" && random_effect == "class_variant") {
    gf_var %>%
      map(\(gf_var)
      purrr::pmap(
        list(
          glue::glue("%C#{1:k}%"),
          rep(gf_mean, k),
          rep(gf_var, k),
          residual_var
        ),
        \(c,
          gf_mean,
          gf_var,
          residual_var)

        list(c,
             gf_mean,
             gf_var,
             residual_var)
      ))
  }

  ### %OVERAL% & %CLASS%
  model <- if (!is.null(mplus_model)) {
    .format(mplus_model)
  } else if (lgm_type %in% c("gcm", "gbtm", "lcga")) {
    c(overall_model, class_model) %>%
      .format()
  } else if (lgm_type == "gmm" && random_effect == "class_invariant") {
    overall_model %>%
      purrr::map(\(overall_model)
      c(overall_model, class_model)) %>%
      purrr::map(~.format(.x))
  } else if (lgm_type == "gmm" && random_effect == "class_variant") {
    overall_model %>%
      purrr::map2(class_model, \(overall_model, class_model)
      c(overall_model, class_model)) %>%
      purrr::map(~.format(.x))
  }

  return(model)
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

