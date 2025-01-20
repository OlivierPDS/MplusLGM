#' @title Select best-fitting model from a list of Latent Growth Models (LGM)
#'
#' @description Identify and extract the best-fitting model from a list of LGM
#' based on a specified set of criteria applied to a summary table of the models fit indices.

#' @param lgm_object A list of LGM `mplusObject` to evaluate.
#' @param ic A character string specifying the information criterion (IC) to use for selecting the best-fitting model.
#' Supported options are Bayesian Information Criterion (BIC), sample-size-adjusted BIC (aBIC), Akaike Information Criterion (AIC),
#' Consistent Akaike Information Criterion (CAIC), and AIC corrected (AICC). The default is BIC.
#' @param lrt A character string specifying the likelihood ratio test (LRT) to use for selecting the best-fitting model.
#' Supported options are Bootstrap LRT (BLRT) and Lo-Mendel-Rubin adjusted LRT (aLRT).
#' Default is "none", the selection of the the best-fitting model is only made based on the selected IC.
#' @param p A numeric value specifying the p-value threshold for statistical significance when using LRT-based selection
#' of the best-fitting model. Default is 0.05.

#' @details The function select the best-fitting model based on the following criteria:
#' \itemize{
#'   \item 1. Models with convergence errors are excluded.
#'   \item 2. The model with the lowest information criterion (IC) is selected.
#'   \item 3. If specified, the likelihood ratio test (LRT) is used to determine whether the K-class model can be reduced to K-1 classes.
#'   \item 4. The resulting model throw a warning if it meet any of the following conditions:
#'     \itemize{
#'       \item - Entropy is below 0.5.
#'       \item - Any class has an average posterior probability of assignment (APPA) below 0.7.
#'       \item - Any class represents less than 5% of the sample size.
#'     }
#' }

#' @return The LGM `mplusObject` of the best-fitting model.

#' @examples
#' \dontrun{
#' # Example usage:
#' GBTM_best <- getBest(
#'   lgm_object = GBTM_models,
#'   ic = "BIC",
#'   lrt = "aLRT",
#'   p = 0.05
#' )
#'
#' best_fit <- getFit(GBTM_best)
#'
#' print(best_fit)
#' }

#' @importFrom dplyr filter mutate case_when if_any pull
#' @importFrom tidyselect starts_with
#' @importFrom purrr keep pluck
#' @importFrom stringr str_detect
#' @importFrom glue glue
#' @importFrom stats na.omit

#' @export

# getBest Function -------------------------------------------------------------
getBest <- function(lgm_object,
                    ic = c("BIC", "aBIC", "AIC", "CAIC", "AICC"),
                    lrt = c("none", "aLRT", "BLRT"),
                    p = 0.05) {
  ## Validate arguments --------------------------------------------------------
  stopifnot(
    .is.mplusObject(lgm_object),
    is.numeric(p)
  )

  ic <- match.arg(ic, choices = c("BIC", "aBIC", "AIC", "CAIC", "AICC"))

  lrt <- match.arg(lrt, choices = c("none", "aLRT", "BLRT")) %>%
    switch(
      "none" = NULL,
      "aLRT" = "T11_LMR_PValue",
      "BLRT" = "BLRT_PValue"
    )

  ## Extract fit indices from list of models
  table <- getFit(lgm_object) %>%
    dplyr::filter(is.na(Errors)) %>%
    dplyr::arrange(desc(NLatentClasses), ic)

  stopifnot(
    ic %in% names(table),
    lrt %in% names(table)
  )

  ## Select then extract title of best-fitting model  --------------------------
  best_ic <- table %>%
    dplyr::slice_min(.data[[ic]], n = 1)

  title <- dplyr::pull(best_ic, Title)

  print(glue::glue("The model with the best {ic} value is: {title}"))

  if (is.null(lrt)) {
    if (any(c("T11_LMR_PValue", "BLRT_PValue") %in% names(table))) {
      lrt <- names(table) %>%
        stringr::str_subset("PValue$")

      if (pull(best_ic, lrt) > p) {
        print(
          glue::glue(
            "Warnings: The {lrt} p-value is not significant (p > {p}), indicating that the number of classes (K) can be reduced to K-1."
          )
        )
      }
      if (pull(best_ic, lrt) <= p) {
        print(
          glue::glue(
            "Note: The {lrt} p-value is significant (p <= {p}), indicating that the number of classes cannot be reduced."
          )
        )
      }
    } else {
      print(
        glue::glue("Warning: Use LRT to assess whether the number of classes (K) can be reduced to K-1.")
      )
    }
  } else {
    best_lrt <- table %>%
      dplyr::filter(NLatentClasses <= dplyr::pull(best_ic, NLatentClasses)) %>%
      dplyr::filter(.data[[lrt]] <= p) %>%
      dplyr::slice(1)

    if (pull(best_ic, lrt) > p) {
      title <- pull(best_lrt, Title)

      print(
        glue::glue(
          "The {lrt} p-value was not significant (p > 0.05), indicating that the number of classes (K) could be reduced to K = {pull(best_lrt, NLatentClasses)}."
        ),
        glue::glue(
          "The suggested best-fitting model is: {title}."
        )
      )
    }
    if (pull(best_ic, lrt) <= p) {
      print(
        glue::glue(
          "The LRT p-value is significant (p <= {p}), indicating that the number of classes cannot be reduced."
        )
      )
    }
  }

  if (!is.null(title)) {
    table <- table %>%
      filter(Title == title)
  } else {
    stop(print("Warning: No model meet the selection criteria"))
  }


  if (pull(table, APPA_criterion) == "fail") {
    print(
      glue(
        "Warning: Some classes have an APPA below 0.7, indicating lower certainty in classification.
        Some individuals have a weak probability of belonging to their assigned class."
      )
    )
  }
  if (pull(table, Entropy_criterion) == "fail") {
    print(
      glue(
        "Warning: Entropy is below 0.5, indicating poor separation between classes and increased uncertainty in class assignments."
      )
    )
  }
  if (pull(table, Proportion_criterion) == "fail") {
    print(
      glue(
        "Warning: Some classes represent less than 5% of the sample, raising concerns about their practical
        significance, relevance, and interpretability."
      )
    )
  }

  ## Extract best model --------------------------------------------------------
  lgm_object <- lgm_object %>%
    purrr::modify_tree(
      is_node = \(node) dplyr::first(class(node[[1]])) == "list",
      post = \(node) purrr::list_flatten(node)
    )

  best_model <- lgm_object %>%
    purrr::keep(\(lgm_object)
    stringr::str_detect(
      string = purrr::pluck(lgm_object, "TITLE"),
      pattern = title
    ))


  return(best_model)
}
