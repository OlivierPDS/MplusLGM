#' @title Get fit indices from Latent Growth Models (LGM)

#' @description Extract key information from Mplus LGM objects, including model summaries,
#' fit statistics, class details, warnings, and errors. The function accounts for non-converging
#' models and compiles the extracted information into a single data frame to facilitate
#' model evaluation and comparison.

#' @param lgm_object A single LGM `mplusObject` or a list of LGM `mplusObject` (nested lists supported).

#' @return A  data frame with a row for each LGM of the input list.

#' @details
# ’ The data frame includes:
#' \itemize{
#'   \item - Model summaries such as the title, log-likelihood value and  number of observations, parameters and latent classes.
#'   \item - Model fit indices such as the BIC, aBIC, AIC, AICC and CAIC along with statistics from the BLRT and adjusted LMR-LRT, if requested.
#'   \item - Latent class counts and proportions.
#'   \item - Classification confidence measures such as the average posterior probabilities (APPA) and entropy.
#'   \item - Mplus warnings or errors encountered during model estimation.
#' }
#'
#' This output facilitates side-by-side comparison of models to support model evaluation and selection.

#' @importFrom purrr reduce pluck modify_tree list_flatten map
#' @importFrom dplyr mutate across rename_with full_join select bind_rows tibble na_if arrange desc
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_trim str_to_title
#' @importFrom tidyselect any_of starts_with

#' @export

#' @examples
#' \dontrun{
#' # Example usage:
#' fit_indices <- getFit(lgm_object = GCM_model)
#' fit_indices <- getFit(lgm_object = list(GCM_model, GBTM_models, LCGA_models))
#'
#' print(fit_indices)
#' }
# getFit Function --------------------------------------------------------------
getFit <- function(lgm_object) {
  # Validate argument
  stopifnot(.is.mplusObject(lgm_object))

  ## Make table for each model listed ------------------------------------------
  list_table <- lgm_object %>%
    purrr::modify_tree(
      leaf = \(x) .getTable(x),
      is_node = \(x) dplyr::first(class(x)) != "mplusObject"
    )

  ## Compile all tables into 1 -------------------------------------------------
  table <- if (is.data.frame(list_table)) {
    list_table
  } else {
    purrr::list_flatten(list_table) %>%
      purrr::reduce(~ dplyr::bind_rows(.x, .y))
  }

  ## Compute additional information criteria -----------------------------------
  table <- table %>%
    dplyr::mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("proportion"), ~ round((.x * 100), digits = 2)))

  table <- tryCatch(
    table %>%
    dplyr::mutate(Entropy_criterion = dplyr::case_when(Entropy < 0.5 ~ "fail", .default = "pass")) %>%
    dplyr::mutate(Proportion_criterion = dplyr::case_when(
      dplyr::if_any(tidyselect::starts_with("proportion"), ~ .x < 5) ~ "fail", .default = "pass")) %>%
    dplyr::mutate(APPA_criterion = dplyr::case_when(
      dplyr::if_any(tidyselect::starts_with("APPA"), ~ .x < 0.7) ~ "fail", .default = "pass")),
    error = function(e) {
      table
    }
  )

    table <- table %>%
    dplyr::select(
      tidyselect::any_of(
        c(
          "Title",
          "Observations",
          "Parameters",
          "NLatentClasses",
          "LL",
          "BIC",
          "aBIC",
          "AIC",
          "AICC",
          "CAIC",
          "BLRT_Value",
          "BLRT_PValue",
          "T11_LMR_Value",
          "T11_LMR_PValue"
        )
      ),
      starts_with(
        c(
          "count",
          "proportion",
          "APPA",
          "Entropy"
        )
      ),
      c("Warnings", "Errors")
    )

  return(table)
}

## Helper functions ------------------------------------------------------------
### getTable -------------------------------------------------------------------
.getTable <- function(lgm_object) {
  ### Get model title ----------------------------------------------------------
  title <- lgm_object %>%
    purrr::pluck("TITLE") %>%
    stringr::str_trim()

  ### Get model errors & warnings ----------------------------------------------
  warnings <- lgm_object %>%
    purrr::pluck("results", "warnings")

  errors <- lgm_object %>%
    purrr::pluck("results", "errors")

  warn_err <- dplyr::tibble(
    Title = title,
    Warnings = paste(unlist(warnings), collapse = " "),
    Errors = paste(unlist(errors), collapse = " ")
  ) %>%
    dplyr::mutate(dplyr::across(c("Warnings", "Errors"), ~ dplyr::na_if(.x, "")))

  ### Discard models with errors ---------------------------------------------
  # sublgm_object <- lgm_object %>%
  #   purrr::discard(
  #     purrr::map_vec(errors, ~ !is_empty(.x))
  #   )

  ### Get Average posterior probabilities --------------------------------------
  appa <- tryCatch(
    lgm_object %>%
      purrr::pluck("results", "class_counts", "avgProbs.mostLikely", .default = NA) %>%
      diag() %>%
      dplyr::tibble(value = .) %>%
      tibble::rowid_to_column("name") %>%
      tidyr::pivot_wider(
        names_prefix = "APPA",
        names_from = "name",
        values_from = "value"
      ) %>%
      dplyr::mutate(Title = title),
    error = function(e) {
      dplyr::tibble(Title = title)
    }
  )

  ### Get class counts & proportions -------------------------------------------
  class_count <- tryCatch(
    lgm_object %>%
      purrr::pluck("results", "class_counts", "mostLikely", .default = NA) %>%
      tidyr::pivot_wider(
        names_from = "class",
        values_from = c("count", "proportion")
      ) %>%
      dplyr::mutate(Title = title),
    error = function(e) {
      dplyr::tibble(Title = title)
    }
  )

  ### Get model fit statistics -------------------------------------------------
  summary <- lgm_object %>%
    purrr::pluck("results", "summaries") %>%
    dplyr::mutate(Title = stringr::str_trim(Title))

  ### Create table -------------------------------------------------------------
  # by merging model summary, appa, class count & proportions, warnings & errors
  table <- list(summary, appa, class_count, warn_err) %>%
    purrr::reduce(~ dplyr::right_join(.x, .y, by = "Title"))

  return(table)
}

### is.mplusObject -------------------------------------------------------------
.is.mplusObject <- function(object) {
  object %>%
    modify_tree(
      leaf = \(x) first(class(x)) == "mplusObject",
      is_node = \(x) first(class(x)) == "list",
    ) %>%
    unlist() %>%
    all()
}

## Declare global variables ----------------------------------------------------
utils::globalVariables(c(
  ".",
  "LL",
  "Parameters",
  "Observations",
  "BIC",
  "desc"
))
