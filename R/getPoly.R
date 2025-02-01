#' @title Refine Polynomial Order in Latent Growth Modelling (LGM)

#' @description Refine the polynomial order for each class of a LGM
#' by iteratively removing non-significant growth factors, and running the updated models.

#' @param lgm_object A LGM `mplusObject`, typically generated with the `LGMobject` and `runLGM` functions.
#' @param wd A character string specifying the directory where the results folder will be created for saving Mplus input, output, and data files.
#' Default is the current working directory.

#' @return A LGM `mplusObject` including the results of the updated model with the refined polynomial order.

#' @details The `getPoly` function refines the polynomial order of a LGM `mplusObject` through an iterative process.
#' In addition to ensuring the statistical significance of growth factors in each latent class,
#' the function ensure that the best loglikelihood value of the updated model is replicated.
#'
#' The function works as follows:
#' \itemize{
#'   \item 1. Extract model information from the provided LGM `mplusObject`.
#'   \item 2. Evaluate the statistical significance of the highest-order growth factor in each class.
#'   \item 3. Remove non-significant growth factors (p-value > 0.05) from the model.
#'   \item 4. Update the LGM `mplusObject` to reflect changes in the growth factor structure.
#'   \item 5. Re-run the updated `mplusObject` until log-likelihood values are successfully replicated using the `runLGM` function.
#'   \item 6. Repeat the process until the highest-order growth factor of all classes are statistically significant or reduce to intercept-only.
#'     }
#'
#' The function automates the procedure outlined for model selection in:
#' Van Der Nest et al,. (2020). "An overview of mixture modelling for latent evolutions in longitudinal data: Modelling approaches, fit statistics and software."
#' Advances in Life Course Research 43: 100323.

#' @examples
#' \donttest{
#' # Example usage:
#' final_model <- getPoly(
#'   lgm_object = LCGA_best,
#'   wd = "Results"
#'   )
#'
#' final_fit <- getFit(final_model)
#'
#'  print(final_fit)
#' }

#' @seealso
#' \code{\link{LGMobject}} for creating the mplusObject of a latent growth model.
#' \code{\link{runLGM}} for conducting latent growth modelling with a mplusObject.

#' @importFrom purrr pluck map pmap modify_in
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_extract str_detect str_replace str_split
#' @importFrom glue glue
#' @importFrom MplusAutomation mplusModeler SummaryTable

#' @export

getPoly <- function(
    lgm_object,
    wd = "Results") {
  # Validate Arguments -------------------------------------------------------------

  lgm_object <- lgm_object[[1]]

  stopifnot(class(lgm_object) == c("mplusObject", "list"))

  # Extract model information --------------------------------------------------
  ## Classes -------------------------------------------------------------------
  k <- lgm_object %>% purrr::pluck("results", "summaries", "NLatentClasses")

  ## Growth factors ------------------------------------------------------------
  gf <- lgm_object %>% purrr::pluck("MODEL") %>%
    stringr::str_extract("(?<=%OVERALL%\\n).*?(?= \\|)") %>%
    stringr::str_to_upper() %>%
    stringr::str_split(" ") %>%
    rep(k)

  gf_highest <- gf %>%
    map(~ last(.x))

  gf_idx <- gf[[1]] %>%
    length()

  ## Model parameters estimates ------------------------------------------------
  param_df <- .getParam(lgm_object)

  ## Growth factor p-values ----------------------------------------------------
  gf_pval <- .getPval(param_df, k, gf_highest)

  # Drop non-significant growth factors ----------------------------------------
  while (any(gf_pval > .05) & !gf_idx < 1) {
    gf0 <- gf_highest %>%
      purrr::map2(1:k,
               \(gf_highest, k) dplyr::filter(param_df, param == gf_highest &
                             LatentClass == k)) %>%
      purrr::map(~ dplyr::mutate(.x, gf0 = dplyr::if_else(pval > 0.05, paste0(param, "@0"), param))) %>%
      purrr::map(~ purrr::pluck(.x, "gf0"))

    ## Update growth factor by classes -----------------------------------------
    gf_k <- list(gf, gf_highest, gf0) %>%
      purrr::pmap(\(gf, gf_highest, gf0)
                  purrr::map_chr(gf, \(gf)
                   stringr::str_replace(gf, glue::glue(
                     "{gf_highest}$"
                   ), gf0)))

    gf <- gf_k

    message(glue("Class {str_which(gf0, '@0')}: the highest-order polynomial term was not significant (p > 0.05) and was removed {str_subset(gf0, '@0')}"))

    ## Update growth factor mean by classes ------------------------------------
    gf_mean <- gf_k %>%
      purrr::map_chr(~ glue::glue("[{paste(str_to_lower(.x), collapse = ' ')}]"))

    ## Update mplusObject ------------------------------------------------------
    ### Update title (K) -------------------------------------------------------
    #### Get the polynomial order by classes
    poly_nth <- gf_k %>%
      purrr::map(~ stringr::str_detect(.x, "@0", negate = TRUE)) %>%
      purrr::map(~ sum(.x) - 1) %>%
      stringr::str_c(collapse = "")

    lgm_object <- lgm_object %>%
      purrr::modify_in("TITLE",
                       \(title) stringr::str_replace(title, "(?<=P)\\d+", poly_nth))

    ### Update model (growth factor mean) --------------------------------------
    for (k in 1:k) {
      lgm_object <- lgm_object %>%
        purrr::modify_in("MODEL", \(model) stringr::str_replace(
          model,
          glue::glue("(?<=%C#{k}%\\n)(\\[.*\\];)?"),
          glue::glue("\n{gf_mean[[k]]};\n")
        ))
    }

    ### Update savedata (FILE) -------------------------------------------------
    lgm_object <- lgm_object %>%
      purrr::modify_in("SAVEDATA",
                       \(savedata) stringr::str_replace(savedata, "(?<=P)\\d+", poly_nth))

    ## Run model ---------------------------------------------------------------
    lgm_object <- runLGM(
      lgm_object = lgm_object,
      wd = wd)

    ## Update model information ------------------------------------------------
    ### Growth factor index ----------------------------------------------------
    gf_idx <- gf_idx - 1

    ### Highest-order growth factor --------------------------------------------
    gf_highest <- gf_k %>%
      map(\(gf_k) gf_k %>%
            purrr::discard(~ stringr::str_detect(.x, "@0")) %>%
            dplyr::last())

    ### Model parameters estimates ---------------------------------------------
    param_df <- .getParam(lgm_object)

    ### Growth factor p-values -------------------------------------------------
    gf_pval <- .getPval(param_df, k, gf_highest)

  }

  message("The highest-order polynomial term is significant (p < 0.05) across all classes.")
  return(lgm_object)
}

# Helper functions -------------------------------------------------------------
## getParam --------------------------------------------------------------------
.getParam <- function(model) {

  param_df <- model %>%
    purrr::pluck("results", "parameters", "unstandardized") %>%
    dplyr::filter(stringr::str_detect(string = paramHeader, pattern = "Means") &
             param %in% c("I", "S", "Q", "CUB")) %>%
    dplyr::select("LatentClass", "param", "pval")


  return(param_df)
}


## getPval ---------------------------------------------------------------------
.getPval <- function(df, k, gf) {

  pval_list <- gf %>%
    purrr::map2(1:k, \(gf, k)
    dplyr::filter(df, param == gf &
      LatentClass == k)) %>%
    purrr::map(~ purrr::pluck(.x, "pval"))

  return(pval_list)
}


# Declare global variables -----------------------------------------------------
utils::globalVariables(c(
  ".data",
  "Errors",
  "Title",
  "NLatentClasses",
  "param",
  "paramHeader",
  "LatentClass",
  "APPA_criterion",
  "Entropy_criterion",
  "Proportion_criterion"
))
