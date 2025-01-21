#' @title Plot individual trajectories of outcome - Spaghetti plot

#' @description Generate a spaghetti plot to visualize the individual trajectories of a given outcome across time..

#' @param data A data frame containing all variables for the trajectory analysis.
#' @param outvar A character vector specifying the outcome variables at different times.

#' @return A ggplot object displaying the spaghetti plot of individual trajectories.

#' @export

#' @importFrom magrittr "%>%"
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom stringr str_split_1
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_minimal

#' @examples
#' \dontrun{
#' # Example usage:
#' plot <- getSpaghetti(
#'   data = df,
#'   ouvar = paste("sx", seq(from = 0, to = 36, by = 12), sep = "_")
#' print(plot)
#' }

# getSpaghetti function --------------------------------------------------------
getSpaghetti <- function(data, outvar) {
  ## Validate arguments --------------------------------------------------------
  stopifnot(
    is.data.frame(data),
    is.vector(outvar),
    is.character(outvar),
    outvar %in% names(data)
  )
  ## Reshape data to long form -------------------------------------------------
  data_long <- data %>%
    dplyr::select(tidyselect::all_of(outvar)) %>%
    tibble::rownames_to_column() %>%
    tidyr::drop_na() %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(outvar),
      names_to = c("time"),
      names_pattern = ".*_(\\d+)",
      values_to = "value") %>%
    dplyr::mutate(time = as.numeric(time))

  ## Plot observed individual values -------------------------------------------
  spaghetti_plot <- data_long %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = value, group = rowname)) +
    ggplot2::geom_point(alpha = .3) +
    ggplot2::geom_line(alpha = .3) +
    ggplot2::labs(x ="Time",
                  y = outvar %>%
                    dplyr::first() %>%
                    stringr::str_split_1("_") %>%
                    dplyr::first()) +
    ggplot2::theme_minimal()

  return(spaghetti_plot)
}

# Declare global variables ------------------------------------
utils::globalVariables(c("time", "value", "rowname"))
