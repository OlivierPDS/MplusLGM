#' @title getLongMeans
#' @description Wrapper around `pivot_longer` which calculate the mean for all
#'     user variables, then shifts them to long format
#' @param df A data frame containing all user variables
#' @param usevar A character vector containing variable names
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector
#' @param group_var An optional grouping variable (e.g., class)
#' @return A data frame
#' @import tidyverse
#' @export
getLongMeans <- function(
  df, 
  usevar, 
  timepoints, 
  group_var = NULL
  ) {

  if (is.null(group_var)) {
    
    # Get mean scores at all timepoints
    usevar_means <- df %>%
      dplyr::summarise_at(vars(usevar), mean, na.rm = TRUE)
    
    # Rename variables as timepoints
    colnames(usevar_means) <- timepoints
    
  } else {
    
    # Get mean scores at all timepoints
    usevar_means <- df %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::summarise_at(vars(usevar), mean, na.rm = TRUE)
    
    # Rename variables as timepoints
    colnames(usevar_means) <- c(group_var, timepoints)
    
  }

  
  # Convert to long form
  usevar_means_long  <- usevar_means %>% 
    tidyr::pivot_longer(
      cols = dplyr::all_of(as.character(timepoints)), 
      names_to = "Time", 
      values_to = "Variable") %>%
    mutate(Time = factor(as.numeric(Time)))
  usevar_means_long$Time <- as.numeric(levels(usevar_means_long$Time))[usevar_means_long$Time]
  
  return(usevar_means_long)
  
}