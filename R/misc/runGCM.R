#' @title runGCM
#' @description Run a Growth Curve Model
#' @param df A data frame containing all user variables and the ID variable
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector
#' @param working_dir The directory where the results folder will be created
#' @return An MplusObject
#' @import MplusAutomation
#' @import tidyverse
#' @import glue
#' @export
runGCM <- function(
  df,
  usevar,
  timepoints,
  working_dir = getwd()
) {
  
  # Input validation
  stopifnot(
    is.data.frame(df), 
    is.character(usevar), 
    is.vector(timepoints),
    length(usevar) == length(timepoints)
  )
  
  # Generate list of user variables @ timepoints
  vars_timepoints <- c()
  for (i in 1:length(usevar)) {
    vars_timepoints <- c(vars_timepoints, glue::glue('{usevar[[i]]}@{timepoints[[i]]}'))
  }
  
  # Split into maximum 60 chars per line
  vars_timepoints <- .splitLength(vars_timepoints)
  vars_timepoints <- glue::glue('{glue::glue_collapse(vars_timepoints, sep = " ")};')
  
  # Create GCM MplusObject
  model <- MplusAutomation::mplusObject(
    TITLE = 'Growth Curve Model',
    MODEL = 
      c('i s |', 
        vars_timepoints),
    OUTPUT = 
      c('sampstat standardized tech1;'),
    PLOT = .getPlot(usevar),
    rdata = subset(df, select = usevar)
  )
  
  # Create model directory
  model_dir <- glue::glue('{working_dir}/Results/GCM')
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  # Run the model
  gcm_results <- MplusAutomation::mplusModeler(
    object = model, 
    dataout = glue::glue('{model_dir}/mplus.dat'), 
    modelout = glue::glue('{model_dir}/GCM.inp'),
    run = 1,
    writeData = 'always', 
    hashfilename = FALSE
  )
  
  return(gcm_results)
  
}