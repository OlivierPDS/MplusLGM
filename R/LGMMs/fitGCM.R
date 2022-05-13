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
fitGCM <- function(df,
                   usevar,
                   timepoints,
                   working_dir = getwd()) {
  
  outcome <- stringr::str_replace_all(usevar[1], "[^[:alpha:]]", "")
  
  # Input validation
  stopifnot(
    is.data.frame(df),
    is.character(usevar),
    is.vector(timepoints),
    length(usevar) == length(timepoints)
  )
  
  # Generate list of user variables @ timepoints
  vars_timepoints <- stringr::str_c(usevar, timepoints, sep = '@') %>% 
    .splitLength() %>% # Split into maximum 60 chars per line - Josh function
    glue::glue_collapse (sep = " ")
  
  
  # Create GCM MplusObject
  GCM_mpobj <- MplusAutomation::mplusObject(
    TITLE = glue::glue('GCM_{outcome};'),
    MODEL = glue::glue('i s | {vars_timepoints};'),
    OUTPUT = 'TECH1 SAMPSTAT STANDARDIZED;',
    SAVEDATA = glue::glue('FILE = GCM_{outcome}_res;'),
    PLOT = .getPlot(usevar),
    autov = FALSE, 
    usevariables = names(select(df, usevar)),
    rdata =  df
  )
  
  # Create directory for results if does not already exist
  model_dir <- glue::glue('{working_dir}', '{outcome}', 'Results', 'GCM', .sep = "/")
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  # Run the model
  GCM_model <- MplusAutomation::mplusModeler(
    object = GCM_mpobj,
    dataout = glue::glue('{model_dir}/GCM_{outcome}.dat'),
    modelout = glue::glue('{model_dir}/GCM_{outcome}.inp'),
    run = 1,
    check = TRUE,
    writeData = 'always',
    hashfilename = FALSE
  )
  
  return(GCM_model)
  
}
