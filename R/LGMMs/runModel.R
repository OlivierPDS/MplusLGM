#' @title runModel
#' @description Given a set of model parameters, runs iterations of this model, 
#'     doubling the number of starting values until the loglikelihood value has 
#'     replicated at least twice within the model, and the best starting value is
#'     equal to that of the model with half the number of starts.
#' @param df A data frame containing all user variables and the ID variable
#' @param usevar A character vector containing variables to be used in Mplus
#'     for analysis
#' @param timepoints A vector containing the timepoints corresponding 
#'     to the elements in the usevar vector
#' @param idvar A character vector containing the ID variable in the data frame
#' @param classes A numeric value representing the number of classes in the model
#' @param overall_polynomial A numeric value representing the polynomial order for 
#'     the overall model (note that only linear, quadratic, and cubic models are 
#'     supported)
#' @param model_type A character vector representing the type of mixture model 
#'     to create (available options are "GBTM", "LCGA1", "LCGA2", and "LCGA3")
#' @param working_dir The directory where the results folder will be created
#' @param max_starts A numeric value representing the maximum level of starting values
#'     to use when attempting to replicate the loglikelihood before raising an 
#'     error
#' @param classes_polynomial An optional character vector to pass as input to 
#'     getMplusObject to specify the growth factors to estimate for each class
#' @return An MplusObject
#' @export
#' @import tidyverse
#' @import glue
#' @import MplusAutomation
runModel <- function(
  df, 
  usevar, 
  timepoints, 
  idvar, 
  classes, 
  overall_polynomial, 
  model_type, 
  working_dir = getwd(), 
  max_starts = 4000, 
  classes_polynomial = NULL
  ) {
  
  # Set initial number of starting values at 500
  starts <- 500
  
  # Initiate list to contain model results and a count of the models run
  list_log <- list()
  count_log <- 1
  
  # Attempt to replicate the LL until the maximum number of starts
  while (starts <= max_starts) {
    
    # Get MplusObject for specified model
    model <- getMplusObject(
      df, 
      usevar, 
      timepoints, 
      idvar, 
      classes, 
      starts, 
      overall_polynomial, 
      model_type, 
      classes_polynomial
      )
    
    # Create model directory to contain specified model if does not exist
    model_dir <- .createModelDirectory(
      working_dir, 
      model_type, 
      overall_polynomial, 
      classes
      )
    
    # Get name of the model
    model_name <-  stringr::str_remove(model[["TITLE"]], '\n')
    
    # Get shortened model name to be used as file name
    model_name_short <- stringr::str_split(model_name, '_')[[1]][-(1:2)]
    model_name_short <- glue::glue_collapse(model_name_short, sep = '_')
    
    print(glue::glue('Begin running model: {model_name}'))
    
    # Specify data file and model output file
    data_file <- glue::glue('{model_dir}/{model_name_short}.dat')
    model_file <- glue::glue('{model_dir}/{model_name_short}.inp')
    
    # Check line length for Mplus
    .checkLineLength(model)
    if (nchar(paste0('FILE = "', model_file, '";')) >= 90) {
      warning(paste0("Line ", 'FILE = "', model_file, '";', " may exceed 90 characters"))
    }
    
    # Run model and add it to the list of models
    tryCatch(
      capture.output(
        list_log[[count_log]] <- MplusAutomation::mplusModeler(
          model, 
          dataout = data_file, 
          modelout = model_file, 
          run = 1,
          writeData = 'always', 
          hashfilename = FALSE
        )
      ), error = function(e) {
        print(paste0("Data File: ", data_file))
        print(paste0("Model File: ", model_file))
        stop(paste('error:', e))
      }
    )
  
    print(glue::glue('Finished running model: {model_name}'))
    print(glue::glue('\n'))
    # If  more than one model in list (i.e., starts != 500, check if replicated)
    if (starts != 500) {
      
      # Read the output file and find separator sentence
      file <- readLines(glue::glue('{model_dir}/{model_name_short}.out'))
      line <- grep(
        "Final stage loglikelihood values at local maxima, seeds, and initial stage start numbers:", 
        file
        )

      # Get lines containing LL values and split into a vector of several strings
      vector1 <- stringr::str_split(file[line+2], " ")[[1]]
      vector2 <- stringr::str_split(file[line+3], " ")[[1]]
      
      # Get the LL values from these vectors
      LL1 <- vector1[12]
      LL2 <- vector2[12]
      
      # If the best LL has been replicated in this model, check across models
      if (LL1 == LL2) {
        
        # Extract LL from MplusObjects
        log_m1 <- MplusAutomation::SummaryTable(
          list_log[[count_log - 1]], 
          keepCols = 'LL', 
          sortBy = 'LL'
          )
        
        log_m2 <- MplusAutomation::SummaryTable(
          list_log[[count_log]], 
          keepCols = 'LL', 
          sortBy = 'LL'
          )
        
        # If these values are equal, return the last model run
        if (log_m1 == log_m2) {
          return(list_log[[count_log]])
        }
      }
      
    }
    
    # If LL is not replicated double starts and count += 1
    starts <- starts * 2
    count_log <- count_log + 1
    
  }
} 


#' Creates the specified directory if it does not exist.
.createModelDirectory <- function(
  working_dir, 
  model_type, 
  overall_polynomial,
  classes
  ) {
  
  results_dir <- glue::glue('{working_dir}/Results')
  model_dir <- glue::glue('{results_dir}/{model_type}/P={overall_polynomial}/K={classes}')
  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }
  
  return(model_dir)
}

#' Check line length
.checkLineLength <- function(m) {
  
  to_check <-c("TITLE", "DATA", "VARIABLE", "DEFINE", "MONTECARLO", "MODELPOPULATION", 
               "MODELMISSING",  "ANALYSIS", "MODEL", "MODELINDIRECT", "MODELCONSTRAINT", 
               "MODELTEST", "MODELPRIORS", "OUTPUT", "SAVEDATA", "PLOT")
  
  for (i in to_check) {
    line_code <- m[[i]]
    
    if (is.character(line_code)) {
      line_code <- strsplit(line_code, "\n")
      
      for (j in line_code[[1]]) {
        
        if (nchar(j) >= 90) {
          warning(paste0("Line ", j, " may exceed 90 characters"))
        }
        
      }
    }
  }
}
