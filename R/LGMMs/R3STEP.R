R3STEP <- function(
  df, 
  model, 
  cov
) {
  
  # Read the output file and find separator sentence
  output <- model[["results"]][["output"]]
  line <- grep("Final stage loglikelihood values at local maxima, seeds, and initial stage start numbers:", output)
  
  # Get seed value for best LL value
  vector <- stringr::str_split(output[line+2], " ")[[1]] 
  seed <- vector[14] 
  
  # Get VARIABLE
  var1 <- word(model[["results"]][["input"]][["variable"]][["usevar"]], 1)
  var2 <- word(model[["results"]][["input"]][["variable"]][["usevar"]], -1)
  class <- model[["results"]][["input"]][["variable"]][["classes"]]
  
  COV_model <- update(
    model,
    TITLE = as.formula(glue("~ 'R3STEP - {cov};'")),
    VARIABLE = as.formula(glue("~ '
USEVAR = {var1}-{var2} {cov};
AUXILIARY = (R3STEP) {cov};
CLASSES = {class};
                               '")),
    ANALYSIS = as.formula(glue("~ '
TYPE = MIXTURE;
STARTS = 0;
OPTSEED = {seed};
PROCESSORS = 8;
                               '")),
    OUTPUT = ~ '
SAMPSTAT
STANDARDIZED
TECH1;',
    usevariables = names(df),
    rdata = df
  )
  
  
  COV_res <- mplusModeler(
    object = COV_model,
    dataout = glue(getwd(),'/SAPS/Results/R3STEP/R3STEP_{cov}.dat'), #to change SAPS
    modelout = glue(getwd(),'/SAPS/Results/R3STEP/R3STEP_{cov}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData = 'always'
  )
  return(COV_res)
}