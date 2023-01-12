D3STEP <- function(df,
                   idvar,
                   usevar,
                   cov,
                   model){
  
  savedata <- list()
  D3STEP_mpobj <-  list()
  D3STEP_fit <- list()
  
  # Read the output file and find separator sentence
  output <- model[["results"]][["output"]]
  line <- grep("Final stage loglikelihood values at local maxima, seeds, and initial stage start numbers:", output)
  
  # Get seed value for best LL value
  vector <- stringr::str_split(output[line+2], " ")[[1]] 
  seed <- vector[14] 
  
  # Get VARIABLE
  var1 <- word(model[["results"]][["input"]][["variable"]][["usevar"]], 1)
  var2 <- word(model[["results"]][["input"]][["variable"]][["usevar"]], -1)
  k <- model[["results"]][["input"]][["variable"]][["classes"]] %>% readr::parse_number()
  
  # Create MplusObject for all var in cov
  for (i in cov) {
    
    D3STEP_mpobj[[i]] <- mplusObject(
      
      TITLE = glue('D3STEP_{i};'),
      
      VARIABLE = (
        if (is.factor(df[[i]])) {
          glue::glue("
USEVAR = {var1}-{var2} {i};
AUXILIARY = {i}(DCAT);
CLASSES = c({k});")
        } else { 
          glue::glue("
USEVAR = {var1}-{var2} {i};
AUXILIARY = {i}(DCON);
CLASSES = c({k});")
        }),

ANALYSIS = glue('
TYPE = MIXTURE;
STARTS = 0;
OPTSEED = {seed};
PROCESSORS = 8;'),

MODEL = model[["MODEL"]],

OUTPUT = '
SAMPSTAT
STANDARDIZED
TECH1;',
usevariables = names(df),
rdata = df)}
  
  
  
  # Create directory for results if does not exist
  path <-glue::glue(getwd(), '{usevar}', 'Results', 'D3STEP', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
  
  
  for (i in cov) {
    D3STEP_fit[[i]] <- mplusModeler(
      object = D3STEP_mpobj[[i]],
      dataout = glue::glue(path, '/D3STEP_{i}.dat'), #to change SAPS
      modelout = glue::glue(path, '/D3STEP_{i}.inp'),
      hashfilename = FALSE,
      run = 1,
      check=TRUE,
      varwarnings = TRUE,
      writeData = 'always')
  }
  
  
  return(D3STEP_fit)
}
