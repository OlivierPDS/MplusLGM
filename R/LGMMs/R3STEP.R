R3STEP <- function(df,
                   idvar,
                   usevar,
                   cov,
                   model,
                   method = c('auxiliary', 'manual'))
                   {
  
  savedata <- list()
  R3STEP_mpobj <-  list()
  R3STEP_fit <- list()
  
  if (method == 'auxiliary') {
    
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
    
    # Create MplusObject for all var in cov
    for (i in cov) {
      
      R3STEP_mpobj[[i]] <- update(
        model,
        
        TITLE = as.formula(glue("~ '
R3STEPa_{i};'")),

VARIABLE = as.formula(glue("~ '
USEVAR = {var1}-{var2} {i};
AUXILIARY = (R3STEP) {i};
CLASSES = {class};'")),

ANALYSIS = 
  as.formula(glue("~ '
TYPE = MIXTURE;
STARTS = 0;
OPTSEED = {seed};
PROCESSORS = 8;'")),

OUTPUT = ~ '
SAMPSTAT
STANDARDIZED
TECH1;',

usevariables = names(df),
rdata = df
      )
      
      R3STEP_mpobj[[i]][["PLOT"]] <- NULL
      R3STEP_mpobj[[i]][["SAVEDATA"]] <- NULL
    }
  } else if (method == 'manual') {
    
    # Extract logits and model parameters
    logits <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] %>% as.data.frame()
    k <- model[["results"]][["input"]][["variable"]][["classes"]] %>% readr::parse_number()
    
    # Create MplusObject for all var in cov 
    for (i in cov) {
      
      #Create df including C and cov
      savedata[[i]] <- model[["results"]][["savedata"]] %>%  
        as.data.frame() %>% 
        select(-starts_with(usevar)) %>% # bug when cov %in% savedata 
        merge(
          y = select(df, c(idvar, i)),
          by.x = str_to_upper(idvar), #vars name are always uppercase in Mplus data output
          by.y = idvar,
          all.x = TRUE) %>%
        dplyr::rename(N = C) #Categorical latent variables (C) cannot have the same name as observed variables.
      
      R3STEP_mpobj[[i]] <- MplusAutomation::mplusObject(
        TITLE = 
          glue::glue("
  R3STEPm_{i};"),
  
  VARIABLE =
    glue::glue("
  USEVAR = {i} N;
  NOMINAL= N;
  CLASSES = c({k});"),
  
  ANALYSIS = "
  TYPE = MIXTURE;
  ALGORITHM = INTEGRATION;
  INTEGRATION = MONTECARLO;
  STARTS = 0;
  PROCESSORS = 8;",
  
  MODEL = (
    if (k == 2) {
      glue::glue("
  %OVERALL%
  C on {i}; {i};

  %C#1%
  [N#1@{logits[1,1]}];

  %C#2%
  [N#1@{logits[2,1]}];")
      
    } else if (k == 3) {
      
    glue::glue("
  %OVERALL%
  C on {i}; {i};

  %C#1%
  [N#1@{logits[1,1]}];
  [N#2@{logits[1,2]}];

  %C#2%
  [N#1@{logits[2,1]}];
  [N#2@{logits[2,2]}];

  %C#3%
  [N#1@{logits[3,1]}];
  [N#2@{logits[3,2]}];")
      
  } else {
    
    stop('Error: Does not currently support model with more than 3 classes')
    
  }
  ),
  
  usevariables = colnames(savedata[[i]]),
  rdata = savedata[[i]]
      )
    }
  }
  
  # Create directory for results if does not exist
  path <-glue::glue(getwd(), '{usevar}', 'Results', 'R3STEP', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
    
  if (method == 'auxiliary') {
    for (i in cov) {
      R3STEP_fit[[i]] <- mplusModeler(
        object = R3STEP_mpobj[[i]],
        dataout = glue::glue(path, '/R3STEPa_{i}.dat'), #to change SAPS
        modelout = glue::glue(path, '/R3STEPa_{i}.inp'),
        hashfilename = FALSE,
        run = 1,
        check=TRUE,
        varwarnings = TRUE,
        writeData = 'always')
    }
  } else if (method == 'manual') {
    for (i in cov) {
      R3STEP_fit[[i]] <- MplusAutomation::mplusModeler(
        object = R3STEP_mpobj[[i]],
        dataout = glue::glue(path, '/R3STEPm_{i}.dat'),
        modelout = glue::glue(path, '/R3STEPm_{i}.inp'),
        hashfilename = FALSE,
        run = 1,
        check = TRUE,
        varwarnings = TRUE,
        writeData = 'always')
    }
  }

return(R3STEP_fit)
  
}
