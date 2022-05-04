### Manual 3RSTEP procedure
R3STEP_manual <- function(df,
                          idvar,
                          usevar,
                          cov,
                          model) {
  
  # Extract logits and model parameters
  logits <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] %>% as.data.frame()
  k <- model[["results"]][["input"]][["variable"]][["classes"]] %>% readr::parse_number()
  
  savedata <- list()
  step3 <- list()
  step3_fit <- list()
  
  for (i in cov) {
  
  savedata[[i]] <- model[["results"]][["savedata"]] %>%
    as.data.frame() %>%
    merge(
      y = select(df, c(idvar, i)),
      by.x = str_to_upper(idvar), #vars name are always uppercase in Mplus data output
      by.y = idvar,
      all.x = TRUE) %>%
    dplyr::rename(N = C) #Categorical latent variables (C) cannot have the same name as observed variables.
   
  step3[[i]] <- MplusAutomation::mplusObject(
    TITLE = 
      glue::glue("
  R3STEP_manual_{i};"),
    
    VARIABLE =
      glue::glue("
  USEVAR = {i} N;
  NOMINAL= N;
  CLASSES = c({k});"),
    
    ANALYSIS = "
  TYPE = MIXTURE;
  ALGORITHM=INTEGRATION;
  INTEGRATION=MONTECARLO;
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
      } else {stop('Error: Does not currently support model with more than 3 classes')}
      ),
    
    usevariables = colnames(savedata[[i]]),
    
    rdata = savedata[[i]]
  )
  
  path <-glue::glue(getwd(), '{usevar}', 'Results', 'R3STEP', .sep = "/")
 
   if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  step3_fit[[i]] <- MplusAutomation::mplusModeler(
    object = step3[[i]],
    dataout = glue::glue(path, '/R3STEPm_{i}.dat'),
    modelout = glue::glue(path, '/R3STEPm_{i}.inp'),
    hashfilename = FALSE,
    run = 1,
    check = TRUE,
    varwarnings = TRUE,
    writeData = 'always'
  )
  }
  return(step3_fit)
}


### To do
  # expand function to more than 3-class models