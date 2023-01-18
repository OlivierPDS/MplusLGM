D3STEPm <- function(df,
                   idvar,
                   usevar,
                   cov,
                   model){
  
  savedata <- list()
  D3STEP_mpobj <-  list()
  D3STEP_fit <- list()
  
  # Extract logits and model parameters
  logits <- model[["results"]][["class_counts"]][["logitProbs.mostLikely"]] %>% as.data.frame()
  k <- model[["results"]][["summaries"]][["NLatentClasses"]]
  
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
    
    D3STEP_mpobj[[i]] <- MplusAutomation::mplusObject(
  
  TITLE = 
        glue::glue("
  D3STEP_{i};
  "),
  
  VARIABLE = (
    if(is.factor(df[[i]])) { 
    glue::glue("
  USEVAR = {i} N;
  NOMINAL = N;
  CATEGORICAL = {i};
  CLASSES = c({k});
  ")
    }
  else {
    glue::glue("
  USEVAR = {i} N;
  NOMINAL = N;
  CLASSES = c({k});
  ")
  }),
  
  ANALYSIS = "
  TYPE = MIXTURE;
  STARTS = 0;
  PROCESSORS = 8;
  ",
  
  
  MODEL = (
    if (k == 2 & is.factor(df[[i]])) {
      glue::glue("
  %OVERALL%
  [{i}$1];
  
  %C#1%
  [N#1@{logits[1,1]}];
  [{i}$1](M1);

  %C#2%
  [N#1@{logits[2,1]}];
  [{i}$1](M2);
  ")
    }
  else if (k == 2 & !is.factor(df[[i]])) {
    glue::glue("
  %OVERALL%
  {i};
  
  %C#1%
  [N#1@{logits[1,1]}];
  [{i}](M1); {i};

  %C#2%
  [N#1@{logits[2,1]}];
  [{i}](M2); {i};
  ")
  } 
  else if (k == 3 & is.factor(df[[i]])) {
    glue::glue("
  %OVERALL%
  [{i}$1];
  
  %C#1%
  [N#1@{logits[1,1]}];
  [N#2@{logits[1,2]}];
  [{i}$1](M1);

  %C#2%
  [N#1@{logits[2,1]}];
  [N#2@{logits[2,2]}];
  [{i}$1](M2);

  %C#3%
  [N#1@{logits[3,1]}];
  [N#2@{logits[3,2]}];
  [{i}$1](M3);")
  } 
  else if (k == 3 & !is.factor(df[[i]])) {
    glue::glue("
  %OVERALL%
  {i};
  
  %C#1%
  [N#1@{logits[1,1]}];
  [N#2@{logits[1,2]}];
  [{i}](M1); {i};

  %C#2%
  [N#1@{logits[2,1]}];
  [N#2@{logits[2,2]}];
  [{i}](M2); {i};

  %C#3%
  [N#1@{logits[3,1]}];
  [N#2@{logits[3,2]}];
  [{i}](M3); {i};")
  } 
  else {
    stop('Error: Does not currently support model with more than 3 classes')
  }),
  
  MODELTEST = (
    if (k == 2) {
      glue::glue("M1 = M2;")
    } 
  else if (k == 3) {
    glue::glue("M1 = M2; M1 = M3;")
  } 
  else {
    stop('Error: Does not currently support model with more than 3 classes')
  }),
  
  MODELCONSTRAINT = (
    if (k == 2) {
      glue::glue("New (diff12); diff12 = M1 - M2;")
    } 
    else if (k == 3) {
      glue::glue("New (diff12 diff13); 
                  diff12 = M1 - M2;
                  diff13 = M1 - M3;
                  ")
    } 
    else {
      stop('Error: Does not currently support model with more than 3 classes')
    }),
  
  usevariables = colnames(savedata[[i]]),
  rdata = savedata[[i]]
    )
  }
  
  # Create directory for results if does not exist
  path <-glue::glue(getwd(), '{usevar}', 'Results', 'D3STEP', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
  
  
  for (i in cov) {
    D3STEP_fit[[i]] <- MplusAutomation::mplusModeler(
      object = D3STEP_mpobj[[i]],
      dataout = glue::glue(path, '/D3STEP_{i}.dat'),
      modelout = glue::glue(path, '/D3STEP_{i}.inp'),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = 'always')
  }
  
  
  return(D3STEP_fit)
  
}
