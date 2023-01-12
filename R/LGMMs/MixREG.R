MixREG <- function(df,
                   idvar,
                   usevar,
                   cov,
                   model){
  
  savedata <- list()
  MixREG_mpobj <-  list()
  MixREG_fit <- list()
  
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
      
      MixREG_mpobj[[i]] <- MplusAutomation::mplusObject(
        TITLE = 
          glue::glue("
  MixREG_{i};"),
        
        VARIABLE =
          glue::glue("
  USEVAR = {i} N S;
  NOMINAL = N;
  CLASSES = c({k});"),
        
        ANALYSIS = "
  TYPE = MIXTURE;
  STARTS = 1000;
  PROCESSORS = 8;",
        
        MODEL = (
          if (k == 2) {
            glue::glue("
  %OVERALL%
  S ON {i}; 
  {i};
  
  %C#1%
  [N#1@{logits[1,1]}];
  S ON {i};
  {i}; 

  %C#2%
  [N#1@{logits[2,1]}];
  S ON {i};
  {i};")
          } 
          else if (k == 3) {
            glue::glue("
  %OVERALL%
  S ON {i}; {i};

  %C#1%
  [N#1@{logits[1,1]}];
  [N#2@{logits[1,2]}];
  S ON {i}; {i};

  %C#2%
  [N#1@{logits[2,1]}];
  [N#2@{logits[2,2]}];
  S ON {i}; {i};

  %C#3%
  [N#1@{logits[3,1]}];
  [N#2@{logits[3,2]}];
  S ON {i}; {i};")
          } 
          else {
            stop('Error: Does not currently support model with more than 3 classes')
            }),
        usevariables = colnames(savedata[[i]]),
        rdata = savedata[[i]]
      )
    }
  
  # Create directory for results if does not exist
  path <-glue::glue(getwd(), '{usevar}', 'Results', 'MixREG', .sep = "/")
  if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
    
  
      for (i in cov) {
        MixREG_fit[[i]] <- MplusAutomation::mplusModeler(
          object = MixREG_mpobj[[i]],
          dataout = glue::glue(path, '/MixREG_{i}.dat'),
          modelout = glue::glue(path, '/MixREG_{i}.inp'),
          hashfilename = FALSE,
          run = 1,
          check = TRUE,
          varwarnings = TRUE,
          writeData = 'always')
        }
      

return(MixREG_fit)
  
}
