savedata <- BEST_model[["results"]][["savedata"]] %>%  
  as.data.frame() %>% 
  merge(
    y = select(SOFAS_df, c('pin', 'ageentry')),
    by.x = str_to_upper('pin'), #vars name are always uppercase in Mplus data output
    by.y = 'pin',
    all.x = TRUE) %>%
  dplyr::rename(N = C)

GF_mpobj <- mplusObject(
  TITLE = 'R3STEP_ageentry;',
  VARIABLE = c(
    'USEVAR = SOFAS_0 SOFAS_12 SOFAS_24 N ageentry;',
    'NOMINAL= N;', 
    'CLASSES = c(2);'),
  
  MODEL =
  "%OVERALL%
    i s q | SOFAS_0@0 SOFAS_12@12 SOFAS_24@24;
    SOFAS_0-SOFAS_24 (1);
    C on ageentry; ageentry;
    I S on  ageentry; ageentry;
  
  %c#1%
  [N#1@2.378];
  i s q;
  
  %c#2%
  [N#1@-1.949];
  i s q;",
  
  ANALYSIS = c(
    'TYPE = MIXTURE;',
    'ALGORITHM = INTEGRATION;',
    'INTEGRATION = MONTECARLO;',
    'STARTS = 0;',
    'PROCESSORS = 8;'),
  
  OUTPUT = 'TECH1 SAMPSTAT STANDARDIZED;',
  autov = FALSE, 
  rdata = savedata 
)

### Create, run, and read Mplus models 
GF_model <- mplusModeler(
  object = GF_mpobj,
  dataout = str_c(getwd(),'/SOFAS/Results/ageentry.dat'),
  modelout = str_c(getwd(),'/SOFAS/Results/ageentry.inp'),
  hashfilename = FALSE,
  run = 0,
  check=FALSE,
  writeData ="always"
)
