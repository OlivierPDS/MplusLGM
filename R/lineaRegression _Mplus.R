library(MplusAutomation)

usevar <- c('K_SAPS', 'FIQ')

SAPNS_df <- rename(SAPNS_df, c('CP1SAPS' = 'CP1_SAPS'))

#Regression on categorical variable 
# binary var: ref cat = 1st cat
# multinomial var: ref cat = last cat

model <- mplusObject( #To replace with mplusGLM()
  TITLE = 'Linear regression;',
  MODEL = 'FIQ ON K_SAPS;',
  OUTPUT = c('sampstat standardized tech1;'),
  ANALYSIS = 'ESTIMATOR = MLR;',
  usevariables = usevar,
  rdata = SAPNS_df
)

results <- mplusModeler(
  object = model,
  dataout = str_c(getwd(),'/MplusfromR/lineaR.dat'),
  modelout = str_c(getwd(),'/MplusfromR/lineaR.inp'),
  hashfilename = FALSE,
  run = 1,
  check=TRUE,
  varwarnings = TRUE,
  writeData ="always"
)



