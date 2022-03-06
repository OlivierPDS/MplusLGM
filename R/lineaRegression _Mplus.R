library(MplusAutomation)

usevar <- c('K_SAPS', 'FIQ')

SAPNS_df <- rename(SAPNS_df, c('CP1SAPS' = 'CP1_SAPS'))

#Regression on categorical variable 
# binary var: ref cat = 1st cat
# multinomial var: ref cat = last cat

model <- mplusObject(
  TITLE = 'Linear regression;',
  MODEL = 'K_SAPS ON FIQ; FIQ;', #need pred>1 or cite pred in model to use available information
  OUTPUT = c('sampstat standardized tech1;'),
  ANALYSIS = 'ESTIMATOR = MLR;',
  usevariables = usevar,
  rdata = SAPScov_df
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

SAPScov_df <-  SAPScov_df %>% mutate(K_SAPS=factor(K_SAPS))
mplusGLM <-  mplusGLM(K_SAPS ~ FIQ, SAPScov_df, 'pin')
# = #
MplusAutomation:::.mplusMultinomial(
  'K_SAPS',
  'FIQ',
  SAPScov_df,
  idvar = 'pin',
  integration = 1000,
  processors = 16,
  OR = TRUE,
  pairwise = TRUE,
)


