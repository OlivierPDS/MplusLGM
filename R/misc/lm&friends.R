

setnames(SAPNS_df, old = c('dx_spect', 'SAPS_0', 'SANS_0'), new = c('y', 'x1', 'x2'))
SAPScov_df <-  SAPScov_df %>% mutate(K_SAPS=factor(K_SAPS))

# mutate(sex = relevel(sex, ref = "Male")) # Change reference category
# library(lm.beta) # Add standardized regression coefficients to lm-objects - lm_object %>% lm.beta()
# library(confint) # Compute confidence interval - lm_object %>% confint() (to use with scale(x), scale(y))
# other summary function - lm_object %>% sum()  


library(MplusAutomation)
#Regression on categorical variable 
#   - binary var: ref cat = 1st cat
#   - multinomial var: ref cat = last cat

model <- mplusObject(
  TITLE = 'Linear regression;',
  MODEL = 'K_SAPS ON FIQ; FIQ;', #need pred/x > 1 or include pred/x in model to use all available information
  OUTPUT = c('sampstat standardized tech1;'),
  ANALYSIS = 'ESTIMATOR = MLR;',
  usevariables = colnames(SAPScov_df),
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

mplusGLM(K_SAPS ~ FIQ, SAPScov_df, 'pin') # only supports categorical/nominal outcome/y
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


