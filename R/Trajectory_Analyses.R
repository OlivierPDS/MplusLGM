# To do -------------------------------------------------------------------
## improve SelectBestModel() by adding some conditions (class count < 5%, lowest k, warnings/errors)
## wrap in a function GetFitIndices() & mutate (BF10)
## wrap together fit functions (R3STEP, MixREG, D3STEP)
## improve GMM functions to replicate best LL (using map functions)
## runModels(): do not always overwright models
## remove GBTM models from LCGA models list

# Instructions ------------------------------------------------------------
## Some Mplus Language 
### vars: refers to variances and residual variances; ex: var1 var1-var9;
### [vars]: refers to means, intercepts, thresholds; ex: [var1, var1-var9];
### *: frees a parameter at a default value or a specific starting value; ex:  var1* var2*.5;
### @: fixes a parameter at a default value or a specific value; ex: var1@ var2@0;
### (number): constrains parameters (residual variance) to be equal var1(1) var2(1)
### with: refers to covariance; ex: var1 with var2@0;

## Examine fit indices 
### N > 176:  CAIC > BIC > aBIC > AIC
### entropy > 0.8: CAIC & BIC > aBIC & BLRT  
### class count should not be < 5% sample size
### BF10 = exp((BIC_H0 - BIC_H1)/2) > 10 = strong evidence in favour of the alternative model (k1)
### H0:K=K1-1 H1:K=K1 If likelihood ratio p<0.05 then choose K1, else choose K1-1
### APPA: Values closer to 1 indicate a good fit (> 0.7 for all classes)
### SK test: Values < ± 2.00 are generally accepted as a normal distribution or pval > 0.05

## Covariates/distal outcomes 
### simple/univariate regression then multiple/multivariate regressions
### missing on pred/Xs: manual R3STEP
### MI: has to be 2 separate steps/inp. files (AUXILIARY = vars excluded from IMPUTE = data)
### can use the manual R3step, including var of the covariates (instead of C on X; use C on X; X; )
### (see section 3;  http://statmodel.com/download/webnotes/webnote15.pdf)

# Load packages & source functions ----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(glue)
library(haven)
library(lubridate) 
library(labelled)
library(data.table)
library(metan)
library(MplusAutomation)  
# library(usethis) library(devtools) install_github("michaelhallquist/MplusAutomation")
library(MplusLGM)
library(rhdf5) #BiocManager::install('rhdf5')
library(parallel)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/LGM/R/LGMMs', modifiedOnly= FALSE, envir=globalenv())

# Installation ------------------------------------------------------------
install.packages('devtools') # Install devtools 
devtools::install_github('joshunrau/MplusLGM') # Install MplusLGM 

# Vars of interest --------------------------------------------------------
## Sociodemographics
SD_num <- c('ageentry', 'FIQ', 'holltotp', 'EDUC_num')
SD_cat <- c('gender', 'Vmin', 'NEET', 'INDPT', 'REL')
SD <- c('DOB', SD_num, SD_cat)

## Clinical information
CHRONO <- c('date_pr','dt2stpsy', 'onset', 'datecont', 'doe', 'prevTX_dur', 'prevAP_dur')
CLIN_num <- c('ageonset', 'duponset', 'dui', 'conv')
CLIN_cat <-c('prev_EP', 'evercont', 'ever_ap', 'txiscm', 'mode', 'CAYRconv', 'ip_op', 'referral', 'Txsitn', 'transfR') #'inRCT', 'RCTgroup'
CLIN <- c(CHRONO, CLIN_num, CLIN_cat)

## Diagnosis
DXII <- paste0('secdx', c(1, 3:6))
DXIIpc <- paste0('secdx', c(1, 3:6), 'pc')
DX <- c('dx_0', 'dx_b2', 'dx_1year', 'dx2', DXII, DXIIpc, 'SUD')

## Cognition
COGSTATE <- c('verbm_z', 'wm_z', 'ef_z', 'sop_z', 'vism_z', 'va_z', 'sc_z')
COG <- c('Battery', COGSTATE)

## Functioning
PAS <- c('PAS_c', 'PAS_ea', 'PAS_la', 'PAS_a', 'PAS_tot2', 'PAS_tot3', 'PAS_tot4')
FUNC_cat <- c('vismin', 'EDUC_cat', 'newwork', 'othwork', 'newliving', 'othliving', 'marital')
EDUC <- paste('EDUC', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
WRK1 <- paste('WRK1', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
HOUS1 <- paste('HOUS1', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
HOUS2 <- paste('HOUS2', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SOFAS <- paste('SOFAS', c(0, 12, 24), sep = '_')
FUNC <- c(PAS, FUNC_cat, 'Years_of_education', EDUC, WRK1, HOUS1, HOUS2, SOFAS)

## Psychopathology
SAPS <- paste('SAPS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SANS <- paste('SANS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
HAS <- paste('HAS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
CDS <- paste('CDS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
YMRS <- paste('YMRS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')

## Insight 
SUMD1 <- paste('SUMD1', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SUMD2 <- paste('SUMD2', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SUMD3 <- paste('SUMD3', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SUMD <- c(SUMD1, SUMD2, SUMD3)

## Suicide
SCD <- paste('cd8', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')

## Remission & Recovery
PSR <- paste('PSR', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
NSR <- paste('NSR', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
JSR <- paste('JSR', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SR_0 <- paste(c('PSR', 'NSR'), '0', sep = '_')
SR_24 <- paste(c('PSR', 'NSR', 'JSR'), '24', sep = '_')
SR_t <- paste(c('PSR', 'NSR', 'JSR'), 't', sep = '_')
SR_1st <- paste(c('PSR', 'NSR', 'JSR'), '1st', sep = '_')
SR_cat <- c(PSR, NSR, JSR, 'JSR_by3', 'RECOV_24')
SR <- c(SR_cat, SR_t, SR_1st, 'JSR_24C')

## Medication
adh <- paste('comp', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
ord <- paste0('txm', c(0, 1, 2, 3, 6, 9, 12, 18, 24), 'co')
CPZ <- paste('CPZ', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
CPZw <- paste('CPZw', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
RX_0 <- c('comp_0', 'txm0co', 'CPZw_0')
RX_24 <- c('comp_24', 'txm24co', 'CPZw_24')
RX <-  c(ord, adh, CPZ)
# AP_yn <- starts_with('anti')
# AP_type <- starts_with('atype')

## Trajectory 
# C <- c('C_SOFAS', 'C_SAPS', 'C_SANS')
# CP <- c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')

## Summary
SX_0 <- paste(c('SAPS', 'SANS', 'HAS', 'CDS', 'cd8', 'YMRS','SUMD1'), '0', sep = '_')
SX_24 <- paste(c('SAPS', 'SANS', 'HAS', 'CDS', 'cd8', 'YMRS','SUMD1'), '24', sep = '_')
SX <- c(SAPS, SANS, HAS, CDS, SCD, YMRS, SUMD)

## covariates
cov_R3STEP <- c(SD_cat, SD_num, 'ageonset', 'duponset', 'mode', 'ip_op', 'dx_b2', 'SUD', 'PAS_tot2', 'SOFAS_0', COGSTATE, SX_0, SR_0, SR_1st, 'JSR_by3', "comp_0", "CPZ_0") %>% setdiff(c('cd8_0')) #CAYRconv, txm0co, SUMD2_0, SUMD3_0
cov_MixReg <- c(SOFAS, SX, PSR, NSR, JSR)
cov_D3STEP <- c('SOFAS_24', SX_24, SR_24, 'JSR_24C','RECOV_24', "comp_24", "CPZ_24", "txm24co") %>% setdiff(c('cd8_24')) #txm24co

## Miscellaneous
# t <- paste0('t', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
# items <- sap1_0:umd_6b_24

## Categorical variables to recode as factors
cat <- c(SD_cat, WRK1, HOUS1, HOUS2, CLIN_cat, FUNC_cat, DX, SR_cat, ord)

# Load dataset ------------------------------------------------------------
PEPP2_df <- list.files(
    '/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/GitHub/PEPP_private/PEPP2/Data/SAV',
    full.names = TRUE) %>%
  file.info() %>%
  filter(isdir == FALSE) %>% 
  slice_max(mtime) %>% # get the most updated file
  rownames() %>% 
  read_sav()

SD_df <- PEPP2_df %>% 
  # filter(selected == 1) %>% 
  filter(pin <= 857) %>% 
  select('pin', all_of(c(cov_R3STEP, cov_MixReg, cov_D3STEP)), starts_with('miss')) %>% 
  mutate(across(any_of(cat), ~ to_factor(.x, levels = 'labels'))) %>% #Recode labelled variables as factor 
  mutate(across(c(-any_of(cat), -where(is.Date)), ~ as.numeric(as.character(.)))) %>% #Recode labelled variables as numeric
  mutate(gender = fct_collapse(gender, male = 'Male', female = c('Female', 'other'))) %>% 
  mutate(dx_b2 = fct_recode(dx_b2, NULL = "substance-induced psychosis")) %>% 
  mutate(dx_b2 = fct_drop(dx_b2))

# ITT_df <- PEPP2_df %>% 
#   filter(pin <= 857) %>% 
#   select('pin', all_of(c(cov_R3STEP, cov_MixReg, cov_D3STEP)), starts_with('miss')) %>% 
#   mutate(across(any_of(cat), ~ to_factor(.x, levels = 'labels'))) %>% #Recode labelled variables as factor 
#   mutate(across(c(-any_of(cat), -where(is.Date)), ~ as.numeric(as.character(.)))) %>% #Recode labelled variables as numeric
#   mutate(dx_b2 = fct_drop(dx_b2)) %>% 
#   mutate(gender = fct_collapse(gender, male = 'Male', female = c('Female', 'other')))
  
# SOFAS -------------------------------------------------------------------
rm(list = ls())
## Load workingspace 
ws <- list.files(
  file.path(getwd(), 'SOFAS', '689'), full.names = TRUE) %>%
  file.info() %>%
  filter(isdir == FALSE) %>% 
  slice_max(mtime) %>% # get the most updated workspace
  rownames()
  
load(ws)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/LGM/R/LGMMs', modifiedOnly = FALSE, envir = globalenv())

## Step 0: Prepare dataset ------------------------------------------------
### Subset dataset
SOFAS_df <- SD_df %>%
  filter(pin <= 857)
  # filter(miss_SOFAS <= 1)

## Step 1: Growth Curve Modeling ------------------------------------------
### Run GCM model
GCM_model <- fitGCM(SOFAS_df, 
                    SOFAS, 
                    c(0, 12, 24),
                    working_dir = file.path(getwd(), 'SOFAS', '689'))

### Get GCM model fit indices
GCM_fit <- GCM_model %>% 
  pluck("results", "summaries") %>% 
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  select("Title", "Observations", "Parameters", "LL", "AIC", "AICC", "CAIC", "BIC") 

GCM_param <- GCM_model %>% 
  pluck("results", "parameters", "unstandardized") %>% 
  filter(str_detect(paramHeader, 'Means|^Variances'))
  
## Step 2: Group-Based Trajectory Modeling ---------------------------------
### Run GBTM models 
GBTM_models <- fitGBTM(
  df = SOFAS_df,
  usevar = SOFAS,
  timepoints = c(0, 12, 24),
  idvar = 'pin',
  working_dir = file.path(getwd(), 'SOFAS', '689'),
  overall_polynomial = 2, 
  max_k = 6)

### Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models) 

### Select best GBTM model 
# GBTM_best <- BEST(GBTM_models, GBTM_fit)
GBTM_best <- GBTM_models[[2]]

## Step 3: Latent Class Growth Analyses ------------------------------------
### Run LCGA models 
LCGA_models <- fitLCGA(
  df = SOFAS_df,
  usevar = SOFAS,
  timepoints = c(0, 12, 24),
  idvar = 'pin',
  classes = 2,
  overall_polynomial = 2,
  working_dir = file.path(getwd(), 'SOFAS', '689'),
  ref_model = GBTM_best) 

### Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) 

### Select best LCGA model 
LCGA_best <- BEST(LCGA_models, LCGA_fit)

## Step 4: Growth Mixture Models ------------------------------------------
### Add class-invariant random effect variances stepwise 
GMMci_models <- map(
  c(2000, 4000),
  \(startval) GMM(
    df = SOFAS_df,
    idvar = "pin",
    usevar = SOFAS,
    k = 2,
    startval = startval,
    overall_polynomial = 2,
    random_effect = "CI",
    output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"), 
    working_dir = file.path(getwd(), 'SOFAS', '689')
  )
)

#### Get Fit indices 
GMMci_fit <- map(GMMci_models, ~ getFitIndices(.x))

### Select best GMMci model 
GMMci_best <- BEST(GMMci_models[[2]], GMMci_fit[[2]])

### Add class-variant random effect variances stepwise 
GMMcv_models <- map(
  c(2000, 4000),
  \(startval) GMM(
    df = SOFAS_df,
    idvar = "pin",
    usevar = SOFAS,
    k = 2,
    startval = startval,
    overall_polynomial = 2,
    random_effect = "CV",
    output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"), 
    working_dir = file.path(getwd(), 'SOFAS', '689')
  )
)

#### Get Fit Indices 
GMMcv_fit <- map(GMMcv_models, ~ getFitIndices(.x))

### Select best GMMcv model 
GMMcv_best <- BEST(GMMcv_models[[2]], GMMcv_fit[[2]]) 

#### Alternative model 
#GMM_CV_isq@0_CT has better BIC, support from BF10, but LL not replicated & traj //
BF10 <- exp((GMMcv_fit[[4]][3,'BIC'] - GMMcv_fit[[4]][10,'BIC'])/2) 

## Step 5: Select best model -----------------------------------------------
# mutate(BF10 = exp((model_fit['#choose model to test','BIC'] - model_fit['#choose model to test','BIC'])/2))

### Get fit indices of all selected models
BEST_fit <- list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best) %>% BEST(BEST_fit)

BEST_param <- BEST_model %>% 
  pluck("results", "parameters", "unstandardized") %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 6: Refine Polynomial Order -----------------------------------------
FINAL_model <- poly(
  model = BEST_model, 
  df = SOFAS_df,
  usevar = 'SOFAS',
  p = 2)

## Step 7: Examine final model -------------------------------------------
### Examine fit indices, parameters and distribution
FINAL_fit <- getFitIndices(list(FINAL_model))

FINAL_param <- FINAL_model %>% 
  pluck("results", "parameters", "unstandardized") %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

FINAL_sk <- SKTEST(FINAL_model, "SOFAS")

### Save final dataset
SOFAS_df <- FINAL_model %>% 
  pluck('results', 'savedata') %>% 
  select(- any_of(c(as.vector(names(SOFAS_df)))), 'PIN') %>%
  add_suffix(everything(), suffix = 'SOFAS') %>% 
  merge(SOFAS_df, .,  all = TRUE, by.x ='pin', by.y ='PIN_SOFAS')

PEPP2_df <- FINAL_model %>% 
  pluck('results', 'savedata') %>% 
  select(- any_of(c(as.vector(names(PEPP2_df)))), 'PIN') %>%
  add_suffix(everything(), suffix = 'SOFAS') %>% 
  merge(PEPP2_df, .,  all = TRUE, by.x ='pin', by.y ='PIN_SOFAS')

## Step 8: Plot trajectories -----------------------------------------------
plot <- plotGrowthMixtures(FINAL_model, bw = TRUE, rawdata = TRUE, time_scale = c(0, 12, 24))

## Step 9: Covariates -------------------------------------------------
## The 3-Step Procedure - multinomial logistic regressions with predictors of class membership
### Manual
R3STEP_models <- R3STEP(
  df = SOFAS_df,
  idvar = 'pin',
  usevar = SOFAS,
  cov = setdiff(cov_R3STEP, c('SOFAS_0')),
  model = FINAL_model
)

R3STEP_unstd <- R3STEPfit(R3STEP_models, std = 'unstd', ref = 1)
  # filter(pval < (0.05 / length(R3STEP_models))) %>%
  # filter(str_detect(paramHeader, "C#\\d.ON")) %>%
  # filter(is.na(errors)) %>% 
  # select(-warnings, -errors)

## The 3-Step Procedure - mixture regressions with predictors of growth factors 
# CANNOT BE COMPUTED BECAUSE FINAL_MODEL IS GBTM (NO WITHIN CLASS VARIANCE)

## The 3-Step Procedure - Linear regressions with distal outcome
D3STEP_models <- D3STEP(
  df = SOFAS_df,
  idvar = 'pin',
  usevar = SOFAS,
  cov = setdiff(cov_D3STEP, c('SOFAS_24')),
  startval = list(CDS_24 = 128000, comp_24 = 500, JSR_24C = 128000),
  model = FINAL_model
)

D3STEP_unstd <- D3STEPfit(D3STEP_models, std = 'unstd')

## Step 10: Sensitivity analyses ----------------------------------------------------
### Individually varying timescores ----------------------------------------------------
#### Exclude cases with complete SOFAS and missing date of observation 
TSCORES_df <- TSCORES_df %>%
  filter(xor(!is.na(SOFAS_12), is.na(t12))) %>%
  filter(xor(!is.na(SOFAS_24), is.na(t24)))

#map <- map2(x, y, ~filter(xor(!is.na(x), is.na(y)))) # Does not work

#### Create an Mplus object 
.mpobj <- update(
  FINAL_model,
  VARIABLE = ~ . +'TSCORES = t0-t24;',
  ANALYSIS = ~ 'TYPE = MIXTURE RANDOM;,
        STARTS = 1000 250;,
        PROCESSORS = 8;',
  OUTPOUT = ~ 'SAMPSTAT;',
  autov = FALSE,
  rdata = SOFAS_df)

.mpobj[['TITLE']] <- str_replace(.mpobj[['TITLE']], ';', '_TSCORES;')
.mpobj[['MODEL']] <- str_replace(.mpobj[['MODEL']], '((\\n)?[:alpha:]+_(\\d)+@(\\d)+(\\s)?)+', 'SOFAS_0-SOFAS_24 AT t0-t24') #use with str_view(0 or 1 new line(\n)); (one or more letters[:alpha:])_; (one or more digits(\d)); (0 or 1 new line(\s)); 1 or more times 
.mpobj[['SAVEDATA']] <- str_replace(.mpobj[['SAVEDATA']], '_', '_TSCORES_') 

### Create, run, and read Mplus models 
FitIndices_GBTM_tscores <- mplusModeler(
  object = .mpobj,
  dataout = glue(getwd(),'/SOFAS/Results/TSCORES.dat'),
  modelout = glue(getwd(),'/SOFAS/Results/TSCORES.inp'),
  hashfilename = FALSE,
  run = 0,
  check=TRUE,
  varwarnings = TRUE,
  writeData ='always'
)

## Save --------------------------------------------------------------------
save.image(glue(getwd(), 'SOFAS','689', 'SOFAS_689_{today()}.RData', .sep = '/'))

# SAPS --------------------------------------------------------------------
rm(list = ls())
## Load workingspace 
ws <- list.files(
  file.path(getwd(), 'SAPS', '689'), full.names = TRUE) %>%
  file.info() %>%
  filter(isdir == FALSE) %>% 
  slice_max(mtime) %>% # get the most updated workspace
  rownames() 

  load(ws)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/LGM/R/LGMMs', modifiedOnly= FALSE, envir=globalenv())

## Step 0: Prepare dataset ------------------------------------------------
SAPS_df <- SD_df 
  # subset(miss_SAPS <= 4)

## Step 1: Growth Curve Modeling ------------------------------------------
## Run GCM model
GCM_model <- fitGCM(SAPS_df, 
                    SAPS, 
                    c(0, 1, 2, 3, 6, 9, 12, 18, 24),
                    working_dir = file.path(getwd(), 'SAPS', '689'))

### Get GCM model fit indices
GCM_fit <- GCM_model %>% 
  pluck("results", "summaries") %>% 
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  select("Title", "Observations", "Parameters", "LL", "AIC", "AICC", "CAIC", "BIC") 

GCM_param <- GCM_model %>% 
  pluck("results", "parameters", "unstandardized") %>% 
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 2: Group-Based Trajectory Modeling ---------------------------------
## Run GBTM models 
GBTM_models <- fitGBTM(
  df = SAPS_df,
  usevar = SAPS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  working_dir = file.path(getwd(), 'SAPS', '689'),
  max_k = 6)

## Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models)
 
## Select best GBTM model 
# GBTM_best <- BEST(GBTM_models, GBTM_fit)
GBTM_best <-  GBTM_models[[2]]

## Step 3: Latent Class Growth Analyses ------------------------------------
## Run LCGA models 
LCGA_models <- fitLCGA(
  df = SAPS_df,
  usevar = SAPS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  classes = 2,
  working_dir = file.path(getwd(), 'SAPS', '689'),
  ref_model = GBTM_best)

### Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models)

### Select best LCGA model 
LCGA_best <- BEST(LCGA_models, LCGA_fit)

## Step 4: Growth Mixture Models ------------------------------------------
### Add class-invariant random effect variances stepwise 
GMMci_models <- map(
  c(500, 1000), #1000, 2000
  \(startval) GMM(
    df = SAPS_df,
    idvar = "pin",
    usevar = SAPS,
    k = 2,
    startval = startval,
    overall_polynomial = 3,
    random_effect = "CI",
    output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"), 
    working_dir = file.path(getwd(), 'SAPS', '689')
  )
)

#### Get Fit indices 
GMMci_fit <- map(GMMci_models, ~ getFitIndices(.x))

### Select best GMMci model 
GMMci_best <- BEST(GMMci_models[[2]], GMMci_fit[[2]])

### Alternative model
BF10 <- exp((GMMci_fit[14,'BIC'] - GMMci_fit[6,'BIC'])/2)

### Add class-variant random effect variances stepwise 
GMMcv_models <- map(
  c(2000, 4000),
  \(startval) GMM(
    df = SAPS_df,
    idvar = "pin",
    usevar = SAPS,
    k = 2,
    startval = startval,
    overall_polynomial = 3,
    random_effect = "CV",
    output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"), 
    working_dir = file.path(getwd(), 'SAPS', '689')
  )
)

#### Get Fit indices 
GMMcv_fit <- map(GMMcv_models, ~ getFitIndices(.x))

### Select best GMMci model 
GMMcv_best <- BEST(GMMcv_models[[2]], GMMcv_fit[[2]])

## Step 5: Select Best Model -------------------------------------------------------
### Get fit indices of all selected models
BEST_fit <- list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best) %>% BEST(BEST_fit)

BEST_param <- BEST_model %>% 
  pluck("results", "parameters", "unstandardized") %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 6: Refine Polynomial Order -----------------------------------------
FINAL_model <- poly(
  model = BEST_model, 
  df = SAPS_df,
  usevar = 'SAPS',
  p = 3)

## Step 7: Examine final model ----------------------
### Examine fit indices, parameters and distribution
FINAL_fit <- getFitIndices(list(FINAL_model))

FINAL_param <- FINAL_model %>% 
  pluck("results", "parameters", "unstandardized") %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

FINAL_sk <- SKTEST(OPTIMAL_model, "SAPS") #Skewed distribution > 2

OPTIMAL_model <- DIST(FINAL_model, "SAPS", "SKEWNORMAL") #DISTRIBUTION = SKEWNORMAL {C}
OPTIMAL_fit <-  getFitIndices(list(OPTIMAL_model))

### Save final dataset 
SAPS_df <- FINAL_model %>% 
  pluck('results', 'savedata') %>% 
  select(- any_of(c(as.vector(names(SAPS_df)))), 'PIN') %>%
  add_suffix(everything(), suffix = 'SAPS') %>% 
  merge(SAPS_df, .,  all = TRUE, by.x ='pin', by.y ='PIN_SAPS')

PEPP2_df <- FINAL_model %>% 
  pluck('results', 'savedata') %>% 
  select(- any_of(c(as.vector(names(PEPP2_df)))), 'PIN') %>%
  add_suffix(everything(), suffix = 'SAPS') %>% 
  merge(PEPP2_df, .,  all = TRUE, by.x ='pin', by.y ='PIN_SAPS')

## Rerun models with appropriate distribution ------------------------------------------
### Add class-invariant random effect variances stepwise 
GMMci_models_dist <- GMM(
  df = SAPS_df,
  idvar = "pin",
  usevar = SAPS,
  k = 2,
  startval = 4000,
  dist = "SKEWNORMAL",
  overall_polynomial = 3,
  random_effect = "CI",
  output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"),
  working_dir = "~/Desktop/DIST"
)

GMMci_fit_dist <- getFitIndices(GMMci_models_dist)
GMMci_best_dist <- BEST(GMMci_models_dist, GMMci_fit_dist)


### Add class-variant random effect variances stepwise 
GMMcv_models_dist <- GMM(
  df = SAPS_df,
  idvar = "pin",
  usevar = SAPS,
  k = 2,
  startval = 4000,
  dist = "SKEWNORMAL",
  overall_polynomial = 3,
  random_effect = "CV",
  output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"), 
  working_dir = "~/Desktop/DIST"
)

GMMcv_fit_dist <- getFitIndices(GMMcv_models_dist)
GMMcv_best_dist <- BEST(GMMcv_models_dist, GMMcv_fit_dist)

### Select Best Model 
BEST_fit_dist <- list(GBTM_best, LCGA_best, GMMci_best_dist, GMMcv_best_dist) %>% getFitIndices()
BEST_model_dist <- list(GBTM_best, LCGA_best, GMMci_best_dist, GMMcv_best_dist) %>% 
  BEST(BEST_fit_dist)

BEST_param_dist <- BEST_model_dist %>% 
  pluck("results", "parameters", "unstandardized") %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 8: Plot trajectories -----------------------------------------------
plot <- plotGrowthMixtures(FINAL_model, bw = TRUE,  rawdata = TRUE, time_scale = c(0, 1, 2, 3, 6, 9, 12, 18, 24))
plotGrowthMixtures(OPTIMAL_model, bw = TRUE,  rawdata = TRUE, time_scale = c(0, 1, 2, 3, 6, 9, 12, 18, 24))

## Step 9: Covariates -------------------------------------------------
## The 3-Step Procedure - multinomial logistic regressions with predictors of class membership
### Manual
   R3STEP_models <- R3STEP(
     df = SAPS_df,
     idvar = 'pin',
     usevar = SAPS,
     cov = setdiff(cov_R3STEP, c('SAPS_0')),
     model = OPTIMAL_model
   )
   
   R3STEP_unstd <- R3STEPfit(R3STEP_models, std = 'unstd', ref = 1)  %>% 
     filter(pval < (0.05 / length(R3STEP_models))) %>%
     filter(str_detect(paramHeader, "C#\\d.ON")) %>%
     filter(is.na(errors)) %>%
     select(-warnings, -errors)

# The 3-Step Procedure - mixture regressions with predictors of growth factors
   MixREG_models <- MixREG(
     df = SAPS_df,
     idvar = 'pin',
     usevar = SAPS, 
     cov = list(SOFAS, SANS, YMRS, HAS, CDS, SCD, SUMD1, SUMD3, NSR, JSR),
     # startval = list(SOFAS = 1000, SCD = 1000, YMRS = 1000),
     gf = "S",
     gfw = c("iw", "sw"),
     test = "S_sw",
     model = FINAL_model
   )
  
   MixREG_stdyx_cov <- MixREGfit(MixREG_models, std = 'stdyx') %>% 
     filter(is.na(warnings) & is.na(errors)) %>% 
     filter(WaldChiSq_PValue < (0.05 / length(MixREG_models))) %>%
     select(-warnings, -errors)
 
   
## The 3-Step Procedure - Linear regressions with distal outcome
   D3STEP_models <- D3STEP(
     df = SAPS_df,
     idvar = 'pin',
     usevar = SAPS,
     cov = setdiff(cov_D3STEP, c('SAPS_24')),
     startval = list(comp_24 = 1000),
     model = FINAL_model
   )
   
   D3STEP_unstd <- D3STEPfit(D3STEP_models, std = 'unstd')
   
## Step 10: Sensitivity analyses ----------------------------------------------------
### Individually varying timescores ----------------------------------------
### Exclude cases with complete SAPS and missing date of observation 
TSCORES_df <- TSCORES_df %>%
  filter(xor(!is.na(SAPS_1), is.na(t1))) %>%
  filter(xor(!is.na(SAPS_2), is.na(t2))) %>%
  filter(xor(!is.na(SAPS_3), is.na(t3))) %>%
  filter(xor(!is.na(SAPS_6), is.na(t6))) %>%
  filter(xor(!is.na(SAPS_9), is.na(t9))) %>%
  filter(xor(!is.na(SAPS_12), is.na(t12))) %>%
  filter(xor(!is.na(SAPS_18), is.na(t18))) %>%
  filter(xor(!is.na(SAPS_24), is.na(t24)))

#map <- map2(x, y, ~filter(xor(!is.na(x), is.na(y)))) # Does not work

### Create an Mplus object 
.mpobj <- update(
      FINAL_model,
      VARIABLE = ~ . +'TSCORES = t0-t24;',
      ANALYSIS = ~ 'TYPE = MIXTURE RANDOM;,
        STARTS = 1000 250;,
        PROCESSORS = 8;',
      OUTPOUT = ~ 'SAMPSTAT;',
      autov = FALSE,
      rdata = SAPS_df)

   .mpobj[['TITLE']] <- str_replace(.mpobj[['TITLE']], ';', '_TSCORES;')
   .mpobj[['MODEL']] <- str_replace(.mpobj[['MODEL']], '((\\n)?[:alpha:]+_(\\d)+@(\\d)+(\\s)?)+', 'SAPS_0-SAPS_24 AT t0-t24') #use with str_view(0 or 1 new line(\n)); (one or more letters[:alpha:])_; (one or more digits(\d)); (0 or 1 new line(\s)); 1 or more times 
   .mpobj[['SAVEDATA']] <- str_replace(.mpobj[['SAVEDATA']], '_', '_TSCORES_') 
    
# Create, run, and read Mplus models 
   FitIndices_GBTM_tscores <- mplusModeler(
     object = .mpobj,
     dataout = glue(getwd(),'/SAPS/Results/TSCORES.dat'),
     modelout = glue(getwd(),'/SAPS/Results/TSCORES.inp'),
     hashfilename = FALSE,
     run = 0,
     check=TRUE,
     varwarnings = TRUE,
     writeData ='always'
   )
   
## Save --------------------------------------------------------------------
save.image(glue(getwd(), 'SAPS', '689', 'SAPS_689_{today()}.RData', .sep = '/'))
    
# SANS --------------------------------------------------------------------
rm(list = ls())
## Load workingspace 
ws <- list.files(
  file.path(getwd(), 'SANS', '689'), full.names = T) %>%
  file.info() %>%
  filter(isdir==FALSE) %>% 
  slice_max(mtime) %>% # get the most updated workspace
  rownames()
    
load(ws)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/LGM/R/LGMMs', modifiedOnly= FALSE, envir=globalenv())

## Step 0: Prepare dataset ------------------------------------------------
  SANS_df <- SD_df
    # subset(miss_SANS <= 4)

## Step 1: Growth Curve Modeling ------------------------------------------
## Run GCM model
GCM_model <- fitGCM(SANS_df, 
                    SANS, 
                    c(0, 1, 2, 3, 6, 9, 12, 18, 24), 
                    working_dir = file.path(getwd(), 'SANS', '689'))

### Get GCM model fit indices
GCM_fit <- GCM_model %>% 
  pluck("results", "summaries") %>% 
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  select("Title", "Observations", "Parameters", "LL", "AIC", "AICC", "CAIC", "BIC") 

GCM_param <- GCM_model %>% 
  pluck("results", "parameters", "unstandardized") %>% 
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 2: Group-Based Trajectory Modeling ---------------------------------
## Run GBTM models 
GBTM_models <- fitGBTM(
  df = SANS_df,
  usevar = SANS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  working_dir = file.path(getwd(), 'SANS', '689'),
  max_k = 6)

## Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models)

## Select best GBTM model 
GBTM_best <- BEST(GBTM_models, GBTM_fit)

## Step 3: Latent Class Growth Analyses ------------------------------------
## Run LCGA models 
LCGA_models <- fitLCGA(
  df = SANS_df,
  usevar = SANS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  classes = 3,
  working_dir = file.path(getwd(), 'SANS', '689'),
  ref_model = GBTM_best)

## Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) 

## Select best LCGA model 
LCGA_best <- BEST(LCGA_models, LCGA_fit)

## Step 4: Growth Mixture Models ------------------------------------------
### Add class-invariant random effect variances stepwise 
GMMci_models <- map(
  c(500, 1000),
  \(startval) GMM(
    df = SANS_df,
    idvar = "pin",
    usevar = SANS,
    k = 3,
    startval = startval,
    overall_polynomial = 3,
    random_effect = "CI",
    output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"), 
    working_dir = file.path(getwd(), 'SANS', '689')
  )
)

#### Get Fit Indices 
GMMci_fit <- map(GMMci_models, ~ getFitIndices(.x))

### Select best GMMci model 
GMMci_best <- BEST(GMMci_models[[2]], GMMci_fit[[2]])

# BF10 <- exp((GMMci_fit[9,'BIC'] - GMMci_fit[6,'BIC'])/2)

### Add class-variant random effect variances stepwise 
GMMcv_models <- map(
  c(2000, 4000),
  \(startval) GMM(
    df = SANS_df,
    idvar = "pin",
    usevar = SANS,
    k = 3,
    startval = startval,
    overall_polynomial = 3,
    random_effect = "CV",
    output = c("TECH7", "TECH11", "SAMPSTAT", "STANDARDIZED", "CINTERVAL"), 
    working_dir = file.path(getwd(), 'SANS', '689')
  )
)

#### Get Fit Indices 
GMMcv_fit <- map(GMMcv_models, ~ getFitIndices(.x))

### Select best GMMci model 
GMMcv_best <- BEST(GMMcv_models[[2]], GMMcv_fit[[2]])

## Step 5: Select Best Model -------------------------------------------------------
### Get fit indices of all selected models
BEST_fit <- list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMci_best, GMMcv_best) %>% BEST(BEST_fit)

BEST_param <- BEST_model %>% 
  pluck("results", "parameters", "unstandardized") %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 6: Refine Polynomial Order -----------------------------------------
FINAL_model <- poly(
  model = BEST_model, 
  df = SANS_df,
  usevar = 'SANS',
  p = 3)

runModels(str_c(getwd(), '/SANS/Results/FINAL/311.inp')) # SV = 8000 to replicate best LL
FINAL_model[['results']] <-  readModels(str_c(getwd(), '/SANS/Results/FINAL/311.out'))

## Step 7: Examine final model ----------------------
### Examine fit indices, parameters and distribution
FINAL_fit <- getFitIndices(list(FINAL_model))

FINAL_param <- FINAL_model %>% 
  pluck("results", "parameters", "unstandardized") %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

FINAL_sk <- SKTEST(FINAL_model, "SANS")

### Save final dataset 
SANS_df <- FINAL_model %>% 
  pluck('results', 'savedata') %>% 
  select(- any_of(c(as.vector(names(SANS_df)))), 'PIN') %>%
  add_suffix(everything(), suffix = 'SANS') %>% 
  merge(SANS_df, .,  all = TRUE, by.x ='pin', by.y ='PIN_SANS')

PEPP2_df <- FINAL_model %>% 
  pluck('results', 'savedata') %>% 
  select(- any_of(c(as.vector(names(PEPP2_df)))), 'PIN') %>%
  add_suffix(everything(), suffix = 'SANS') %>% 
  merge(PEPP2_df, .,  all = TRUE, by.x ='pin', by.y ='PIN_SANS')

## Step 8: Plot trajectories -----------------------------------------------
mplus.view.plots(file.choose())

plot <- plotGrowthMixtures(FINAL_model, bw = TRUE, rawdata = TRUE, time_scale = c(0, 1, 2, 3, 6, 9, 12, 18, 24)) 
+ geom_hline(yintercept = 8,  linetype = "dashed", color = "red")

## Step 9: Add covariates -------------------------------------------------
## The 3-Step Procedure - multinomial logistic regressions with predictors of class membership
### Manual
R3STEP_models <- R3STEP(
  df = SANS_df,
  idvar = 'pin',
  usevar = SANS,
  cov = setdiff(cov_R3STEP, c('SANS_0')),
  model = FINAL_model
)

R3STEP_unstd <- R3STEPfit(R3STEP_models, std = 'unstd', ref = 3)
  # filter(pval < (0.05 / length(R3STEP_models))) %>%
  # filter(str_detect(paramHeader, "C#\\d.ON")) %>% 
  # filter(is.na(errors)) %>%
  # select(-warnings, -errors)

# The 3-Step Procedure - mixture regressions with predictors of growth factors
MixREG_models <- MixREG(
  df = SANS_df,
  idvar = 'pin',
  usevar = SANS,
  cov = list(SOFAS, SAPS, HAS, CDS, SCD, YMRS, SUMD1, SUMD2, PSR, JSR),
  startval = list(SOFAS = 500, HAS = 500, CDS = 500, SCD = 500, SUMD1 = 500, SUMD2 = 500, PSR = 500, JSR = 500),
  gf = "S",
  gfw = c("iw", "sw"),
  test = "S_sw",
  model = FINAL_model, 
)

MixREG_stdyx <- MixREGfit(MixREG_models, std = 'stdyx') %>% 
  filter(WaldChiSq_PValue < (0.05 / length(MixREG_models))) %>%
  filter(is.na(warnings) & is.na(errors)) %>% 
  select(-warnings, -errors)

## The 3-Step Procedure - Linear regressions with distal outcome
D3STEP_models <- D3STEP(
  df = SANS_df,
  idvar = 'pin',
  usevar = SANS,
  cov = setdiff(cov_D3STEP, c('SANS_24')),
  startval = list(CDS_24 = 500, comp_24 = 500, CPZ_24 = 1000, SAPS_24 = 500, SUMD1_24 = 500, YMRS_24 = 500, JSR_24C = 500, HAS_24 = 1000),
  model = FINAL_model
)

 D3STEP_unstd <- D3STEPfit(D3STEP_models, std = 'unstd')

## Step 10: Sensitivity analyses ----------------------------------------------------
### Individually varying timescores
#### Exclude cases with complete SANS and missing date of observation 
TSCORES_df <- TSCORES_df %>%
  filter(xor(!is.na(SANS_1), is.na(t1))) %>%
  filter(xor(!is.na(SANS_2), is.na(t2))) %>%
  filter(xor(!is.na(SANS_3), is.na(t3))) %>%
  filter(xor(!is.na(SANS_6), is.na(t6))) %>%
  filter(xor(!is.na(SANS_9), is.na(t9))) %>%
  filter(xor(!is.na(SANS_12), is.na(t12))) %>%
  filter(xor(!is.na(SANS_18), is.na(t18))) %>%
  filter(xor(!is.na(SANS_24), is.na(t24)))

#map <- map2(x, y, ~filter(xor(!is.na(x), is.na(y)))) # Does not work

#### Create an Mplus object 
.mpobj <- update(
  FINAL_model,
  VARIABLE = ~ . +'TSCORES = t0-t24;',
  ANALYSIS = ~ 'TYPE = MIXTURE RANDOM;,
        STARTS = 1000 250;,
        PROCESSORS = 8;',
  OUTPOUT = ~ 'SAMPSTAT;',
  autov = FALSE,
  rdata = SANS_df)

.mpobj[['TITLE']] <- str_replace(.mpobj[['TITLE']], ';', '_TSCORES;')
.mpobj[['MODEL']] <- str_replace(.mpobj[['MODEL']], '((\\n)?[:alpha:]+_(\\d)+@(\\d)+(\\s)?)+', 'SANS_0-SANS_24 AT t0-t24') #use with str_view(0 or 1 new line(\n)); (one or more letters[:alpha:])_; (one or more digits(\d)); (0 or 1 new line(\s)); 1 or more times 
.mpobj[['SAVEDATA']] <- str_replace(.mpobj[['SAVEDATA']], '_', '_TSCORES_') 

#### Create, run, and read Mplus models 
FitIndices_GBTM_tscores <- mplusModeler(
  object = .mpobj,
  dataout = glue(getwd(),'/SANS/Results/TSCORES.dat'),
  modelout = glue(getwd(),'/SANS/Results/TSCORES.inp'),
  hashfilename = FALSE,
  run = 0,
  check=TRUE,
  varwarnings = TRUE,
  writeData ='always'
)

# Save --------------------------------------------------------------------
save.image(glue(getwd(), 'SANS', '689', 'SANS_689_{today()}.RData', .sep = '/'))

# Work in Progress --------------------------------------------------------
WIP <- runModels(
  target = "~/Desktop/LGM/SAPS/Results/FINAL/DIST_SKEWNORMAL.inp",
  showOutput = TRUE,
  logFile = NULL,
)

WIP_model <- readModels(target = "~/Desktop/LGM/SAPS/Results/FINAL/DIST_SKEWNORMAL.out")

assumptions <- function(models, cov){

  #models <- MixREG_models
  #cov <- list(SOFAS, SANS, CDS, HAS, YMRS, PSR, NSR)
    
  df <- map(models, ~ pluck(.x, "rdata"))
  cov <- map(cov, ~ paste(.x, collapse = " + "))
  
  df <- df %>% 
    map( ~ nest(.x, .by = N))
  
  reg <- map2(df, cov, \(df, cov) 
              mutate(df,
                     fit = map(data, ~ lm(S ~ cov, data = .x))
                     )
              )
}
  

df <- PEPP2_df %>%
nest(.by = C_SAPS) %>% 
  filter(!is.na(C_SAPS))

reg <- df %>% 
    mutate(fit = map(data, ~ lm(S_SAPS ~ PSR_0 + PSR_1 + PSR_2 + PSR_3 + PSR_6 + PSR_9 + PSR_12 + PSR_18 + PSR_24, data = .x)),
         summed = map(fit, summary),
         vifed = map(fit, car::vif),
         tidied = map(fit, broom::tidy),
         glanced = map(fit, broom::glance),
         augmented = map(fit, broom::augment))

untidied <- reg %>% unnest(c(tidied))

map(reg[["fit"]], ~ plot(.x))


plot(S_SAPS ~ SANS_0, data = PEPP2_df)
  
# JUNK --------------------------------------------------------------------
## Function to check if column names are unique namesx2(PEPP2_df)
namesx2 <- function(df) {

  length1 <- length(colnames(df))
  length2 <- length(unique(colnames(df)))
  if (length1 - length2 > 0 ) {
    print(paste('There are', length1 - length2, ' duplicates', sep=' '))
  }
}
anyDuplicated(colnames(PEPP2_df)) # locate column of the first duplicate)
data.frame(colnames(PEPP2_df))

## Write xlsx file
list(R3STEP_unstd, TVCreg_stdyx, MixREG_stdyx, D3STEP_stdyx) %>% 
  write.xlsx(file = glue(getwd(), 'SANS', 'results', 'SANS_{today()}.xlsx', .sep = '/'))


