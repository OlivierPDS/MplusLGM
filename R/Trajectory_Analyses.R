# To do -------------------------------------------------------------------
## improve SelectBestModel() by adding some conditions (class count < 5%, lowest k, warnings/errors)
## wrap in a function GetFitIndices() & mutate (BF10)
## wrap together fit functions (R3STEP, MixREG, D3STEP)
## improve GMMi & GMMv functions to replicate best LL (using map functions)
## runModels(): do not always overwright models

# Load packages & source functions ----------------------------------------------------------------
library(tidyverse)
library(haven)
library(magrittr)
library(glue)
library(lubridate) 
library(data.table)
library(metan)
library(MplusAutomation)
library(MplusLGM)
library(rhdf5) #BiocManager::install('rhdf5')
library(parallel)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/MplusLGM/R/LGMMs', modifiedOnly= FALSE, envir=globalenv())

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
### BF10 > 10 = strong evidence in favour of the alternative model
### H0:K=K1-1 H1:K=K1 If likelihood ratio p<0.05 then choose K1, else choose K1-1
### APPA: Values closer to 1 indicate a good fit (> 0.7 for all classes)

## Covariates/distal outcomes 
### simple/univariate regression then multiple/multivariate regressions
### missing on pred/Xs: manual R3STEP
### MI: has to be 2 separate steps/inp. files (AUXILIARY = vars excluded from IMPUTE = data)
### can use the manual R3step, including var of the covariates (instead of C on X; use C on X; X; )
### (see section 3;  http://statmodel.com/download/webnotes/webnote15.pdf)

# Installation ------------------------------------------------------------
install.packages('devtools') # Install devtools 
devtools::install_github('joshunrau/MplusLGM') # Install MplusLGM 

# Load dataset ------------------------------------------------------------
PEPP2_df <-
  list.files(
    '/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets',
    full.names = T
  ) %>%
  file.info() %>%
  filter(isdir==FALSE) %>% 
  slice_max(mtime) %>% # get the most updated file
  rownames() %>%
  read_csv() %>% 
  modify_at(c(SD_cat, SR, NSR, PSR, C), as.factor)

# Vars of interest --------------------------------------------------------
## SD  
SD_num <- c('ageentry', 'educ_num', 'FIQ', 'holltotp', 'ageonset', 'duponset', 'PAS_tot2')
SD_cat <- c('gender', 'minority', 'marital2', 'housing', 'work', 'dx_spect', 'SUD')

## Sx 
SAPS <- str_c('SAPS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SANS <- str_c('SANS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SOFAS <- str_c('SOFAS_', c(0, 12, 24))
HAS <- str_c('HAS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
CDS <- str_c('CDS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
YMRS <- str_c('YMRS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SX <- c(SAPS, SANS, SOFAS, HAS, CDS, YMRS)
SX_0 <- paste(c('SAPS', 'SANS', 'SOFAS', 'HAS', 'CDS', 'YMRS'), '0', sep = '_')
SX_24 <- paste(c('SAPS', 'SANS', 'SOFAS', 'HAS', 'CDS', 'YMRS'), '24', sep = '_')


## Remission 
PSR <- str_c('PSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
NSR <- str_c('NSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SR <- c('PSR_BY3', 'NSR_BY3', 'NSR_at3', 'NSR_at3')
SR_C <- c('PSR_24C', 'NSR_24C')

## Trajectories 
C <- c('C_SAPS', 'C_SANS', 'C_SOFAS')
CP <-  c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')
#t <- str_c('t', c(0, 1, 2, 3, 6, 9, 12, 18, 24))


# SOFAS -------------------------------------------------------------------
## Load workingspace 
.ws <- list.files(
  str_c(getwd(), '/SOFAS'), full.names = T) %>%
  file.info() %>%
  filter(isdir==FALSE) %>% 
  slice_max(mtime) %>% # get the most updated workspace
  rownames()
  
load(.ws)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/MplusLGM/R/LGMMs', modifiedOnly= FALSE, envir=globalenv())

## Step 0: Prepare dataset  ------------------------------------------------
### Load dataset 
SOFAS_df <- 
  list.files(
    '/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets',
    full.names = T
  ) %>%
  file.info() %>%
  filter(isdir==FALSE) %>% 
  slice_max(ctime) %>% # get the most updated file
  rownames() %>%
  read_csv() %>%
  subset(miss_SOFAS <= 1 & n == 1) %>% 
  select ('pin', all_of(c(SX, SD_num, SD_cat, PSR, NSR, SR, SR_C))) %>% 
  modify_at(c(SD_cat, SR, PSR, NSR, C), as.factor)

### rename vars > 8 characters
SOFAS_df <- names(SOFAS_df) %>% 
  grep('.{9,}', ., value = TRUE) %>% 
  setnames(SOFAS_df, old = ., new = c('varname'))
# names(SOFAS_df) <- str_sub(names(SOFAS_df), 1, 8) # remove characters from variables names > 8 characters 

## Step 1: Growth Curve Modeling  ------------------------------------------
### Run GCM model
GCM_model <- fitGCM(SOFAS_df, SOFAS, c(0, 12, 24))

### Get GCM model fit indices
GCM_fit <- GCM_model[['results']][['parameters']][['std.standardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))
  
## Step 2: Group-Based Trajectory Modeling ---------------------------------
### Run GBTM models 
GBTM_models <- fitGBTM(
  df = SOFAS_df,
  usevar = SOFAS,
  timepoints = c(0, 12, 24),
  idvar = 'pin',
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  overall_polynomial = 2, 
  max_k = 6)

### Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models) 
# mutate(BF10 = exp((GBTM_fit['#choose model to test','BIC']-GBTM_fit['#choose model to test','BIC'])/2))

### Select best GBTM model 
GBTM_best <- selectBestModel(GBTM_models, selection_method = 'BIC') 

## Step 3: Latent Class Growth Analyses ------------------------------------
### Run LCGA models 
LCGA_models <- fitLCGA(
  df = SOFAS_df,
  usevar = SOFAS,
  timepoints = c(0, 12, 24),
  idvar = 'pin',
  classes = 2,
  overall_polynomial = 2,
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  ref_model = GBTM_best)

### Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) 
#mutate(BF10 = exp((LCGA_fit['# choose model to test', 'BIC']-LCGA_fit['# choose model to test', 'BIC'])/2)) 

### Select best LCGA model 
LCGA_best <- selectBestModel(LCGA_models, selection_method = 'BIC')

## Step 4: Growth Mixture Models  ------------------------------------------
### Add class-invariant random effect variances stepwise 
GMMi_models <- fitGMMi(
  df = SOFAS_df, 
  usevar = 'SOFAS', 
  list_mpobj = LCGA_models, 
  overall_polynomial = 2)

#### Get Fit indices 
GMMi_fit <- getFitIndices(GMMi_models) 
#mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC'] - GMMi_fit['# chose model to test', 'BIC']) /2))

### Add class-variant random effect variances stepwise 
GMMv_models <- fitGMMv(
  df = SOFAS_df, 
  usevar = 'SOFAS', 
  list_mpobj = GMMi_models, 
  overall_polynomial = 2)

runModels(str_c(getwd(), '/SOFAS/Results/GMMv/GMM4/GMM4v_i s q@0.inp')) #best LL not replicated (32000)
GMMv_models[[4]][['i s q@0']][['results']] <-  readModels(str_c(getwd(), '/SOFAS/Results/GMMv/GMM4/GMM4v_i s q@0.out'))  
    
#### Get Fit Indices 
GMMv_fit <- getFitIndices(GMMv_models) 
# mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC']-GMMi_fit['# chose model to test', 'BIC'])/2))

### Select best GMM model 
GMMi_best <- GMMi_models[[1]][['i s-q@0']] # because warnings
GMMv_best <- GMMv_models[[1]][['i s-q@0']] # because warnings

#GMMi_best <- unlist(GMMi_models, FALSE) %>% selectBestModel(selection_method = 'BIC_LRT')
#GMMv_best <- selectBestModel(unlist(GMMv_models, FALSE), selection_method = 'BIC_LRT')

## Step 5: Select best model -----------------------------------------------
### Get fit indices of all selected models
BEST_fit <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% selectBestModel()

BEST_param <-
  BEST_model[['results']][['parameters']][['unstandardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 6: Refine Polynomial Order -----------------------------------------
FINAL_model <- poly(
  model = BEST_model, 
  df = SOFAS_df,
  usevar = SOFAS,
  p = 2)

## Step 7: Examine final model -------------------------------------------
### Examine fit indices, parameters and distribution
FINAL_fit <- getFitIndices(FINAL_model)

FINAL_param <- FINAL_model[['results']][['parameters']][['unstandardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

FINAL_sk <- SKTEST(FINAL_model, "SOFAS")

### Save final dataset
  PEPP2_df <- FINAL_model[['results']][['savedata']] %>% 
  modify_at('C', as.factor) %>% 
  select(., -starts_with('SOFAS')) %>% 
  add_suffix(., everything(), suffix = 'SOFAS') %>% 
  merge(PEPP2_df, ., all = TRUE, by.x ='pin', by.y ='PIN_SOFAS')

#final_dataset <- getDataset(final_model, SOFAS_df, 'pin') 
  
  write_csv(PEPP2_df, paste0('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_', today(), '.csv'))

## Step 8: Plot trajectories -----------------------------------------------
plot <- plotGrowthMixtures(FINAL_model, bw = TRUE, rawdata = TRUE, time_scale = c(0, 12, 24))

est_means <- FINAL_model[['results']][['gh5']][['means_and_variances_data']][['y_estimated_medians']][['values']] %>%
  as.data.frame()

## Step 9: Add covariates  -------------------------------------------------
## The 3-Step Procedure - multinomial logistic regressions with predictors of class membership
### Manual
R3STEP_models <- R3STEP(
  df = SOFAS_df,
  idvar = 'pin',
  usevar = SOFAS,
  cov = c(SD_num, SD_cat, SX_0),
  model = FINAL_model
)

R3STEP_unstd <- R3STEPfit(R3STEP_models, std = 'unstd', ref = 2)
R3STEP_stdyx <- R3STEPfit(R3STEP_models, std = 'stdyx', ref = 2)

## The 3-Step Procedure - mixture regressions with predictors of growth factors 
# CANNOT BE COMPUTED BECAUSE FINAL_MODEL IS GBTM (NO WITHIN CLASS VARIANCE)

## The 3-Step Procedure - regressions with time-varying covariates
# PSR, NSR: try with higher start values
TVCreg_models <- TVCreg(
  df = SOFAS_df,
  idvar = 'pin',
  usevar = SOFAS,
  cov = list(SAPS, SANS, CDS, HAS, YMRS, PSR, NSR),
  starts = 500,
  model = FINAL_model
)

TVCreg_unstd <- MixREGfit(TVCreg_models, std = 'unstd')
TVCreg_stdyx <- MixREGfit(TVCreg_models, std = 'stdyx')

## The 3-Step Procedure - Linear regressions with distal outcome
D3STEP_models <- D3STEP(
  df = SOFAS_df,
  idvar = 'pin',
  usevar = SOFAS,
  cov = c(SX_24, 'PSR_24', 'NSR_24', SR_C, SR),
  model = FINAL_model
)

D3STEP_stdyx <- D3STEPfit(D3STEP_models, std = 'stdyx')
D3STEP_unstd <- D3STEPfit(D3STEP_models, std = 'unstd')

## Step 10: Sensitivity analyses ----------------------------------------------------
### Individually varying timescores 
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
save.image(glue(getwd(), 'SOFAS', 'SOFAS_{today()}.RData', .sep = '/'))

# SAPS --------------------------------------------------------------------
## Load workingspace 
.ws <- list.files(
  str_c(getwd(), '/SAPS'), full.names = T) %>%
  file.info() %>%
  filter(isdir==FALSE) %>% 
  slice_max(mtime) %>% # get the most updated workspace
  rownames() 

  load(.ws)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/MplusLGM/R/LGMMs', modifiedOnly= FALSE, envir=globalenv())

## Step 0: Prepare dataset  ------------------------------------------------
## Load dataset 
SAPS_df <- 
  list.files(
    '/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets',
    full.names = T
  ) %>%
  file.info() %>%
  filter(isdir==FALSE) %>% 
  slice_max(ctime) %>% # get the most updated file
  rownames() %>%
  read_csv() %>%
  subset(miss_SAPS <= 4 & n == 1) %>% 
  select ('pin', all_of(c(SX, SD_num, SD_cat, PSR, NSR, SR_C, SR))) %>% 
  modify_at(c(SD_cat, SR, PSR, NSR, C), as.factor)
  
## rename vars > 8 characters
SAPS_df <- names(SAPS_df) %>% 
  grep('.{9,}', ., value = TRUE) %>% 
  setnames(SAPS_df, old = ., new = c('minority', 'housing', 'work'))

## Step 1: Growth Curve Modeling  ------------------------------------------
## Run GCM model
GCM_model <- fitGCM(SAPS_df, SAPS, c(0, 1, 2, 3, 6, 9, 12, 18, 24))

## Get GCM model fit indices
GCM_fit <-
  GCM_model[['results']][['parameters']][['unstandardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 2: Group-Based Trajectory Modeling ---------------------------------
## Run GBTM models 
GBTM_models <- fitGBTM(
  df = SAPS_df,
  usevar = SAPS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  max_k = 6)

## Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models) #mutate(BF10 = exp((GBTM_fit['#choose model to test','BIC']-GBTM_fit['#choose model to test','BIC'])/2))
 
## Select best GBTM model 
GBTM_best <- GBTM_models[[2]] #GBTM_best <- selectBestModel(GBTM_models, selection_method = 'BIC')

## Step 3: Latent Class Growth Analyses ------------------------------------
## Run LCGA models 
LCGA_models <- fitLCGA(
  df = SAPS_df,
  usevar = SAPS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  classes = 2,
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  ref_model = GBTM_best)

## Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) #mutate(BF10 = exp((LCGA_fit['# choose model to test', 'BIC']-LCGA_fit['# choose model to test', 'BIC'])/2)) 

## Select best LCGA model 
LCGA_best <- selectBestModel(LCGA_models, selection_method = 'BIC')

## Step 4: Growth Mixture Models  ------------------------------------------
## Add class-invariant random effect variances stepwise 
GMMi_models <- fitGMMi(SAPS_df, 'SAPS', LCGA_models, overall_polynomial = 3)

### Get Fit indices 
GMMi_fit <- getFitIndices(GMMi_models) 
#mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC'] - GMMi_fit['# chose model to test', 'BIC']) /2))

## Add class-variant random effect variances stepwise 
GMMv_models <- fitGMMv(SAPS_df, 'SAPS', GMMi_models, overall_polynomial = 3)

runModels(str_c(getwd(), '/SAPS/Results/GMMv/GMM4/GMM4_i s q cub@0.inp'))
GMMv_models[[4]][['i s q cub@0']][['results']] <-  readModels(str_c(getwd(), '/SAPS/Results/GMMv/GMM4/GMM4_i s q cub@0.out'))  #best LL not replicated at 4000 SV

### Get Fit Indices 
GMMv_fit <- getFitIndices(GMMv_models) 
#mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC']-GMMi_fit['# chose model to test', 'BIC'])/2))

## Select best GMM model 
# GMMi_best <- unlist(GMMi_models, FALSE) %>% selectBestModel(selection_method = 'BIC_LRT')
# GMMv_best <- selectBestModel(unlist(GMMv_models, FALSE), selection_method = 'BIC_LRT')

GMMi_best <- GMMi_models[[4]][['i s q cub@0']]
GMMv_best <- GMMv_models[[4]][['i s q cub@0']]


## Step 5: Select Best Model -------------------------------------------------------
### Get fit indices of all selected models
BEST_fit <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% selectBestModel()

BEST_param <-
  BEST_model[['results']][['parameters']][['unstandardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 6: Refine Polynomial Order -----------------------------------------
FINAL_model <- poly(
  model = BEST_model, 
  df = SAPS_df,
  usevar = SAPS,
  p = 3)

## Step 7: Examine final model ----------------------
### Examine fit indices, parameters and distribution
FINAL_fit <- getFitIndices(FINAL_model)

FINAL_param <- FINAL_model[['results']][['parameters']][['unstandardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

FINAL_sk <- SKTEST(FINAL_model, "SAPS")

### Save final dataset 
PEPP2_df <- 
  FINAL_model[['results']][['savedata']] %>% 
  modify_at('C', as.factor) %>% 
  select(., -starts_with('SAPS')) %>% 
  add_suffix(., everything(), suffix = 'SAPS') %>% 
  merge(PEPP2_df, ., all = TRUE, by.x ='pin', by.y ='PIN_SAPS')

write_csv(PEPP2_df, paste0('/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/PEPP2_', today(), '.csv'))

#final_dataset <- getDataset(final_model, SAPS_df, 'pin') 

## Step 8: Plot trajectories -----------------------------------------------
plot <- plotGrowthMixtures(FINAL_model, bw = TRUE, rawdata = TRUE, time_scale = c(0, 1, 2, 3, 6, 9, 12, 18, 24))

# est_means <- FINAL_model[['results']][['gh5']][['means_and_variances_data']][['y_estimated_medians']][['values']] %>% as.data.frame()

## Step 9: Add covariates  -------------------------------------------------
## The 3-Step Procedure - multinomial logistic regressions with predictors of class membership
### Manual
   R3STEP_models <- R3STEP(
     df = SAPS_df,
     idvar = 'pin',
     usevar = SAPS,
     cov = c(SD_num, SD_cat, SX_0),
     model = FINAL_model
   )
   
   R3STEP_unstd <- R3STEPfit(R3STEP_models, std = 'unstd', ref = 2)
   R3STEP_stdyx <- R3STEPfit(R3STEP_models, std = 'stdyx', ref = 2)

# The 3-Step Procedure - mixture regressions with predictors of growth factors
   MixREG_models <- MixREG(
     df = SAPS_df,
     idvar = 'pin',
     usevar = SAPS, 
     cov = list(SOFAS, SANS, CDS, HAS, YMRS, PSR, NSR),
     #startval = list(PSR = 500, NSR = 500)
     #test = "S",
     model = FINAL_model
   )
  
   MixREG_unstd <- MixREGfit(MixREG_models, std = 'unstd')
   MixREG_stdyx <- MixREGfit(MixREG_models, std = 'stdyx')
 
## The 3-Step Procedure - regressions with time-varying covariates
   TVCreg_models <- TVCreg(
     df = SAPS_df,
     idvar = 'pin',
     usevar = SAPS,
     cov = list(SOFAS, SANS, HAS, CDS, YMRS, PSR, NSR),
     #startval = list(CDS = 4000, PSR = 4000, NSR = 4000)
     model = FINAL_model
   )
   
   TVCreg_unstd <- MixREGfit(TVCreg_models, std = 'unstd')
   TVCreg_stdyx <- MixREGfit(TVCreg_models, std = 'stdyx')
   
## The 3-Step Procedure - Linear regressions with distal outcome
   D3STEP_models <- D3STEP(
     df = SAPS_df,
     idvar = 'pin',
     usevar = SAPS,
     cov = c(SX_24, 'PSR_24', 'NSR_24', SR, SR_C),
     model = FINAL_model
   )
   
   D3STEP_stdyx <- D3STEPfit(D3STEP_models, std = 'stdyx')
   D3STEP_unstd <- D3STEPfit(D3STEP_models, std = 'unstd')
   
## Step 10: Sensitivity analyses ----------------------------------------------------
### Individually varying timescores 
#### Exclude cases with complete SAPS and missing date of observation 
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

#### Create an Mplus object 
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
    
## Create, run, and read Mplus models 
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
save.image(glue(getwd(), 'SAPS', 'SAPS_{today()}.RData', .sep = '/'))
    
# SANS --------------------------------------------------------------------
## Load workingspace 
.ws <- list.files(
  str_c(getwd(), '/SANS'), full.names = T) %>%
  file.info() %>%
  filter(isdir==FALSE) %>% 
  slice_max(mtime) %>% # get the most updated workspace
  rownames()
    
load(.ws)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/MplusLGM/R/LGMMs', modifiedOnly= FALSE, envir=globalenv())

## Step 0: Prepare dataset  ------------------------------------------------
## Load dataset 
  SANS_df <-
    list.files(
      '/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets',
      full.names = T
    ) %>%
    file.info() %>%
    filter(isdir==FALSE) %>% 
    slice_max(ctime) %>% # get the most updated file
    rownames() %>%
    read_csv() %>%
    subset(miss_SANS <= 4 & n == 1) %>%
    select ('pin', all_of(c(SX, SD_num, SD_cat, PSR, NSR, SR_C, SR))) %>%
    modify_at(c(SD_cat, SR, PSR, NSR), as.factor)


## rename vars > 8 characters
SANS_df <- names(SANS_df) %>% 
  grep('.{9,}', ., value = TRUE) %>% 
  setnames(SANS_df, old = ., new = c('minority', 'housing', 'work'))

# names(SANS_df) <- str_sub(names(SANS_df), 1, 8) # remove characters from variables names > 8 characters 

## Step 1: Growth Curve Modeling  ------------------------------------------
## Run GCM model
GCM_model <- fitGCM(SANS_df, SANS, c(0, 1, 2, 3, 6, 9, 12, 18, 24))

## Get GCM model fit indices
GCM_fit <-
  GCM_model[['results']][['parameters']][['std.standardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 2: Group-Based Trajectory Modeling ---------------------------------
## Run GBTM models 
GBTM_models <- fitGBTM(
  df = SANS_df,
  usevar = SANS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  working_dir = paste(getwd(), 'SANS', sep = '/'),
  max_k = 6)

## Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models) #mutate(BF10 = exp((GBTM_fit['#choose model to test','BIC']-GBTM_fit['#choose model to test','BIC'])/2))

## Select best GBTM model 
GBTM_best <- GBTM_models[[3]]
#GBTM_best <- selectBestModel(GBTM_models, selection_method = 'BIC')

## Step 3: Latent Class Growth Analyses ------------------------------------
## Run LCGA models 
LCGA_models <- fitLCGA(
  df = SANS_df,
  usevar = SANS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = 'pin',
  classes = 3,
  working_dir = paste(getwd(), 'SANS', sep = '/'),
  ref_model = GBTM_best)

## Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) #mutate(BF10 = exp((LCGA_fit['# choose model to test', 'BIC']-LCGA_fit['# choose model to test', 'BIC'])/2)) 

## Select best LCGA model 
LCGA_best <- selectBestModel(LCGA_models, selection_method = 'BIC')

## Step 5: Select Best Model -------------------------------------------------------
### Get fit indices of all selected models
BEST_fit <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% selectBestModel()

BEST_param <-
  BEST_model[['results']][['parameters']][['unstandardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

## Step 6: Refine Polynomial Order -----------------------------------------
FINAL_model <- poly(
  model = BEST_model, 
  df = SAPS_df,
  usevar = SAPS,
  p = 3)

runModels(str_c(getwd(), '/SANS/Results/FINAL/311.inp')) # SV = 8000 to replicate best LL
FINAL_model[['results']] <-  readModels(str_c(getwd(), '/SANS/Results/FINAL/311.out'))

## Step 7: Examine final model ----------------------
### Examine fit indices, parameters and distribution
FINAL_fit <- getFitIndices(FINAL_model)

FINAL_param <- FINAL_model[['results']][['parameters']][['unstandardized']] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

FINAL_sk <- SKTEST(FINAL_model, "SANS")

### Save final dataset 
PEPP2_df <- 
  FINAL_model[['results']][['savedata']] %>% 
  modify_at('C', as.factor) %>% 
  select(., -starts_with('SANS')) %>% 
  add_suffix(., everything(), suffix = 'SANS') %>% 
  merge(PEPP2_df, ., all = TRUE, by.x ='pin', by.y ='PIN_SANS')

write_csv(PEPP2_df, paste0('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_', today(), '.csv'))

#final_dataset <- getDataset(final_model, SANS_df, 'pin') 

## Step 8: Plot trajectories -----------------------------------------------

mplus.view.plots(file.choose())

plot <- plotGrowthMixtures(FINAL_model, bw = TRUE, rawdata = TRUE, time_scale = c(0, 1, 2, 3, 6, 9, 12, 18, 24))


est_means <- FINAL_model[['results']][['gh5']][['means_and_variances_data']][['y_estimated_medians']][['values']] %>%
  as.data.frame()

## Step 9: Add covariates  -------------------------------------------------
## The 3-Step Procedure - multinomial logistic regressions with predictors of class membership
### Manual
R3STEP_models <- R3STEP(
  df = SANS_df,
  idvar = 'pin',
  usevar = SANS,
  cov = c(SD_num, SD_cat, SX_0),
  model = FINAL_model
)

R3STEP_unstd <- R3STEPfit(R3STEP_models, std = 'unstd', ref = 1)
R3STEP_stdyx <- R3STEPfit(R3STEP_models, std = 'stdyx', ref = 1)

# The 3-Step Procedure - mixture regressions with predictors of growth factors
MixREG_models <- MixREG(
  df = SANS_df,
  idvar = 'pin',
  usevar = SANS,
  cov = list(SOFAS, SAPS, HAS, CDS, YMRS),
  startval = list(CDS = 500), #YMRS = 500, #PSR = 4000; NSR = 4000 (fail to converge)
  model = FINAL_model, 
)

MixREG_unstd <- MixREGfit(MixREG_models, std = 'unstd')
MixREG_stdyx <- MixREGfit(MixREG_models, std = 'stdyx')

## The 3-Step Procedure - regressions with time-varying covariates
TVCreg_models <- TVCreg(
  df = SANS_df,
  idvar = 'pin',
  usevar = SANS,
  cov = list(SOFAS, SAPS, HAS, CDS, NSR),
  startval = list(CDS = 4000, NSR = 4000), #CDS = 4000; YMRS = 4000 (LL not replicated); NSR = 4000; PSR = 4000 (fail to converge)
  model = FINAL_model
)

TVCreg_unstd <- MixREGfit(TVCreg_models, std = 'unstd')
TVCreg_stdyx <- MixREGfit(TVCreg_models, std = 'stdyx')

## The 3-Step Procedure - Linear regressions with distal outcome
D3STEP_models <- D3STEP(
  df = SANS_df,
  idvar = 'pin',
  usevar = SANS,
  cov = c(SX_24, 'PSR_24', 'NSR_24', SR, SR_C),
  startval = list(SAPS_24 = 8000, CDS_24 = 8000),
  model = FINAL_model
)

D3STEP_unstd <- D3STEPfit(D3STEP_models, std = 'unstd')
D3STEP_stdyx <- D3STEPfit(D3STEP_models, std = 'stdyx')

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
save.image(glue(getwd(), 'SANS', 'SANS_{today()}.RData', .sep = '/'))


# Work in Progress --------------------------------------------------------
WIP <- runModels(
  target = file.choose(),
  showOutput = FALSE,
  logFile = NULL,
)

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
