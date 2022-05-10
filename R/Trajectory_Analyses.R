library(tidyverse)
library(haven)
library(magrittr)
library(glue)
library(lubridate) 
library(data.table)
library(metan)
library(MplusAutomation)
library(MplusLGM)
library(rhdf5)

R.utils::sourceDirectory('/Users/olivierpercie/Desktop/MplusLGM/R/LGMMs', modifiedOnly= FALSE)

# To do -------------------------------------------------------------------
## improve SelectBestModel() by adding some conditions (class count < 5%, lower k)
## wrap in a function GetFitIndices() & mutate (BF10)
## create function to run GMMi & GMMv
## runModels(): do not always overwright models
## adapt RefinePolynomial() to GMMs
## Create function to select best GMM model 
## discard all element with specific warning before selecting best model

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

# Installation ------------------------------------------------------------
## Install devtools 
install.packages("devtools")

## Install Bioconductor & rhdf5 packages 
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("rhdf5")

## Install MplusLGM 
devtools::install_github("joshunrau/MplusLGM")





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
SX_0 <-  c('SAPS_0', 'SANS_0', 'SOFAS_0', 'HAS_0', 'CDS_0', 'YMRS_0')
SX <- c(SAPS, SANS, SOFAS, HAS, CDS, YMRS)

## Remission 
PSR <- str_c('PSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
NSR <- str_c('NSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SR_C <- c('PSR_24C', 'NSR_24C')
SR_BY <- c('PSR_BY3', 'NSR_BY3')

## Trajectories 
C <- c('C_SAPS', 'C_SANS', 'C_SOFAS')
CP <-  c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')
t <- str_c('t', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

# SOFAS -------------------------------------------------------------------
# Step 0: Prepare dataset  ------------------------------------------------
## Load dataset 
SOFAS_df <- 
  paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_2022-05-04.csv') %>% 
  read_csv() %>%
  subset(miss_SOFAS <= 1 & n == 1) %>% 
  select ('pin', all_of(c(SX, SD_num, SD_cat, PSR, NSR, SR_C, SR_BY)))

## rename vars > 8 characters
SOFAS_df <- names(SOFAS_df) %>% 
  grep('.{9,}', ., value = TRUE) %>% 
  setnames(SOFAS_df, old = ., new = c('varname'))

# names(SOFAS_df) <- str_sub(names(SOFAS_df), 1, 8) # remove characters from variables names > 8 characters 

# Function to check if column names are unique 
# namesx2(PEPP2_df) 
# namesx2 <- function(df) { 
#   
#   length1 <- length(colnames(df))
#   length2 <- length(unique(colnames(df)))        
#   if (length1 - length2 > 0 ) {
#     print(paste("There are", length1 - length2, " duplicates", sep=" "))
#   }     
# }
# anyDuplicated(colnames(PEPP2_df)) # locate column of the first duplicate)
# data.frame(colnames(PEPP2_df))

# Step 1: Growth Curve Modeling  ------------------------------------------
## Run GCM model
GCM_model <- runGCM(SOFAS_df, SOFAS, c(0, 12, 24))

## Get GCM model fit indices
GCM_fit <-
  GCM_model[["results"]][["parameters"]][["std.standardized"]] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))
  
# Step 2: Group-Based Trajectory Modeling ---------------------------------
## Run GBTM models 
GBTM_models <- fitGBTM(
  df = SOFAS_df,
  usevar = SOFAS,
  timepoints = c(0, 12, 24),
  idvar = "pin",
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  overall_polynomial = 2, 
  max_k = 6)

## Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models) %>%
  mutate(BF10 = exp((GBTM_fit['#choose model to test','BIC']-GBTM_fit['#choose model to test','BIC'])/2))

## Select best GBTM model 
#GBTM_best <- GBTM_models[[2]]
GBTM_best <- selectBestModel(GBTM_models, selection_method = "BIC")

# Step 3: Latent Class Growth Analyses ------------------------------------
## Run LCGA models 
LCGA_models <- fitLCGA(
  df = SOFAS_df,
  usevar = SOFAS,
  timepoints = c(0, 12, 24),
  idvar = "pin",
  classes = 2,
  overall_polynomial = 2,
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  ref_model = GBTM_best)

## Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) %>% 
  mutate(BF10 = exp((LCGA_fit['# choose model to test', 'BIC']-LCGA_fit['# choose model to test', 'BIC'])/2)) 

## Select best LCGA model 
LCGA_best <- selectBestModel(LCGA_models, selection_method = "BIC")


# Step 4: Growth Mixture Models  ------------------------------------------
## Add class-invariant random effect variances stepwise 
GMMi_models <- fitGMMi(SOFAS_df, 'SOFAS', LCGA_models, overall_polynomial = 2)

### Extract errors & warnings 
.GMMi_err <- GMMi_models %>% map_depth(2, pluck, 'results', 'errors') %>%
  map_depth(3, keep, str_detect, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY") %>% 
  map_depth(2,flatten_chr) %>% 
  map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)

.GMMi_warn <- GMMi_models %>% map_depth(2, pluck, 'results', 'warnings') %>%
  map_depth(3, keep, str_detect, "WARNING:") %>% 
  map_depth(2,flatten_chr) %>% 
  map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)

### Get Fit indices 
GMMi_fit <- SummaryTable(
  unlist(GMMi_models, FALSE),
  keepCols = c(
    "Title",
    "Parameters",
    "LL",
    "AIC",
    "AICC",
    "BIC",
    "Entropy",
    "T11_LMR_Value",
    "T11_LMR_PValue"
  )
) 

GMMi_fit <- GMMi_fit %>%  
  mutate(CAIC = -2 * LL + Parameters * (log(405) + 1)) %>%
  mutate(warnings = unlist(.GMMi_warn, FALSE),
         errors = unlist(.GMMi_err, FALSE))
#mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC'] - GMMi_fit['# chose model to test', 'BIC']) /2))

## Add class-variant random effect variances stepwise 
GMMv_models <- fitGMMv(SOFAS_df, 'SOFAS', GMMi_models, overall_polynomial = 2)

### Extract errors & warnings
.GMMv_err <- GMMv_models %>% map_depth(2, pluck, 'results', 'errors') %>%
  map_depth(3, keep, str_detect, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY") %>% 
  map_depth(2,flatten_chr) %>% 
  map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)

.GMMv_warn <- GMMv_models %>% map_depth(2, pluck, 'results', 'warnings') %>%
    map_depth(3, keep, str_detect, "WARNING:") %>% 
    map_depth(2,flatten_chr) %>% 
    map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)
    
### Get Fit Indices 
GMMv_fit <- SummaryTable(
  unlist(GMMv_models, FALSE),
  keepCols = c(
    "Title",
    'Parameters',
    'LL',
    "AIC",
    "AICC",
    "BIC",
    "Entropy",
    "T11_LMR_Value",
    "T11_LMR_PValue"))

GMMv_fit <- GMMv_fit %>% 
  mutate(CAIC = -2 * LL + Parameters * (log(405) + 1)) %>%
  mutate(warnings = unlist(.GMMv_warn, FALSE),
         errors = unlist(.GMMv_err, FALSE))
# mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC']-GMMi_fit['# chose model to test', 'BIC'])/2))

## Select best GMM model 
# GMMi_best <- unlist(GMMi_models, FALSE) %>% selectBestModel(selection_method = "BIC_LRT")
# GMMv_best <- selectBestModel(unlist(GMMv_models, FALSE), selection_method = "BIC_LRT")

GMMi_best <- GMMi_models[[4]][["i s q-cub@0"]] # because warnings
GMMv_best <- GMMv_models[[4]][["i s q-cub@0"]] # because warnings

## Get fit indices of all selected models and select best model 
BEST_fit <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% selectBestModel()

# Step 5: Refine Polynomial Order -----------------------------------------
FINAL_model <- refinePolynomial(
  model = BEST_model, 
  df = SOFAS_df,
  usevar = SOFAS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  idvar = "pin")

#FINAL_model <- BEST_model

## Examine fit indices 
FINAL_fit <- list(FINAL_model) %>% getFitIndices()

# Step 6: Extract model parameters and save results  ----------------------
## Get class counts & proportions of all models 
GBTM_cc <- GBTM_models %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>%  #extract2
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(GBTM_models, pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

LCGA_cc <- LCGA_models %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>%  #extract2
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(LCGA_models, pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

GMMi_cc <- unlist(GMMi_models, FALSE) %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>% 
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(unlist(GMMi_models, FALSE), pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

GMMv_cc <- unlist(GMMv_models, FALSE) %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>% 
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  # mutate(model= map(unlist(GMMv_models, FALSE), pluck, 'TITLE')) %>% 
  # select('model', starts_with(c('count', 'proportion')))
  
  # map_depth(2, pluck, 'results', 'class_counts', 'mostLikely') %>% 
  # map_if(is_null, ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  # map_if(is_null, mutate(model = map(unlist(GMMv_models, FALSE), pluck, 'TITLE')))
  # is_null <- function(x) {!is.null(x)}
  
  ## Get and save final dataset based on most probable class membership 
  PEPP2_df <- 
  FINAL_model[["results"]][["savedata"]] %>% 
  modify_at('C', as.factor) %>% 
  select(., -starts_with('SOFAS')) %>% 
  add_suffix(., everything(), suffix = 'SOFAS') %>% 
  merge(PEPP2_df, ., all = TRUE, by.x ='pin', by.y ='PIN_SOFAS')

#final_dataset <- getDataset(final_model, SOFAS_df, 'pin') 

# Step 7: Plot trajectories -----------------------------------------------
## Get means as long form
class_means <- getLongMeans(
  df = final_dataset,
  usevar = c('SOFAS_0', 'SOFAS_1', 'SOFAS_2', 'SOFAS_3', 'SOFAS_6', 'SOFAS_9', 'SOFAS_12', 'SOFAS_18', 'SOFAS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  group_var = 'Class')

## Create line for observed symptoms
line2 <- geom_line(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class), 
  linetype = 'dashed')

## Create points for observed symptoms
point2 <- geom_point(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class, shape = Class))

## Plot final model with additional geoms for observed means
plotModel(
  model = final_model, 
  x_axis_label = 'Month', 
  y_axis_label = 'Symptoms', 
  geom_line2 = line2,
  geom_point2 = point2) + 
  scale_x_continuous(breaks = seq(0, 24, by = 3)) # Specify scale for asthetics

plotSOFAS_est <- 
  final_model[["results"]][["input"]][["data"]][["file"]] %>%
  strsplit('.dat') %>% 
  paste0('.gh5') %>% 
  mplus.plot.estimated_means()

#OR#

est.means <- 
  final_model[["results"]][["gh5"]][["means_and_variances_data"]][["y_estimated_medians"]][["values"]] %>%
  as.data.frame()

# Step 8: Using individually varying timescores ---------------------------
## Exclude cases with complete SOFAS and missing date of observation 
TSCORES_df <- TSCORES_df %>%
  filter(xor(!is.na(SOFAS_1), is.na(t1))) %>%
  filter(xor(!is.na(SOFAS_2), is.na(t2))) %>%
  filter(xor(!is.na(SOFAS_3), is.na(t3))) %>%
  filter(xor(!is.na(SOFAS_6), is.na(t6))) %>%
  filter(xor(!is.na(SOFAS_9), is.na(t9))) %>%
  filter(xor(!is.na(SOFAS_12), is.na(t12))) %>%
  filter(xor(!is.na(SOFAS_18), is.na(t18))) %>%
  filter(xor(!is.na(SOFAS_24), is.na(t24)))

#map <- map2(x, y, ~filter(xor(!is.na(x), is.na(y)))) # Does not work

## Create an Mplus object 
.mpobj <- update(
  FINAL_model,
  VARIABLE = ~ . +"TSCORES = t0-t24;",
  ANALYSIS = ~ "TYPE = MIXTURE RANDOM;,
        STARTS = 1000 250;,
        PROCESSORS = 8;",
  OUTPOUT = ~ "SAMPSTAT;",
  autov = FALSE,
  rdata = SOFAS_df)

.mpobj[["TITLE"]] <- str_replace(.mpobj[["TITLE"]], ";", "_TSCORES;")
.mpobj[["MODEL"]] <- str_replace(.mpobj[["MODEL"]], "((\\n)?[:alpha:]+_(\\d)+@(\\d)+(\\s)?)+", "SOFAS_0-SOFAS_24 AT t0-t24") #use with str_view(0 or 1 new line(\n)); (one or more letters[:alpha:])_; (one or more digits(\d)); (0 or 1 new line(\s)); 1 or more times 
.mpobj[["SAVEDATA"]] <- str_replace(.mpobj[["SAVEDATA"]], "_", "_TSCORES_") 

## Create, run, and read Mplus models 
FitIndices_GBTM_tscores <- mplusModeler(
  object = .mpobj,
  dataout = glue(getwd(),'/SOFAS/Results/TSCORES.dat'),
  modelout = glue(getwd(),'/SOFAS/Results/TSCORES.inp'),
  hashfilename = FALSE,
  run = 0,
  check=TRUE,
  varwarnings = TRUE,
  writeData ="always"
)

# Step 9: Add covariates  -------------------------------------------------
## Instructions 
### simple/univariate regression then multiple/multivariate regressions
### R3STEP: no need to list var as categorical
### missing on pred/Xs:
### MI: has to be 2 separate steps/inp. files (AUXILIARY = vars excluded from IMPUTE = data)
### can use the manual R3step, including a model for the covariates (instead of C on X; use C on X; X; )
### (see section 3;  http://statmodel.com/download/webnotes/webnote15.pdf)


## The 3-Step Procedure  
miss <- {unlist(lapply(SOFAS_df, function(x) sum(is.na(x)))) / nrow(SOFAS_df) * 100} %>% view()

### Auxilliary
### Manual
R3STEP_models <- R3STEP(
  df = SOFAS_df,
  idvar = 'pin',
  usevar = 'SOFAS',
  cov = SX,
  model = FINAL_model,
  manual_R3STEP = TRUE
)


R3STEP_fit <- fitR3STEP(R3STEP_models, SX, manual_R3STEP = TRUE)

# R3STEP_warn <- map(R3STEPm, pluck, 'results', 'warnings')
# R3STEP_err <- map(R3STEPm, pluck, 'results', 'errors')


# Save --------------------------------------------------------------------
save.image(glue(getwd(), 'SOFAS', 'SOFAS_{today()}.RData', .sep = "/"))

# SAPS --------------------------------------------------------------------
# Step 0: Prepare dataset  ------------------------------------------------
## Load dataset 
SAPS_df <- PEPP2_df %>% 
  subset(miss_SAPS <= 4 & n == 1) %>% 
  select ('pin', all_of(c(SX, SD_num, SD_cat, PSR, NSR, SR_C, SR_BY)))
  

## rename vars > 8 characters
SAPS_df <- names(SAPS_df) %>% 
  grep('.{9,}', ., value = TRUE) %>% 
  setnames(SAPS_df, old = ., new = c('minority', 'housing', 'work'))

# names(SAPS_df) <- str_sub(names(SAPS_df), 1, 8) # remove characters from variables names > 8 characters 
# namesx2(PEPP2_df) # Function to check if column names are unique 
# namesx2 <- function(df) { 
#   
#   length1 <- length(colnames(df))
#   length2 <- length(unique(colnames(df)))        
#   if (length1 - length2 > 0 ) {
#     print(paste("There are", length1 - length2, " duplicates", sep=" "))
#   }     
# }
# anyDuplicated(colnames(PEPP2_df)) # locate column of the first duplicate)
# data.frame(colnames(PEPP2_df))


# Step 1: Growth Curve Modeling  ------------------------------------------
## Run GCM model
GCM_model <- runGCM(SAPS_df, SAPS, c(0, 1, 2, 3, 6, 9, 12, 18, 24))

## Get GCM model fit indices
GCM_fit <-
  GCM_model[["results"]][["parameters"]][["std.standardized"]] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

# Step 2: Group-Based Trajectory Modeling ---------------------------------
## Run GBTM models 
GBTM_models <- fitGBTM(
  df = SAPS_df,
  usevar = SAPS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  max_k = 6)

## Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models) %>%
  mutate(BF10 = exp((GBTM_fit['#choose model to test','BIC']-GBTM_fit['#choose model to test','BIC'])/2))
 
## Select best GBTM model 
GBTM_best <- GBTM_models[[2]]
#GBTM_best <- selectBestModel(GBTM_models, selection_method = "BIC")

# Step 3: Latent Class Growth Analyses ------------------------------------
## Run LCGA models 
LCGA_models <- fitLCGA(
  df = SAPS_df,
  usevar = SAPS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  classes = 2,
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  ref_model = GBTM_best)

## Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) %>% 
  mutate(BF10 = exp((LCGA_fit['# choose model to test', 'BIC']-LCGA_fit['# choose model to test', 'BIC'])/2)) 

## Select best LCGA model 
LCGA_best <- selectBestModel(LCGA_models, selection_method = "BIC")


# Step 4: Growth Mixture Models  ------------------------------------------
## Add class-invariant random effect variances stepwise 
GMMi_models <- fitGMMi(SAPS_df, 'SAPS', LCGA_models, overall_polynomial = 2)

### Extract errors & warnings 
.GMMi_err <- GMMi_models %>% map_depth(2, pluck, 'results', 'errors') %>%
  map_depth(3, keep, str_detect, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY") %>% 
  map_depth(2,flatten_chr) %>% 
  map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)

.GMMi_warn <- GMMi_models %>% map_depth(2, pluck, 'results', 'warnings') %>%
  map_depth(3, keep, str_detect, "WARNING:") %>% 
  map_depth(2,flatten_chr) %>% 
  map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)

### Get Fit indices 
GMMi_fit <- SummaryTable(
  unlist(GMMi_models, FALSE),
  keepCols = c(
    "Title",
    "Parameters",
    "LL",
    "AIC",
    "AICC",
    "BIC",
    "Entropy",
    "T11_LMR_Value",
    "T11_LMR_PValue"
  )
) 

GMMi_fit <- GMMi_fit %>%  
  mutate(CAIC = -2 * LL + Parameters * (log(405) + 1)) %>%
  mutate(warnings = unlist(.GMMi_warn, FALSE),
         errors = unlist(.GMMi_err, FALSE))
#mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC'] - GMMi_fit['# chose model to test', 'BIC']) /2))

## Add class-variant random effect variances stepwise 
GMMv_models <- fitGMMv(SAPS_df, 'SAPS', GMMi_models, overall_polynomial = 2)

### Extract errors & warnings
.GMMv_err <- GMMv_models %>% map_depth(2, pluck, 'results', 'errors') %>%
  map_depth(3, keep, str_detect, "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY") %>% 
  map_depth(2,flatten_chr) %>% 
  map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)

.GMMv_warn <- GMMv_models %>% map_depth(2, pluck, 'results', 'warnings') %>%
  map_depth(3, keep, str_detect, "WARNING:") %>% 
  map_depth(2,flatten_chr) %>% 
  map_depth(1, modify_if, ~ length(.) ==0, ~ NA_character_)

### Get Fit Indices 
GMMv_fit <- SummaryTable(
  unlist(GMMv_models, FALSE),
  keepCols = c(
    "Title",
    'Parameters',
    'LL',
    "AIC",
    "AICC",
    "BIC",
    "Entropy",
    "T11_LMR_Value",
    "T11_LMR_PValue"))

GMMv_fit <- GMMv_fit %>% 
  mutate(CAIC = -2 * LL + Parameters * (log(405) + 1)) %>%
  mutate(warnings = unlist(.GMMv_warn, FALSE),
         errors = unlist(.GMMv_err, FALSE))
# mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC']-GMMi_fit['# chose model to test', 'BIC'])/2))

## Select best GMM model 
# GMMi_best <- unlist(GMMi_models, FALSE) %>% selectBestModel(selection_method = "BIC_LRT")
# GMMv_best <- selectBestModel(unlist(GMMv_models, FALSE), selection_method = "BIC_LRT")

GMMi_best <- GMMi_models[[4]][["i s q-cub@0"]] # because warnings
GMMv_best <- GMMv_models[[4]][["i s q-cub@0"]] # because warnings

## Get fit indices of all selected models and select best model 
BEST_fit <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% selectBestModel()

# Step 5: Refine Polynomial Order -----------------------------------------
FINAL_model <- refinePolynomial(
  model = BEST_model, 
  df = SAPS_df,
  usevar = SAPS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  idvar = "pin")

#FINAL_model <- BEST_model

## Examine fit indices 
FINAL_fit <- list(FINAL_model) %>% getFitIndices()

# Step 6: Extract model parameters and save results  ----------------------
## Get class counts & proportions of all models 
GBTM_cc <- GBTM_models %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>%  #extract2
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(GBTM_models, pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))
  
LCGA_cc <- LCGA_models %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>%  #extract2
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(LCGA_models, pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

GMMi_cc <- unlist(GMMi_models, FALSE) %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>% 
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(unlist(GMMi_models, FALSE), pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

GMMv_cc <- unlist(GMMv_models, FALSE) %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>% 
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  # mutate(model= map(unlist(GMMv_models, FALSE), pluck, 'TITLE')) %>% 
  # select('model', starts_with(c('count', 'proportion')))

  # map_depth(2, pluck, 'results', 'class_counts', 'mostLikely') %>% 
  # map_if(is_null, ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  # map_if(is_null, mutate(model = map(unlist(GMMv_models, FALSE), pluck, 'TITLE')))
  # is_null <- function(x) {!is.null(x)}
  
## Get and save final dataset based on most probable class membership 
PEPP2_df <- 
  FINAL_model[["results"]][["savedata"]] %>% 
  modify_at('C', as.factor) %>% 
  select(., -starts_with('SAPS')) %>% 
  add_suffix(., everything(), suffix = 'SAPS') %>% 
  merge(PEPP2_df, ., all = TRUE, by.x ='pin', by.y ='PIN_SAPS')

#final_dataset <- getDataset(final_model, SAPS_df, 'pin') 

# Step 7: Plot trajectories -----------------------------------------------
## Get means as long form
class_means <- getLongMeans(
  df = final_dataset,
  usevar = c('SAPS_0', 'SAPS_1', 'SAPS_2', 'SAPS_3', 'SAPS_6', 'SAPS_9', 'SAPS_12', 'SAPS_18', 'SAPS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  group_var = 'Class')

## Create line for observed symptoms
line2 <- geom_line(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class), 
  linetype = 'dashed')

## Create points for observed symptoms
point2 <- geom_point(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class, shape = Class))

## Plot final model with additional geoms for observed means
plotModel(
  model = final_model, 
  x_axis_label = 'Month', 
  y_axis_label = 'Symptoms', 
  geom_line2 = line2,
  geom_point2 = point2) + 
  scale_x_continuous(breaks = seq(0, 24, by = 3)) # Specify scale for asthetics

plotSAPS_est <- 
  final_model[["results"]][["input"]][["data"]][["file"]] %>%
  strsplit('.dat') %>% 
  paste0('.gh5') %>% 
  mplus.plot.estimated_means()

#OR#

est.means <- 
  final_model[["results"]][["gh5"]][["means_and_variances_data"]][["y_estimated_medians"]][["values"]] %>%
  as.data.frame()

# Step 8: Using individually varying timescores ---------------------------
## Exclude cases with complete SAPS and missing date of observation 
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

## Create an Mplus object 
.mpobj <- update(
      FINAL_model,
      VARIABLE = ~ . +"TSCORES = t0-t24;",
      ANALYSIS = ~ "TYPE = MIXTURE RANDOM;,
        STARTS = 1000 250;,
        PROCESSORS = 8;",
      OUTPOUT = ~ "SAMPSTAT;",
      autov = FALSE,
      rdata = SAPS_df)

   .mpobj[["TITLE"]] <- str_replace(.mpobj[["TITLE"]], ";", "_TSCORES;")
   .mpobj[["MODEL"]] <- str_replace(.mpobj[["MODEL"]], "((\\n)?[:alpha:]+_(\\d)+@(\\d)+(\\s)?)+", "SAPS_0-SAPS_24 AT t0-t24") #use with str_view(0 or 1 new line(\n)); (one or more letters[:alpha:])_; (one or more digits(\d)); (0 or 1 new line(\s)); 1 or more times 
   .mpobj[["SAVEDATA"]] <- str_replace(.mpobj[["SAVEDATA"]], "_", "_TSCORES_") 
    
## Create, run, and read Mplus models 
   FitIndices_GBTM_tscores <- mplusModeler(
     object = .mpobj,
     dataout = glue(getwd(),'/SAPS/Results/TSCORES.dat'),
     modelout = glue(getwd(),'/SAPS/Results/TSCORES.inp'),
     hashfilename = FALSE,
     run = 0,
     check=TRUE,
     varwarnings = TRUE,
     writeData ="always"
   )
   
# Step 9: Add covariates  -------------------------------------------------
## Instructions 
### simple/univariate regression then multiple/multivariate regressions
### R3STEP: no need to list var as categorical
### missing on pred/Xs:
### MI: has to be 2 separate steps/inp. files (AUXILIARY = vars excluded from IMPUTE = data)
### can use the manual R3step, including a model for the covariates (instead of C on X; use C on X; X; )
### (see section 3;  http://statmodel.com/download/webnotes/webnote15.pdf)


## The 3-Step Procedure  
miss <- {unlist(lapply(SAPS_df, function(x) sum(is.na(x)))) / nrow(SAPS_df) * 100} %>% view()

### Auxilliary
### Manual
R3STEP_models <- R3STEP(
  df = SAPS_df,
  idvar = 'pin',
  usevar = 'SAPS',
  cov = SX_0,
  model = FINAL_model,
  manual_R3STEP = TRUE
)


R3STEP_fit <- fitR3STEP(R3STEP_models, SX_0, manual_R3STEP = TRUE)

# R3STEP_warn <- map(R3STEPm, pluck, 'results', 'warnings')
# R3STEP_err <- map(R3STEPm, pluck, 'results', 'errors')


# Save --------------------------------------------------------------------
save.image(glue(getwd(), 'SAPS', 'SAPS_{today()}.RData', .sep = "/"))

# SANS --------------------------------------------------------------------
# Step 0: Prepare dataset  ------------------------------------------------
## Load dataset 
SANS_df <- 
  paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_2022-05-02.csv') %>% 
  read_csv() %>%
  subset(miss_SANS <= 4 & n == 1) %>% 
  select ('pin', all_of(c(SX, SD_num, SD_cat, PSR, NSR, SR_C, SR_BY)))


## rename vars > 8 characters
SANS_df <- names(SANS_df) %>% 
  grep('.{9,}', ., value = TRUE) %>% 
  setnames(SANS_df, old = ., new = c('minority', 'housing', 'work'))

# names(SANS_df) <- str_sub(names(SANS_df), 1, 8) # remove characters from variables names > 8 characters 
# namesx2(PEPP2_df) # Function to check if column names are unique 
# namesx2 <- function(df) { 
#   
#   length1 <- length(colnames(df))
#   length2 <- length(unique(colnames(df)))        
#   if (length1 - length2 > 0 ) {
#     print(paste("There are", length1 - length2, " duplicates", sep=" "))
#   }     
# }
# anyDuplicated(colnames(PEPP2_df)) # locate column of the first duplicate)
# data.frame(colnames(PEPP2_df))


# Step 1: Growth Curve Modeling  ------------------------------------------
## Run GCM model
GCM_model <- runGCM(SANS_df, SANS, c(0, 1, 2, 3, 6, 9, 12, 18, 24))

## Get GCM model fit indices
GCM_fit <-
  GCM_model[["results"]][["parameters"]][["std.standardized"]] %>%
  filter(str_detect(paramHeader, 'Means|^Variances'))

# Step 2: Group-Based Trajectory Modeling ---------------------------------
## Run GBTM models 
GBTM_models <- fitGBTM(
  df = SANS_df,
  usevar = SANS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  working_dir = paste(getwd(), 'SANS', sep = '/'),
  max_k = 6)

## Get GBTM models fit indices 
GBTM_fit <- getFitIndices(GBTM_models) %>%
  mutate(BF10 = exp((GBTM_fit['#choose model to test','BIC']-GBTM_fit['#choose model to test','BIC'])/2))

## Select best GBTM model 
GBTM_best <- GBTM_models[[2]]
#GBTM_best <- selectBestModel(GBTM_models, selection_method = "BIC")

# Step 3: Latent Class Growth Analyses ------------------------------------
## Run LCGA models 
LCGA_models <- fitLCGA(
  df = SANS_df,
  usevar = SANS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  classes = 2,
  working_dir = paste(getwd(), 'SANS', sep = '/'),
  ref_model = GBTM_best)

## Get LCGA models fit indices 
LCGA_fit <- getFitIndices(LCGA_models) %>% 
  mutate(BF10 = exp((LCGA_fit['# choose model to test', 'BIC']-LCGA_fit['# choose model to test', 'BIC'])/2)) 

## Select best LCGA model 
LCGA_best <- selectBestModel(LCGA_models, selection_method = "BIC")


# Step 4: Growth Mixture Models  ------------------------------------------
## Add class-invariant random effect variances stepwise 
### Create Mplus Object 
.mpobj_2 <- list()
.mpobj_1 <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .mpobj_2[[gf]] <- update(
      LCGA_models[[rv]],
      TITLE = as.formula(glue("~ 'GMM{rv}i_{gf};'")),
      SAVEDATA = as.formula(glue("~ '
      file = GMM{rv}_{gf}_res.dat;
      save = CPROBABILITIES;'")),
      autov = FALSE,
      rdata = SANS_df)
    .mpobj_2[[gf]][["MODEL"]] <- str_replace(LCGA_models[[rv]][["MODEL"]], "i-cub@0", gf)
  }
  .mpobj_1[[rv]] <- .mpobj_2
}

### Create, run, and read Mplus models 
GMMi_models <- list()
.GMM <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .GMM[[gf]] <- mplusModeler(
      object = .mpobj_1[[rv]][[gf]],
      dataout = glue(getwd(), '/SANS/Results/GMMi/GMM{rv}/GMM{rv}_{gf}.dat'),
      modelout = glue(getwd(), '/SANS/Results/GMMi/GMM{rv}/GMM{rv}_{gf}.inp'),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )
  }
  GMMi_models[[rv]] <- .GMM
}

### Get Fit indices 
GMMi_fit <- SummaryTable(
  unlist(GMMi_models, FALSE),
  keepCols = c(
    "Title",
    "Parameters",
    "LL",
    "AIC",
    "AICC",
    "BIC",
    "Entropy",
    "T11_LMR_Value",
    "T11_LMR_PValue")) %>%
  mutate(CAIC = -2 * LL + Parameters * (log(405) + 1)) %>% 
  mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC']-GMMi_fit['# chose model to test', 'BIC'])/2)) 

## Add class-variant random effect variances stepwise 
### Create Mplus Object 
.mpobj_2 <- list()
.mpobj_1 <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .mpobj_2[[gf]] <- update(
      GMMi_models[[rv]][[gf]],
      TITLE = as.formula(glue("~ 'GMM{rv}v_{gf};'")),
      autov = FALSE,
      rdata = SANS_df)
    .mpobj_2[[gf]][["MODEL"]] <- str_replace_all(GMMi_models[[rv]][[gf]][["MODEL"]], "\\[i s q cub\\]", gf)
  }
  .mpobj_1[[rv]] <- .mpobj_2
}

### Create, run, and read Mplus models 
GMMv_models <- list()
.GMM <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .GMM[[gf]] <- mplusModeler(
      object = .mpobj_1[[rv]][[gf]],
      dataout = glue(getwd(), '/SANS/Results/GMMv/GMM{rv}/GMM{rv}_{gf}.dat'),
      modelout = glue(getwd(), '/SANS/Results/GMMv/GMM{rv}/GMM{rv}_{gf}.inp'),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )
  }
  GMMv_models[[rv]] <- .GMM
}

### Get Fit Indices 
GMMv_fit <- SummaryTable(
  unlist(GMMv_models, FALSE),
  keepCols = c(
    "Title",
    'LL',
    'Parameters',
    "AIC",
    "AICC",
    "BIC",
    "Entropy",
    "T11_LMR_Value",
    "T11_LMR_PValue")) %>% 
  mutate(CAIC = -2 * LL + Parameters * (log(405) + 1)) %>% 
  mutate(BF10 = exp((GMMi_fit['# chose model to test', 'BIC']-GMMi_fit['# chose model to test', 'BIC'])/2))


## Extract errors & warnings 
warnings <- map(unlist(c(GMMi_models, GMMv_models), FALSE), pluck, 'results', 'warnings')
errors <- map(unlist(c(GMMi_models, GMMv_models), FALSE), pluck, 'results', 'errors') %>% compact()

## Select best GMM model 
GMMi_best <- unlist(GMMi_models, FALSE) %>% selectBestModel(selection_method = "BIC_LRT")
GMMv_best <- selectBestModel(unlist(GMMv_models, FALSE), selection_method = "BIC_LRT")

GMMi_best <- GMMi_models[[4]][["i s q-cub@0"]] # because warnings
GMMv_best <- GMMv_models[[4]][["i s q-cub@0"]] # because warnings

## Get fit indices of all selected models and select best model 
BEST_fit <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% getFitIndices()
BEST_model <- list(GBTM_best, LCGA_best, GMMi_best, GMMv_best) %>% selectBestModel()

# Step 5: Refine Polynomial Order -----------------------------------------
FINAL_model <- refinePolynomial(
  model = BEST_model, 
  df = SANS_df,
  usevar = SANS,
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  working_dir = paste(getwd(), 'SANS', sep = '/'),
  idvar = "pin")

#FINAL_model <- BEST_model

## Examine fit indices 
FINAL_fit <- list(FINAL_model) %>% getFitIndices()

# Step 6: Extract model parameters and save results  ----------------------
## Get class counts & proportions of all models 
GBTM_cc <- GBTM_models %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>%  #extract2
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(GBTM_models, pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

LCGA_cc <- LCGA_models %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>%  #extract2
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(LCGA_models, pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

GMMi_cc <- unlist(GMMi_models, FALSE) %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>% 
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  mutate(model= map(unlist(GMMi_models, FALSE), pluck, 'TITLE')) %>% 
  select('model', starts_with(c('count', 'proportion')))

GMMv_cc <- unlist(GMMv_models, FALSE) %>% 
  map(pluck, 'results', 'class_counts', 'mostLikely') %>% 
  map_dfr( ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  # mutate(model= map(unlist(GMMv_models, FALSE), pluck, 'TITLE')) %>% 
  # select('model', starts_with(c('count', 'proportion')))
  
  # map_depth(2, pluck, 'results', 'class_counts', 'mostLikely') %>% 
  # map_if(is_null, ~ pivot_wider(.x, names_from = 'class', values_from = c('count', 'proportion'))) %>% 
  # map_if(is_null, mutate(model = map(unlist(GMMv_models, FALSE), pluck, 'TITLE')))
  # is_null <- function(x) {!is.null(x)}
  
  ## Get and save final dataset based on most probable class membership 
  PEPP2_df <- 
  FINAL_model[["results"]][["savedata"]] %>% 
  modify_at('C', as.factor) %>% 
  select(., -starts_with('SANS')) %>% 
  add_suffix(., everything(), suffix = 'SANS') %>% 
  merge(PEPP2_df, ., all = TRUE, by.x ='pin', by.y ='PIN_SANS')

#final_dataset <- getDataset(final_model, SANS_df, 'pin') 

# Step 7: Plot trajectories -----------------------------------------------
## Get means as long form
class_means <- getLongMeans(
  df = final_dataset,
  usevar = c('SANS_0', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  group_var = 'Class')

## Create line for observed symptoms
line2 <- geom_line(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class), 
  linetype = 'dashed')

## Create points for observed symptoms
point2 <- geom_point(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class, shape = Class))

## Plot final model with additional geoms for observed means
plotModel(
  model = final_model, 
  x_axis_label = 'Month', 
  y_axis_label = 'Symptoms', 
  geom_line2 = line2,
  geom_point2 = point2) + 
  scale_x_continuous(breaks = seq(0, 24, by = 3)) # Specify scale for asthetics

plotSANS_est <- 
  final_model[["results"]][["input"]][["data"]][["file"]] %>%
  strsplit('.dat') %>% 
  paste0('.gh5') %>% 
  mplus.plot.estimated_means()

#OR#

est.means <- 
  final_model[["results"]][["gh5"]][["means_and_variances_data"]][["y_estimated_medians"]][["values"]] %>%
  as.data.frame()

# Step 8: Using individually varying timescores ---------------------------
## Exclude cases with complete SANS and missing date of observation 
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

## Create an Mplus object 
.mpobj <- update(
  FINAL_model,
  VARIABLE = ~ . +"TSCORES = t0-t24;",
  ANALYSIS = ~ "TYPE = MIXTURE RANDOM;,
        STARTS = 1000 250;,
        PROCESSORS = 8;",
  OUTPOUT = ~ "SAMPSTAT;",
  autov = FALSE,
  rdata = SANS_df)

.mpobj[["TITLE"]] <- str_replace(.mpobj[["TITLE"]], ";", "_TSCORES;")
.mpobj[["MODEL"]] <- str_replace(.mpobj[["MODEL"]], "((\\n)?[:alpha:]+_(\\d)+@(\\d)+(\\s)?)+", "SANS_0-SANS_24 AT t0-t24") #use with str_view(0 or 1 new line(\n)); (one or more letters[:alpha:])_; (one or more digits(\d)); (0 or 1 new line(\s)); 1 or more times 
.mpobj[["SAVEDATA"]] <- str_replace(.mpobj[["SAVEDATA"]], "_", "_TSCORES_") 

## Create, run, and read Mplus models 
FitIndices_GBTM_tscores <- mplusModeler(
  object = .mpobj,
  dataout = glue(getwd(),'/SANS/Results/TSCORES.dat'),
  modelout = glue(getwd(),'/SANS/Results/TSCORES.inp'),
  hashfilename = FALSE,
  run = 0,
  check=TRUE,
  varwarnings = TRUE,
  writeData ="always"
)

# Step 9: Add covariates  -------------------------------------------------
## Instructions 
### simple/univariate regression then multiple/multivariate regressions
### R3STEP: no need to list var as categorical
### missing on pred/Xs:
### MI: has to be 2 separate steps/inp. files (AUXILIARY = vars excluded from IMPUTE = data)
### can use the manual R3step, including a model for the covariates (instead of C on X; use C on X; X; )
### (see section 3;  http://statmodel.com/download/webnotes/webnote15.pdf)


## The 3-Step Procedure  
miss <- {unlist(lapply(SANS_df, function(x) sum(is.na(x)))) / nrow(SANS_df) * 100} %>% view()

### Auxilliary
### Manual
R3STEP_models <- R3STEP(
  df = SANS_df,
  idvar = 'pin',
  usevar = 'SANS',
  cov = SX_0,
  model = FINAL_model,
  manual_R3STEP = TRUE
)


R3STEP_fit <- fitR3STEP(R3STEP_models, SX, manual_R3STEP = TRUE)

# R3STEP_warn <- map(R3STEPm, pluck, 'results', 'warnings')
# R3STEP_err <- map(R3STEPm, pluck, 'results', 'errors')


# Save --------------------------------------------------------------------
save.image(glue(getwd(), 'SAPS', 'SAPS_{today()}.RData', .sep = "/"))


# JUNK --------------------------------------------------------------------
## Run GCM (SAPS) 
### Create Mplus Object 
.GCM_model <- mplusObject(
  TITLE = 'GCM_P3_K1_S1000;',
  VARIABLE = c(
    'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24;',
    'MISSING=.;',
    'USEVAR = SAPS_0-SAPS_24;'),
  MODEL = 'i s q cub | SAPS_0@0 SAPS_1@1 SAPS_2@2 SAPS_3@3
    SAPS_6@6 SAPS_9@9 SAPS_12@12 SAPS_18@18 SAPS_24@24;',
  ANALYSIS = c(
    'TYPE = GENERAL;',
    'PROCESSORS = 8;'),
  OUTPUT = 'TECH1 SAMPSTAT STANDARDIZED;',
  SAVEDATA ='
    file = K1_S1000_res.dat;',
  autov = FALSE,
  rdata = SAPS_df 
)

### Create, run, and read Mplus models 
GCM_model <- mplusModeler(
  object = .GCM_model,
  dataout = str_c(getwd(),'/SAPS/Results/GCM/GCM_P3_K1.dat'),
  modelout = str_c(getwd(),'/SAPS/Results/GCM/GCM_P3_K1.inp'),
  hashfilename = FALSE,
  run = 1,
  check=FALSE,
  writeData ="always"
)

## Run GMMi models (SAPS) 
### Create Mplus Object 
.mpobj_2 <- list()
.mpobj_1 <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .mpobj_2[[gf]] <- update(
      LCGA_models[[rv]],
      TITLE = as.formula(glue("~ 'GMM{rv}i_{gf};'")),
      SAVEDATA = as.formula(glue("~ '
      file = GMM{rv}_{gf}_res.dat;
      save = CPROBABILITIES;'")),
      autov = FALSE,
      rdata = SAPS_df)
    .mpobj_2[[gf]][["MODEL"]] <- str_replace(LCGA_models[[rv]][["MODEL"]], "i-cub@0", gf)
  }
  .mpobj_1[[rv]] <- .mpobj_2
}

### Create, run, and read Mplus models 
GMMi_models <- list()
.GMM <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .GMM[[gf]] <- mplusModeler(
      object = .mpobj_1[[rv]][[gf]],
      dataout = glue(getwd(), '/SAPS/Results/GMMi/GMM{rv}/GMM{rv}_{gf}.dat'),
      modelout = glue(getwd(), '/SAPS/Results/GMMi/GMM{rv}/GMM{rv}_{gf}.inp'),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )
  }
  GMMi_models[[rv]] <- .GMM
}

## Run GMMv models (SAPS) 
### Create Mplus Object 
.mpobj_2 <- list()
.mpobj_1 <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .mpobj_2[[gf]] <- update(
      GMMi_models[[rv]][[gf]],
      TITLE = as.formula(glue("~ 'GMM{rv}v_{gf};'")),
      autov = FALSE,
      rdata = SAPS_df)
    .mpobj_2[[gf]][["MODEL"]] <- str_replace_all(GMMi_models[[rv]][[gf]][["MODEL"]], "\\[i s q cub\\]", gf)
  }
  .mpobj_1[[rv]] <- .mpobj_2
}

### Create, run, and read Mplus models 
GMMv_models <- list()
.GMM <- list()
for (rv in 1:4) {
  for (gf in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    .GMM[[gf]] <- mplusModeler(
      object = .mpobj_1[[rv]][[gf]],
      dataout = glue(getwd(), '/SAPS/Results/GMMv/GMM{rv}/GMM{rv}_{gf}.dat'),
      modelout = glue(getwd(), '/SAPS/Results/GMMv/GMM{rv}/GMM{rv}_{gf}.inp'),
      hashfilename = FALSE,
      run = 1,
      check = TRUE,
      varwarnings = TRUE,
      writeData = "always"
    )
  }
  GMMv_models[[rv]] <- .GMM
}

## Run individual GMM models (SAPS) 
### GMM1 - equality across t and k 
GMM1_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  GMM1_models[[g]] <- mplusObject(
    TITLE = glue('GMM1_{g};'),
    VARIABLE = c(
      'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24;',
      'MISSING=.;',
      'USEVAR = SAPS_0-SAPS_24;',
      'CLASSES = c(2);'),
    MODEL = c(
      '%OVERALL%',
      'i s q cub | SAPS_0@0 SAPS_1@1 SAPS_2@2 SAPS_3@3 SAPS_6@6 
                   SAPS_9@9 SAPS_12@12 SAPS_18@18 SAPS_24@24;',
      glue('{g};'),
      'SAPS_0-SAPS_24 (1);'),
    ANALYSIS = c(
      'TYPE = MIXTURE;',
      'STARTS = 1000 250;',
      'PROCESSORS = 16;'),
    OUTPUT = 'SAMPSTAT STANDARDIZED TECH1 TECH11;',
    SAVEDATA = glue('file = GMM1_{g}_res.dat;'),
    autov = FALSE,
    rdata = SAPS_df 
  )
}

#### Create, run, and read Mplus models
FitIndices_GMM1 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  
  FitIndices_GMM1[[g]] <- mplusModeler(
    object = GMM1_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM1/GMM1_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM1/GMM1_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}

### GMM2 - equality across t/free across k 
GMM2_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  GMM2_models[[g]] <- mplusObject(
    TITLE = glue('GMM2_{g};'),
    VARIABLE = c(
      'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24;',
      'MISSING=.;',
      'USEVAR = SAPS_0-SAPS_24;',
      'CLASSES = c(2);'),
    MODEL = c(
      '%OVERALL%',
      'i s q cub | SAPS_0@0 SAPS_1@1 SAPS_2@2 SAPS_3@3 SAPS_6@6 
                   SAPS_9@9 SAPS_12@12 SAPS_18@18 SAPS_24@24;',
      glue('{g};'),
      
      '%c#1%',
      'SAPS_0-SAPS_24 (1);',
      
      '%c#2%',
      'SAPS_0-SAPS_24 (2);'),
    ANALYSIS = c(
      'TYPE = MIXTURE;',
      'STARTS = 1000 250;',
      'PROCESSORS = 16;'),
    OUTPUT = 'SAMPSTAT;',
    SAVEDATA = glue('file = GMM2_{g}_res.dat;'),
    autov = FALSE,
    rdata = SAPS_df 
  )
}

#### Create, run, and read Mplus models
FitIndices_GMM2 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  
  FitIndices_GMM2[[g]] <- mplusModeler(
    object = GMM2_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM2/GMM2_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM2/GMM2_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}

### GMM3 - equality across k/free across t 
GMM3_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  GMM3_models[[g]] <- mplusObject(
    TITLE = glue('GMM3_{g};'),
    VARIABLE = c(
      'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24;',
      'MISSING=.;',
      'USEVAR = SAPS_0-SAPS_24;',
      'CLASSES = c(2);'),
    MODEL = c(
      '%OVERALL%',
      'i s q cub | SAPS_0@0 SAPS_1@1 SAPS_2@2 SAPS_3@3 SAPS_6@6 
                   SAPS_9@9 SAPS_12@12 SAPS_18@18 SAPS_24@24;',
      glue('{g};'),
      'SAPS_0 (1)
      SAPS_1 (2)
      SAPS_2 (3)
      SAPS_3 (4)
      SAPS_6 (5)
      SAPS_9 (6)
      SAPS_12 (7)
      SAPS_18 (8)
      SAPS_24 (9);'),
    ANALYSIS = c(
      'TYPE = MIXTURE;',
      'STARTS = 1000 250;',
      'PROCESSORS = 16;'),
    OUTPUT = 'SAMPSTAT;',
    SAVEDATA = glue('file = GMM3_{g}_res.dat;'),
    autov = FALSE,
    rdata = SAPS_df 
  )
}

### Create, run, and read Mplus models 
FitIndices_GMM3 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  FitIndices_GMM3[[g]] <- mplusModeler(
    object = GMM3_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM3/GMM3_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM3/GMM3_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}

### GMM4 - free across t and k 
GMM4_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  GMM4_models[[g]] <- mplusObject(
    TITLE = glue('GMM4_{g};'),
    VARIABLE = c(
      'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24;',
      'MISSING=.;',
      'USEVAR = SAPS_0-SAPS_24;',
      'CLASSES = c(2);'),
    MODEL = c(
      '%OVERALL%',
      'i s q cub | SAPS_0@0 SAPS_1@1 SAPS_2@2 SAPS_3@3 SAPS_6@6 
               SAPS_9@9 SAPS_12@12 SAPS_18@18 SAPS_24@24;',
      glue('{g};'),
      
      '%c#1%
  SAPS_0 (1)
  SAPS_1 (2)
  SAPS_2 (3)
  SAPS_3 (4)
  SAPS_6 (5)
  SAPS_9 (6)
  SAPS_12 (7)
  SAPS_18 (8)
  SAPS_24 (9);',
  
  '%c#2%
  SAPS_0 (10)
  SAPS_1 (11)
  SAPS_2 (12)
  SAPS_3 (13)
  SAPS_6 (14)
  SAPS_9 (15)
  SAPS_12 (16)
  SAPS_18 (17)
  SAPS_24 (18);'),
  ANALYSIS = c(
    'TYPE = MIXTURE;',
    'STARTS = 1000 250;',
    'PROCESSORS = 16;'),
  OUTPUT = 'SAMPSTAT;',
  SAVEDATA = glue('file = GMM4_{g}_res.dat;'),
  autov = FALSE,
  rdata = SAPS_df 
  )
}

#### Create, run, and read Mplus models 
FitIndices_GMM4 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  FitIndices_GMM4 <- mplusModeler(
    object = GMM4_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM4/GMM4_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM4/GMM4_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
} 

## TSCORES for all GBTM models (SAPS) 
GBTM_tscores_models <- list()
for (k in 1:3) {
  GBTM_tscores_models[[k]] <- mplusObject(
    TITLE = glue('GBTM_K{k}_TSCORES;'),
    VARIABLE = c(
      'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24
             t0 t1 t2 t3 t6 t9 t12 t18 t24;',
      'MISSING=.;',
      'USEVAR = SAPS_0-SAPS_24 t0-t24;',
      'TSCORES = t0-t24;', 
      glue('CLASSES = c({k});')),
    MODEL = c(
      '%OVERALL%',
      'i s q cub | SAPS_0-SAPS_24 AT t0-t24;',
      'i-cub@0;',
      'SAPS_0-SAPS_24 (1);'),
    ANALYSIS = c(
      'TYPE = MIXTURE RANDOM;',
      'STARTS = 1000 250;',
      'PROCESSORS = 16;'),
    OUTPUT = 'SAMPSTAT;',
    SAVEDATA = glue('file = K{k}_TSCORES_res.dat;'),
    autov = FALSE,
    rdata = SAPS_df 
  )}

## Create, run, and read Mplus models 
FitIndices_GBTM_tscores <- list()
for (k in 1:3) {
  
  FitIndices_GBTM_tscores <- mplusModeler(
    object = GBTM_tscores_models[[k]],
    dataout = glue(getwd(),'/SAPS/Results/GBTM/GBTM_K{k}_TSCORES.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GBTM/GBTM_K{k}_TSCORES.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}


