### Installation
# Install devtools from CRAN if not already installed 
install.packages("devtools")

# Install the Bioconductor package manager if required, then the rhdf5 package
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("rhdf5")

# Install the latest version of MplusLGM from this repository
devtools::install_github("joshunrau/MplusLGM")

### Step 0: Load the Package and Dataset
# Load required packages
library(MplusLGM)
library(tidyverse)
library(haven)
library(MplusAutomation)
library(magrittr)
library(glue)
library(lubridate)

# Some Mplus Language:
    # vars: refers to variances and residual variances; ex: var1 var1-var9;
    # [vars]: refers to means, intercepts, thresholds; ex: [var1, var1-var9];
    # *: frees a parameter at a default value or a specific starting value; ex:  var1* var2*.5;
    # @: fixes a parameter at a default value or a specific value; ex: var1@ var2@0;
    # (number): constrains parameters to be equal var1(1) var2(1)

# Load dataset
SAPS <- str_c('SAPS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
t <-  str_c('t', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

SAPS_df <- 
  paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_2022-03-11.csv') %>% 
  read_csv() %>%
  subset(miss_SAPS <= 4 & n == 1) %>% 
  select ('pin', all_of(c(SAPS, t)))

# Get means for each diagnostic group at variables of interest 
df %>% group_by(dx) %>% 
  summarise_at(vars(colnames(df)[3:9]), mean, na.rm = TRUE)


### Step 1: Growth Curve Modeling

# s@0; !set slope factor variance to 0
# [s@0]; !set slope factor mean to 0
# i with s@0; !set growth factor covariance to 0
# vars (1); fix residual variance

#Run GCM model
gcm_models <- mplusObject(
  TITLE = 'GCM_P3_K1_S1000;',
  VARIABLE = c(
    'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24;',
    'MISSING=.;',
    'USEVAR = SAPS_0-SAPS_24;'),
  MODEL = 'i s q cub | SAPS_0@0 SAPS_1@1 SAPS_2@2 SAPS_3@3
    SAPS_6@6 SAPS_9@9 SAPS_12@12 SAPS_18@18 SAPS_24@24;',
  ANALYSIS = c(
    'TYPE = GENERAL;',
    'PROCESSORS = 16;'),
  OUTPUT = 'TECH1 SAMPSTAT STANDARDIZED;',
  SAVEDATA ='
    file = K1_S500_res.dat;',
  autov = FALSE,
  rdata = SAPS_df 
)

# Create, run, and read Mplus models
FitIndices_gcm <- mplusModeler(
  object = gcm_models,
  dataout = str_c(getwd(),'/SAPS/Results/GCM/GCM_P3_K1.dat'),
  modelout = str_c(getwd(),'/SAPS/Results/GCM/GCM_P3_K1.inp'),
  hashfilename = FALSE,
  run = 1,
  check=FALSE,
  writeData ="always"
)

#add GCM fit indices

### Step 2: Group-Based Trajectory Modeling
# Run GBTM models
gbtm_models <- fitGBTM(
  df = SAPS_df,
  usevar = c('SAPS_0', 'SAPS_1', 'SAPS_2', 'SAPS_3', 'SAPS_6', 'SAPS_9', 'SAPS_12', 'SAPS_18', 'SAPS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  max_k = 6)

# Examine fit indices
# - N > 176:  CAIC > BIC > aBIC > AIC
# - entropy > 0.8: CAIC & BIC > aBIC & BLRT  
# - class count should not be < 25% sample size
# - H0:K=K1-1 H1:K=K1 If likelihood ratio p<0.05 then choose K1, else choose K1-1

FitIndices_gbtm <- getFitIndices(gbtm_models)

#best_gbtm_model <- selectBestModel(gbtm_models, selection_method = "BIC_LRT") # to improve
best_gbtm_model <- gbtm_models[[2]]

### Step 3: Examine Alternative Variance Structures
# Run LCGA models
lcga_models <- fitLCGA(
  df = SAPS_df,
  usevar = c('SAPS_0', 'SAPS_1', 'SAPS_2', 'SAPS_3', 'SAPS_6', 'SAPS_9', 'SAPS_12', 'SAPS_18', 'SAPS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  classes = 2,
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  ref_model = best_gbtm_model)

# Examine fit indices
FitIndices_lcga <- getFitIndices(lcga_models)

# Here, we will select the best model based only on the BIC.
best_bic_model <- selectBestModel(lcga_models, selection_method = "BIC")

### Step 4: Refine Polynomial Order
final_model <- refinePolynomial(
  model = best_bic_model, 
  df = SAPS_df,
  usevar = c('SAPS_0', 'SAPS_1', 'SAPS_2', 'SAPS_3', 'SAPS_6', 'SAPS_9', 'SAPS_12', 'SAPS_18', 'SAPS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  working_dir = paste(getwd(), 'SAPS', sep = '/'),
  idvar = "pin")

# Examine fit indices
FitIndices_final_model <- final_model %>% list() %>% getFitIndices()

### Step 5: Extract model parameters and save results

#Get class counts & proportions of all models (to improve)
library(plyr)
library(readxl)

alloutput <- readModels(
  paste(getwd(), 'SAPS', sep = '/'),
  what="all",
  recursive=TRUE)
class_counts <- get_class_counts(alloutput, simplify = FALSE)
mostlikely <- do.call("rbind.fill", sapply(class_counts,"[", "mostLikely"))
write_excel_csv(mostlikely, paste(getwd(), 'SAPS', 'SAPSclass_counts', sep = '/'))
SAPSclass_counts <- read_excel(paste(getwd(), 'SAPS', 'SAPSclass_counts', sep = '/'))
wide_mostlikely <- pivot_wider(xl, names_from = 'class', values_from = c('count', 'proportion'))

# Get and save final dataset based on most probable class membership
final_dataset <- getDataset(final_model, SAPS405_df, 'pin')

data <- data.frame(final_model[["results"]][["savedata"]]) %>% rename(pin = PIN)
final_dataset <- merge(SAPS405_df, data, by = 'pin') %>% select('pin','C', starts_with('CPROB'))
write_sav(final_dataset, paste(getwd(), 'SAPS', 'SAPS405.sav', sep = '/'))

### Step 6: Plot trajectories

# Get means as long form
class_means <- getLongMeans(
  df = final_dataset,
  usevar = c('SAPS_0', 'SAPS_1', 'SAPS_2', 'SAPS_3', 'SAPS_6', 'SAPS_9', 'SAPS_12', 'SAPS_18', 'SAPS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  group_var = 'Class')

# Create line for observed symptoms
line2 <- geom_line(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class), 
  linetype = 'dashed')

# Create points for observed symptoms
point2 <- geom_point(
  data = class_means, 
  aes(x = Time, y = Variable, group = Class, color=Class, shape = Class))

# Plot final model with additional geoms for observed means
plotModel(
  model = final_model, 
  x_axis_label = 'Month', 
  y_axis_label = 'Symptoms', 
  geom_line2 = line2,
  geom_point2 = point2) + 
  scale_x_continuous(breaks = seq(0, 24, by = 3)) # Specify scale for asthetics

source('/Users/olivierpercie/Desktop/MplusLGM/R/mplus.R')
library(rhdf5)
library(magrittr)

plotSAPS_est <- 
  final_model[["results"]][["input"]][["data"]][["file"]] %>%
    strsplit('.dat') %>% 
      paste0('.gh5') %>% 
        mplus.plot.estimated_means()

#OR#

est.means <- 
  final_model[["results"]][["gh5"]][["means_and_variances_data"]][["y_estimated_medians"]][["values"]] %>%
  as.data.frame()

### Step x: Growth Mixture Models
  # GMM1 - equality across t and k 
gmm1_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  gmm1_models[[g]] <- mplusObject(
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

# Create, run, and read Mplus models
FitIndices_gmm1 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  
  FitIndices_gmm1[[g]] <- mplusModeler(
    object = gmm1_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM1/GMM1_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM1/GMM1_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}

  # GMM2 - equality across t/free across k 
gmm2_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  gmm2_models[[g]] <- mplusObject(
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

# Create, run, and read Mplus models
FitIndices_gmm2 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  
  FitIndices_gmm2[[g]] <- mplusModeler(
    object = gmm2_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM2/GMM2_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM2/GMM2_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}

  # GMM3 - equality across k/free across t 
gmm3_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  gmm3_models[[g]] <- mplusObject(
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

# Create, run, and read Mplus models 
FitIndices_gmm3 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  FitIndices_gmm3[[g]] <- mplusModeler(
    object = gmm3_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM3/GMM3_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM3/GMM3_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}

  # GMM4 - free across t and k 
gmm4_models <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
  gmm4_models[[g]] <- mplusObject(
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

# Create, run, and read Mplus models
FitIndices_gmm4 <- list()
for (g in c('i s-cub@0', 'i s q-cub@0', 'i s q cub@0', 'i s q cub')) {
    FitIndices_gmm4 <- mplusModeler(
    object = gmm4_models[[g]],
    dataout = glue(getwd(),'/SAPS/Results/GMM/GMM4/GMM4_{g}.dat'),
    modelout = glue(getwd(),'/SAPS/Results/GMM/GMM4/GMM4_{g}.inp'),
    hashfilename = FALSE,
    run = 1,
    check=TRUE,
    varwarnings = TRUE,
    writeData ="always"
  )
}
  
### Step 7: Using individually varying timescores 
  # Exclude cases with complete SAPS and missing date of observation 
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
  # Create an Mplus model object - Should try to update input file from best model with update() 
gbtm_tscores_models <- list()
for (k in 1:3) {
gbtm_tscores_models[[k]] <- mplusObject(
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
  # Create, run, and read Mplus models 
FitIndices_gbtm_tscores <- list()
for (k in 1:3) {
  
FitIndices_gbtm_tscores <- mplusModeler(
  object = gbtm_tscores_models[[k]],
  dataout = glue(getwd(),'/SAPS/Results/GBTM/GBTM_K{k}_TSCORES.dat'),
  modelout = glue(getwd(),'/SAPS/Results/GBTM/GBTM_K{k}_TSCORES.inp'),
  hashfilename = FALSE,
  run = 1,
  check=TRUE,
  varwarnings = TRUE,
  writeData ="always"
)
}

# --------------- #
# MplusAutomation #
# --------------- #

### Prepare dataset
  # remove characters from variables names > 8 characters 
names(PEPP2_df) <- str_sub(names(PEPP2_df), 1, 8) 
namesx2(PEPP2_df) # Function to check if column names are unique 
namesx2 <- function(df) { 
  
  length1 <- length(colnames(df))
  length2 <- length(unique(colnames(df)))        
  if (length1 - length2 > 0 ) {
    print(paste("There are", length1 - length2, " duplicates", sep=" "))
  }     
}
anyDuplicated(colnames(PEPP2_df)) # locate column of the first duplicate)
data.frame(colnames(PEPP2_df))

  # Create tab-delimited file and Mplus input syntax from R data.frame
prepareMplusData(
  SAPScov_df,
  filename = str_c(getwd(),'/MplusfromR/cov.dat'),
  inpfile = FALSE,
  writeData = "always"
)

  # Create the Mplus input text for an mplusObject 
inp <- createSyntax(
  object = m, 
  filename = str_c(getwd(),'/MplusfromR/GBTM.inp'),
  check = FALSE,
  add = FALSE) %>% 
writeLines(str_c(getwd(),'/MplusfromR/GBTM.inp'))

  # Run Mplus Models
runModels('/Users/olivierpercie/Desktop/MplusLGM/MplusfromR/cov.inp')

### Add covariates
SAPScov_df <- PEPP2_df %>% 
  subset(miss_SAPS <= 4 & n == 1) %>% 
  select ('pin', all_of(SAPS), 'K_SAPS', 'FIQ') 

# simple/univariate regression then multiple/multivariate 
# missing on pred/Xs:
#   - MI: has to be 2 separate steps/inp. files (AUXILIARY = vars excluded from IMPUTE = data)
#   - add variance to model in manual 3 steps method 

# cont
cov_model <- update(final_model,
VARIABLE = ~'
USEVAR = SAPS_0-SAPS_24 ageentry;
CLASSES = c(2);
AUXILIARY = (R3STEP) ageentry;',
ANALYSIS = ~'
TYPE = MIXTURE;
PROCESSORS = 16;',
OUTPUT = ~'
SAMPSTAT
STANDARDIZED
TECH1;',
usevariables = names(SAPScov_df),
rdata = df
)

cov_fit <- mplusModeler(
  object = cov_model,
  dataout = str_c(getwd(),'/MplusfromR/cov.dat'),
  modelout = str_c(getwd(),'/MplusfromR/cov.inp'),
  hashfilename = FALSE,
  run = 1,
  check=TRUE,
  varwarnings = TRUE,
  writeData = 'never'
)


save.image(glue(getwd(), 'SAPS', 'SAPS_{today()}.RData', .sep = "/"))
