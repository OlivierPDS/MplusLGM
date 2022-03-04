# MplusLGM

## Installation

# Install devtools from CRAN if not already installed 
install.packages("devtools")

# Install the Bioconductor package manager if required, then the rhdf5 package
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("rhdf5")

# Install the latest version of MplusLGM from this repository
devtools::install_github("joshunrau/MplusLGM")


### Step 1: Load the Package and Dataset

# Load required packages
library(MplusLGM)
library(tidyverse)
library(haven)
library(MplusAutomation)

# Load dataset
SAPS_df <- 
  paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_2022-02-28.sav') %>% 
  read_spss(user_na = FALSE, skip = 0, n_max = Inf) %>%
  subset(miss_SAPS <= 4 & n == 1) %>% 
  select ('pin', all_of(c(SAPS)))


# Examine the structure of the dataset
str(df)

# Get means for each diagnostic group at variables of interest
df %>% group_by(dx) %>% 
  summarise_at(vars(colnames(df)[3:9]), mean, na.rm = TRUE)


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
FitIndices_gbtm <- getFitIndices(gbtm_models)

# class count should not be < 25% sample size

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

FitIndices_final_model <- final_model %>% list() %>% getFitIndices()

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

### Step 5: Plot trajectories

# Get and save final dataset based on most probable class membership
final_dataset <- getDataset(final_model, SAPS405_df, 'pin')

data <- data.frame(final_model[["results"]][["savedata"]]) %>% rename(pin = PIN)
final_dataset <- merge(SAPS405_df, data, by = 'pin') %>% select('pin','C', starts_with('CPROB'))
write_sav(final_dataset, paste(getwd(), 'SAPS', 'SAPS405.sav', sep = '/'))

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

#[Alt Text](https://github.com/joshunrau/MplusLGM/blob/main/example/adv_plot.png?raw=true)


### Using individually varying timescores  

SAPS_df <- SAPS_df %>% select ('pin', all_of(c(SAPS, t)))

# Exclude cases with complete SAPS and missing date of observation
SAPS_df <- SAPS_df %>%
  filter(xor(!is.na(SAPS_1), is.na(t1))) %>% 
  filter(xor(!is.na(SAPS_2), is.na(t2))) %>% 
  filter(xor(!is.na(SAPS_3), is.na(t3))) %>% 
  filter(xor(!is.na(SAPS_6), is.na(t6))) %>% 
  filter(xor(!is.na(SAPS_9), is.na(t9))) %>% 
  filter(xor(!is.na(SAPS_12), is.na(t12))) %>% 
  filter(xor(!is.na(SAPS_18), is.na(t18))) %>% 
  filter(xor(!is.na(SAPS_24), is.na(t24)))

#map <- map2(x, y, ~filter(xor(!is.na(x), is.na(y)))) # Does not work

# Should try to update input file from best model with update() 

# Create an Mplus model object
m <- mplusObject(
  TITLE = 'RealTime;',
  VARIABLE = c(
    'NAMES = pin SAPS_0 SAPS_1 SAPS_2 SAPS_3 SAPS_6 SAPS_9 SAPS_12 SAPS_18 SAPS_24
             t0 t1 t2 t3 t6 t9 t12 t18 t24;',
    'MISSING=.;',
    'USEVAR = SAPS_0-SAPS_24 t0-t24;',
    'TSCORES = t0-t24;', 
    'CLASSES = c(2);'),
  MODEL = c(
    '%OVERALL%',
    'i s q cub | SAPS_0-SAPS_24 AT t0-t24;',
    'i-cub@0;',
    'SAPS_0-SAPS_24 (1);'),
  ANALYSIS = c(
    'TYPE = MIXTURE RANDOM;',
    'STARTS = 500 125;',
    'PROCESSORS = 16;'),
  OUTPUT = 'SAMPSTAT;',
  autov = FALSE,
  rdata = SAPS_df 
)

#Separate steps
# Create, run, and read Mplus models
m_fit <- mplusModeler(
  object = m,
  dataout = str_c(getwd(),'/MplusfromR/GBTM.dat'),
  modelout = str_c(getwd(),'/MplusfromR/GBTM.inp'),
  hashfilename = FALSE,
  run = 1,
  check=TRUE,
  varwarnings = TRUE,
  writeData ="always"
)

# Create tab-delimited file and Mplus input syntax from R data.frame
prepareMplusData(
  SAPS_df,
  filename = str_c(getwd(),'/MplusfromR/K2_S500.dat'),
  dropCols = t,
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
runModels('/Users/olivierpercie/Desktop/MplusLGM/MplusfromR/K2_S500.inp')


### Add covariates

SAPScov_df <- PEPP2_df %>% select ('pin', all_of(SAPS), 'FIQ')


model_cov <- update(final_model,
                    VARIABLE = ~ . + 'AUXILARY = (R3STEP) FIQ;',
                    ANALYSIS = ~ . + 'PROCESSORS = 16;',
                    rdata = SAPScov_df,
                    )




