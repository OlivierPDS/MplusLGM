# MplusLGM

## Installation

# Install devtools from CRAN if not already installed 
install.packages("devtools")
library(devtools)

# Install the Bioconductor package manager if required, then the rhdf5 package
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("rhdf5")

# Install the latest version of MplusLGM from this repository
devtools::install_github("joshunrau/MplusLGM", force = TRUE)


### Step 1: Load the Package and Dataset

# Load required packages
library(MplusLGM)
library(tidyverse)
library(haven)
library(MplusAutomation)
library(magrittr)

# Load dataset

spss.sav = paste(getwd(), 'data', 'dataset533_03Sept2021.sav', sep = '/')
dataset_df <- read_spss(spss.sav, user_na = FALSE, skip = 0, n_max = Inf)
SOFAS_df <- dataset_df[, c('pin', 'SOFAS_B', 'SOFAS_12', 'SOFAS_24','miss_SOFAS')]
SOFAS345_df <- subset(SOFAS_df, miss_SOFAS <= 1)

# Examine the structure of the dataset
str(SOFAS345_df)

# Get means for each diagnostic group at variables of interest
dataset_df %>% group_by('K_SOFAS') %>% 
  summarise_at(vars(colnames(dataset_df)[3:9]), mean, na.rm = TRUE)


### Step 2: Group-Based Trajectory Modeling

# Run GBTM models
gbtm_models <- fitGBTM(
  df = SOFAS345_df,
  usevar = c('SOFAS_B', 'SOFAS_12', 'SOFAS_24'),
  timepoints = c(0, 12, 24),
  idvar = "pin",
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  max_k = 6,
  overall_polynomial = 2)

# Examine fit indices
FitIndices_gbtm <- getFitIndices(gbtm_models)

best_gbtm_model <- selectBestModel(gbtm_models, selection_method = "BIC_LRT")
#best_gbtm_model <- gbtm_models[[2]]

### Step 3: Examine Alternative Variance Structures

# Run LCGA models
lcga_models <- fitLCGA(
  df = SOFAS345_df,
  usevar = c('SOFAS_B', 'SOFAS_12', 'SOFAS_24'),
  timepoints = c(0, 12, 24),
  idvar = "pin",
  classes = 2,
  overall_polynomial = 2,
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  ref_model = best_gbtm_model)

# Examine fit indices
FitIndices_lcga <- getFitIndices(lcga_models)

# Here, we will select the best model based only on the BIC.
best_bic_model <- selectBestModel(lcga_models, selection_method = "BIC")

### Step 4: Refine Polynomial Order

final_model <- refinePolynomial(
  model = best_bic_model, 
  df = SOFAS345_df,
  usevar = c('SOFAS_B', 'SOFAS_12', 'SOFAS_24'),
  timepoints = c(0, 12, 24),
  classes = 2,
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
  idvar = "pin")

FitIndices_final_model <- final_model %>% list() %>% getFitIndices()

#Get class counts & proportions of all models (to improve)
library(plyr)
library(readxl)

alloutput <- readModels(
  paste(getwd(), 'SOFAS', sep = '/'),
  what="all",
  recursive=TRUE)
class_counts <- get_class_counts(alloutput, simplify = FALSE)
mostlikely <- do.call("rbind.fill", sapply(class_counts,"[", "mostLikely"))
write_excel_csv(mostlikely, paste(getwd(), 'SOFAS', 'SOFASclass_counts', sep = '/'))
SOFASclass_counts <- read_excel(paste(getwd(), 'SOFAS', 'SOFASclass_counts', sep = '/'))
wide_mostlikely <- pivot_wider(xl, names_from = 'class', values_from = c('count', 'proportion'))

# Get final dataset based on most probable class membership
#final_dataset <- getDataset(final_model, SOFAS345_df, 'pin')

data <- data.frame(final_model[["results"]][["savedata"]]) %>% rename(pin = PIN)
final_dataset <- merge(SOFAS345_df, data, by = 'pin') %>% select('pin','C', starts_with('CPROB'))
write_sav(final_dataset, paste(getwd(), 'SOFAS', 'SOFAS345.sav', sep = '/'))

### Step 5: Follow-Up Analyses

# Get means as long form
class_means <- getLongMeans(
  df = final_dataset,
  usevar = c('SOFAS_B', 'SOFAS_12', 'SOFAS_24'),
  timepoints = c(0, 12, 24),
  working_dir = paste(getwd(), 'SOFAS', sep = '/'),
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
  y_axis_label = 'Symptoms') +
  #geom_line2 = line2,
  #geom_point2 = point2) 
  scale_x_continuous(breaks = seq(0, 24, by = 3)) # Specify scale for asthetics

source('/Users/olivierpercie/Desktop/MplusLGM/R/mplus.R')
library(rhdf5)

plotSOFAS_est <- 
  final_model[["results"]][["input"]][["data"]][["file"]] %>%
    strsplit('.dat') %>% 
      paste0('.gh5') %>% 
        mplus.plot.estimated_means()

#OR#

est.means <- 
  final_model[["results"]][["gh5"]][["means_and_variances_data"]][["y_estimated_medians"]][["values"]] %>%
  as.data.frame()

#[Alt Text](https://github.com/joshunrau/MplusLGM/blob/main/example/adv_plot.png?raw=true)