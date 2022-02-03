# MplusLGM

## Installation

# Install devtools from CRAN if not already installed 
install.packages("devtools")
library(devtools)
library(magrittr)

# Install the Bioconductor package manager if required, then the rhdf5 package
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("rhdf5")

# Install the latest version of MplusLGM from this repository
devtools::install_github("joshunrau/MplusLGM", force=TRUE)

### Step 1: Load the Package and Dataset

# Load required packages
library(MplusLGM)
library(tidyverse)
library(haven)

# Load dataset

spss.sav = paste(getwd(), 'data', 'Dataset533_03Sept2021.sav', sep = '/')
dataset_df <- read_spss(spss.sav, user_na = FALSE, skip = 0, n_max = Inf)
SANS_df <- dataset_df[, c('pin', 'SANS_B', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24','miss_SANS')]
SANS405_df <- subset(SANS_df, miss_SANS <= 4)

# Examine the structure of the dataset
str(SANS405_df)

### Step 2: Group-Based Trajectory Modeling

# Run GBTM models
gbtm_models <- fitGBTM(
  df = SANS405_df,
  usevar = c('SANS_B', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  working_dir = paste(getwd(), 'SANS', sep = '/'),
  max_k = 6)

# Examine fit indices
FitIndices_gbtm <- getFitIndices(gbtm_models)

#best_gbtm_model <- selectBestModel(gbtm_models, selection_method = "BIC_LRT")
best_gbtm_model <- gbtm_models[[3]]

### Step 3: Examine Alternative Variance Structures

# Run LCGA models
lcga_models <- fitLCGA(
  df = SANS405_df,
  usevar = c('SANS_B', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  idvar = "pin",
  classes = 3,
  working_dir = paste(getwd(), 'SANS', sep = '/'),
  ref_model = best_gbtm_model)

# Examine fit indices
FitIndices_lcga <- getFitIndices(lcga_models)

# Here, we will select the best model based only on the BIC.
best_bic_model <- selectBestModel(lcga_models, selection_method = "BIC")

### Step 4: Refine Polynomial Order

final_model <- refinePolynomial(
  model = best_bic_model, 
  df = SANS405_df,
  usevar = c('SANS_B', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  working_dir = paste(getwd(), 'SANS', sep = '/'),
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

### Step 5: Plot trajectories

# Get final dataset based on most probable class membership
final_dataset <- getDataset(final_model, SANS405_df, 'pin')

data <- data.frame(final_model[["results"]][["savedata"]]) %>% rename(pin = PIN)
final_dataset <- merge(SANS405_df, data, by = 'pin') %>% select('pin','C', starts_with('CPROB'))
write_sav(final_dataset, paste(getwd(), 'SANS', 'SANS405.sav', sep = '/'))

# Get means as long form
class_means <- getLongMeans(
  df = final_dataset,
  usevar = c('SANS_B', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24'),
  timepoints = c(0, 1, 2, 3, 6, 9, 12, 18, 24),
  #working_dir = paste(getwd(), 'SANS', sep = '/'),
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

plotSANS_est <- 
  final_model[["results"]][["input"]][["data"]][["file"]] %>%
    strsplit('.dat') %>% 
      paste0('.gh5') %>% 
        mplus.plot.estimated_means()

SANS_est <- 
  final_model[["results"]][["gh5"]][["means_and_variances_data"]][["y_estimated_medians"]] %>% 
  as.data.frame()


#[Alt Text](https://github.com/joshunrau/MplusLGM/blob/main/example/adv_plot.png?raw=true)
