# LOAD PACKAGES -----------------------------------------------------------
library (tidyverse)
library (magrittr)
library(foreign) 
library (haven)
library (lubridate)
library(labelled)
library(magrittr)
library(glue)

# VARS OF INTEREST ---------------------------------------------------------
## Define variables of interest 
### SD
SD_num <- c('ageentry', 'educ_num', 'FIQ', 'holltotp', 'ageonset', 'duponset', 'PAS_tot2')
SD_cat <- c('gender', 'minority', 'marital2', 'housing', 'work', 'dx_spect', 'SUD')
misc_cat <- c('Txsitn')

### Sx
SAPS <- str_c('SAPS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SANS <- str_c('SANS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SOFAS <- str_c('SOFAS_', c(0, 12, 24))
HAS <- str_c('HAS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
CDS <- str_c('CDS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
YMRS <- str_c('YMRS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
insight <- str_c('PANSS12_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

SX_0 <- paste(c('SAPS', 'SANS', 'SOFAS', 'HAS', 'CDS', 'YMRS'), '0', sep = '_')
SX_24 <- paste(c('SAPS', 'SANS', 'SOFAS', 'HAS', 'CDS', 'YMRS'), '24', sep = '_')
SX <- c(SAPS, SANS, SOFAS, HAS, CDS, YMRS)

### Remission 
PSR <- str_c('PSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
NSR <- str_c('NSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
JSR <- str_c('JSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SR <- c('PSR_BY3', 'NSR_BY3', 'NSR_at3', 'NSR_at3')
SR_C <- c('PSR_24C', 'NSR_24C')

### SUD
SUDx <-  str_c('secdx', c(1:6))
SUDpc <-  str_c('secdx', c(1:6), 'pc')

### Suicide
scd_CDS <- str_c('cd8_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
scd_BPRS <- str_c('bprs4_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

### Rx
adh <- str_c('comp_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
ord <- str_c('txm', c(2, 3, 6, 9, 12, 18, 24), "co")

### Pathway to/off care
path <- c('Transfcat')

### Trajectory 
C <- c('C_SAPS', 'C_SANS', 'C_SOFAS')
CP <-  c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')
t <- str_c('t', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

### Miscellaneous 
# appmiss <- names(select(PEPP2_df, starts_with('appmiss')))
# datedue <- names(select(PEPP2_df, starts_with('datedue')))
# sxb <- names(select(PEPP2_df, ends_with('_0')))
# items <- names(select(PEPP2_df, sap1_0:ymrs11_24))

# DATABASE ----------------------------------------------------------------
OG_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/1-992 (N=762) Jan2003-Jan2020/Merged_25Feb2022.sav") %>%
  read_spss()

CORS_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/1-992 (N=762) Jan2003-Jan2020/Cors-Demo March 2020.sav") %>%
  read_spss()

Rx_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/Rx_2016-10-17.xlsx") %>%
  read_excel()
  
  read.xlsx()
  read.table(header = TRUE)

database <- full_join(CORS_df, OG_df, by = "pin")

# LOAD PEPP2 -------------------------------------------------------------------
## Working session
.ws <- list.files(
  str_c(getwd(), '/PEPP2'), full.names = TRUE) %>%
  file.info() %>%
  filter(isdir == FALSE) %>% 
  slice_max(mtime) %>% # get the most updated workspace
  rownames()

load(.ws)

## Dataset 
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
  modify_at(c(SD_cat, SR, NSR, PSR, C, misc_cat, SUDx, SUDpc, adh), as.factor) # Recode variables as factor 

# PREPARE DATASET ---------------------------------------------------------
## Get and set labels from most updated SPSS file
labelled_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/Older/PEPP2_2022-03-07.sav") %>% 
  read_spss() # read.spss(use.value.labels = TRUE, to.data.frame = TRUE)

  labels <- look_for(labelled_df)
  labels_db <- look_for(database)
  
  for (var in names(PEPP2_df)) {
    if (var %in% labels$variable) {
      if (!is.factor(PEPP2_df[[var]])) {
        var_label(PEPP2_df[[var]]) <- var_label(labelled_df[[var]])
        val_labels(PEPP2_df[[var]]) <- val_labels(labelled_df[[var]])
        
      } else if (is.factor(PEPP2_df[[var]])) {
        PEPP2_df[[var]] <- PEPP2_df[[var]] %>%
          to_labelled
        
        var_label(PEPP2_df[[var]]) <- var_label(labelled_df[[var]])
        val_labels(PEPP2_df[[var]]) <- val_labels(labelled_df[[var]])
        
        PEPP2_df[[var]] <- PEPP2_df[[var]] %>%
          to_factor(levels = "labels", nolabel_to_na = TRUE)
      }
    }
  }
  
  labels <- look_for(PEPP2_df)

## Remove old variables
# PEPP2_df <- PEPP2_df %>%
#   select(everything(), -starts_with('appmiss'))
#   select(-("PSR_0":"t24"))
#   select(-starts_with('dsfs'), -starts_with(c('datedue', 'app')), -'dsofas_b')
  
## Add variables from database
PEPP2_df <- database %>%
select('pin', adh, ord, scd_BPRS, scd ) %>%
merge(PEPP2_df, ., by = 'pin')

#added = 'starts_with('dsfs')' 'starts_with('datedue'), starts_with('app'), 'dsofas_b', 'CRsofas12'

## Clean dataset - history
# PEPP2_df <- PEPP2_df %>%
  # setNames(., gsub('_b', '_0', names(.))) %>%
  # setNames(., gsub('_M', '_', names(.))) %>%
  # setNames(., gsub('ROB', '', names(.))) %>%
  # mutate(SOFAS_12 = coalesce(SOFAS_12, CRsofas12)) #replace missing SOFAS_12 from case review
  # setnames(old = c('minority_status', 'housing_status', 'working_status'), new = c('minority', 'housing', 'work'))
  # mutate(across(where(is.numeric), ~ round(.x, digits = 2)))  
  # mutate(across(where(is.Date), ~ replace(.x, .x < 1900-01-01, NA)))
  # vars8 <- names(PEPP2_df) %>% grep('.{9,}', ., value = TRUE) 

## Individually varying time of observation 
### Replace missing in date of assessment with due date 
DMY_df <- PEPP2_df %>% 
{list(select(., starts_with('dsfs')), select(., all_of(SAPS)), select(., 'dsfs_0', starts_with('datedue')), c(0, 1, 2, 3, 6, 9, 12, 18, 24))} %>% 
pmap_dfc(
  \(x, y, z, n) {mutate(PEPP2_df, "dmy_{n}" := case_when(!is.na(y) ~ coalesce(x, z))) %>% 
  select(starts_with('dmy'))})

### Compute individually varying time of observation t0:t24 
T_df <- PEPP2_df %>%
  select(., starts_with('dsfs')) %>% 
  {map2_dfc(., 
            c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(x, y) {mutate(., "t{y}" := as.numeric(difftime(x, dsfs_0, 'days'))) %>%
                select(glue("t{y}"))})}

### Replace missing with mean timescores 
# subset(df, n == 1 & pin <= 857)
TSCORES_df <- T_df %>% 
  {pmap_dfc(list(., 
    select(PEPP2_df, all_of(SAPS)),
    c(0, 1, 2, 3, 6, 9, 12, 18, 24)), 
       \(x, y, z) {mutate(., "tscores_{z}" := case_when(!is.na(y) ~ replace_na(x, mean(x, na.rm = TRUE)))) %>% 
           select(glue("tscores_{z}"))
    })}

PEPP2_df <- PEPP2_df %>% 
  bind_cols(T_df)
  # merge(PEPP2_df, TSCORES_df, by = 'pin', all.x = TRUE)

# COMPUTE VARS ------------------------------------------------------------
## Total scores 
for (i in c(0, 1, 2, 3, 6, 9, 12, 18, 24)) {
  PEPP2_df <- PEPP2_df %>%
    mutate(!!str_c('SAPS_', i) :=
             rowSums(across(all_of(str_c(
               c('sap7_', 'sap20_', 'sap25_', 'sap34_'), i
             ))))) %>%
    mutate(!!str_c('SANS_', i) :=
             rowSums(across(all_of(str_c(
               c('san8_', 'san13_', 'san17_', 'san22_'), i
             ))))) %>%
    mutate(!!str_c('HAS_', i) :=
             rowSums(across(all_of(str_c(
               'ha', c(1:13), '_', i)
             )))) %>%
    mutate(!!str_c('CDS_', i) :=
             rowSums(across(all_of(str_c(
               'cd', c(1:9), '_', i)
             )))) %>%
    mutate(!!str_c('YMRS_', i) :=
             rowSums(across(all_of(str_c(
               'ymrs', c(1:11), '_', i)
             ))))
}

## Sx remission
### PSR
PSR_df <-  map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(t) mutate(PEPP2_df, "PSR_{t}" := case_when(
              if_all(paste0(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), t), ~ .x < 3) ~ 1,
              if_any(paste0(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), t), ~ .x > 2) ~ 0,
              TRUE ~ NA))) %>% 
  map_dfc(\(x) select(x, starts_with("PSR"))) %>% 
  bind_cols(select(PEPP2_df, "pin"), T_df, .)

PSR_df <- PSR_df %>% 
  pivot_longer(cols = starts_with("PSR"), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(PSR_t = time[which(PSR == 1)[1]]) %>% 
  ungroup()

PSR_df <- PSR_df %>%
  pivot_wider(names_from = time,
              names_glue = "PSR_{time}",
              values_from = PSR)

PSR_df <- PSR_df %>% rowwise %>% 
  mutate(PSR_1st = map_dbl(PSR_t, \(x) ifelse(!is.na(x), get(glue("t{x}")), NA))) %>% 
  select(-all_of(t))
           
### NSR
NSR_df <-  map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(t) mutate(PEPP2_df, "NSR_{t}" := case_when(
              if_all(paste0(c("san8_", "san13_", "san17_", "san22_"), t), ~ .x < 3) ~ 1,
              if_any(paste0(c("san8_", "san13_", "san17_", "san22_"), t), ~ .x > 2) ~ 0,
              TRUE ~ NA))) %>% 
  map_dfc(\(x) select(x, starts_with("NSR"))) %>% 
  bind_cols(select(PEPP2_df, "pin"), T_df, .)

NSR_df <- NSR_df %>% 
  pivot_longer(cols = starts_with("NSR"), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(NSR_t = time[which(NSR == 1)[1]]) %>% 
  ungroup()

NSR_df <- NSR_df %>%
  pivot_wider(names_from = time,
              names_glue = "NSR_{time}",
              values_from = NSR)

NSR_df <- NSR_df %>% 
  mutate(NSR_1st = map_dbl(NSR_t, \(x) ifelse(!is.na(x), get(glue("t{x}")), NA))) %>% 
  select(-all_of(t))
  
### Joint
JSR_df <-  merge(PSR_df, NSR_df, all = TRUE)

JSR_df <-  map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
               \(t) mutate(JSR_df, "JSR_{t}" := case_when(
                 if_all(paste0(c("PSR_", "NSR_"), t), ~ .x == 1) ~ 1,
                 if_any(paste0(c("PSR_", "NSR_"), t), ~ .x == 0) ~ 0,
                 TRUE ~ NA))) %>% 
  map_dfc(\(x) select(x, starts_with("JSR"))) %>% 
  bind_cols(select(PEPP2_df, "pin"), T_df, .)

JSR_df <- JSR_df %>% 
  pivot_longer(cols = starts_with("JSR"), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(JSR_t = time[which(JSR == 1)[1]]) %>% 
  ungroup()

JSR_df <- JSR_df %>%
  pivot_wider(names_from = time,
              names_glue = "JSR_{time}",
              values_from = JSR)

JSR_df <- JSR_df %>% 
  mutate(JSR_1st = map_dbl(JSR_t, \(x) ifelse(!is.na(x), get(glue("t{x}")), NA))) %>% 
  select(-all_of(t))

PEPP2_df <- list(PEPP2_df, PSR_df, NSR_df, JSR_df) %>% 
  reduce(merge, all = TRUE) %>% 
  mutate(across(all_of(c(PSR, NSR, JSR)), ~ factor(.x, levels = c(0, 1), labels = c("not remitting", "remitting"))))

## SUD dx
val_labels(PEPP2_df$secdx1) %>% as.data.frame
attributes(PEPP2_df$secdx1)$levels %>% as.data.frame

scid <- c(seq(from = 303.90, to = 305.90, 0.1), 999.99)

PEPP2_df <- PEPP2_df %>% 
  {pmap_dfc(list(
    select(., all_of(SUDx)), 
    select(., all_of(SUDpc)), 
    1:6), 
    \(x, y, z) {mutate(., "SUD{z}" := case_when(
      !is.na(x) & (x %in% scid) & (y == 1) ~ 2,
      !is.na(x) & (x %in% scid) & (y == 2) ~ 1,
      !is.na(x) & !(x %in% scid) ~ 0,
      TRUE ~ NA)) %>%
        select(contains("SUD"))})} %>% 
  mutate(SUD = case_when(
    if_any(everything(), ~ .x == 1) ~ 1,
    if_any(everything(), ~ .x == 2) ~ 2,
    if_any(everything(), ~ .x == 0) ~ 0,
    TRUE ~ NA)) %>% 
  select("SUD") %>% 
  bind_cols(PEPP2_df, .)

PEPP2_df$SUD <- factor(PEPP2_df$SUD, levels = c(0, 1, 2), labels = c("no SUD dx", "current SUD dx", "past SUD dx"))

## Age
PEPP2_df <- PEPP2_df %>% 
  mutate(age = as.numeric(difftime(Sys.Date(), dob, 'days') / 365.25))

## Previous episode, treatment duration & antipsychotic duration
PEPP2_df <- PEPP2_df %>% 
  mutate(prev_EP = case_when(dt2stpsy < onset ~ 1, 
                             dt2stpsy >= onset ~ 0))

PEPP2_df <- PEPP2_df %>%   
  mutate(prevTx_dur = as.numeric(difftime(doe, datecont, units = 'days'))) %>% 
  mutate(prevTx_dur = case_when(prevTx_dur < 0 ~ 0, .default = prevTx_dur))

PEPP2_df <- PEPP2_df %>% 
  mutate(prevAP_dur = case_when(prevTx_dur == 0 ~ 0, 
                                ever_ap == 0 | evercont !=1 | txiscm == 1 ~ 0,
                                evercont == 1 ~ prevTx_dur))
  
## Mean folLow-up 
SD_df <- SD_df %>% 
  mutate(FU = as.numeric(difftime(ymd('2023-01-01'), doe, 'days') / 365.25))
  

## Compute missings count per outcome 
  PEPP2_df <- PEPP2_df %>% # could use rowwise() and sum()
  mutate(miss_SOFAS = rowSums(is.na(across(all_of(SOFAS))))) %>%
  mutate(miss_SAPS =rowSums(is.na(across(all_of(SAPS))))) %>%
  mutate(miss_SANS =rowSums(is.na(across(all_of(SANS)))))

# SUBSET DATASET ----------------------------------------------------------
## SD 
SD_df <- PEPP2_df %>%
  filter(pin <= 857) %>% 
  filter(ageentry >= 14 & ageentry < 36) %>%
  filter(FIQ >= 70 | is.na(FIQ)) %>% 
  filter(dx_spect !=3 | is.na(dx_spect)) %>% 
  filter(prev_EP == 0 | is.na(prev_EP)) %>% 
  filter(prevAP_dur < 30 | is.na(prevAP_dur)) %>% 
  filter(case_when(prevTx_dur >= 30 & is.na(prevAP_dur) ~ FALSE, .default = TRUE)) %>% 
  filter(is.na(Txsitn)) %>% 
  select('pin', all_of(c(SD_num, SD_cat, 'doe', SAPS, SANS, SOFAS, HAS, CDS, YMRS)))

val_labels(labelled_df$dx_spect) %>% as.data.frame
val_labels(labelled_df$Txsitn) %>% as.data.frame

## Traj 
SAPNS_df <- SD_df %>%
  filter(miss_SAPS <= 4)

SOFAS_df <- SD_df %>%
  filter(miss_SOFAS <= 1)



# RESHAPE DATASET ---------------------------------------------------------
SAPNS_Ldf <- pivot_longer(SAPNS_df,
            cols =c(SAPS, SANS, PSR, NSR, HAS, CDS, YMRS), 
            names_to = c('.value', 'time'),
            names_pattern = '(.+)_(\\d+)'
)

# DESCRIPTIVES STATS ####
## Df description 
dfSummary()

## Sociodemographics 
library(psych)
library(kableExtra)
library(vtable)

dscr()
tby() # by group

describeBy(SAPNS_df ~ K_SAPS, skew = FALSE, ranges = FALSE)

df %>% group_by(cat) %>% 
  summarise_at(vars(colnames(df)[3:9]), mean, na.rm = TRUE)
  summary(SD_df$var)

SD_tb <- sumtable(
  data = SD_df,
  vars = c(SD_num, SD_cat, 'SAPS_0', 'SANS_0', 'SOFAS_0'),
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group.test = FALSE,
  out = 'return',
)

SOFAS_tb <- sumtable(
  data = SOFAS_df,
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group = 'K_SOFAS',
  group.test = TRUE,
  out = 'return',
)

SAPS_tb <- sumtable(
  data = SAPNS_df,
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group = 'K_SAPS',
  group.test = TRUE,
  out = 'return',
)

SANS_tb <- sumtable(
  data = SAPNS_df,
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group = 'K_SANS',
  group.test = TRUE,
  out = 'return',
)

# MISSING DATA ANALYSES ####
## Little's MCAR 
library(MissMech)

df %>% 
  select('vars') %>% 
  TestMCARNormality

## Average percentage of missing data 
library(misty)
na.descript(SD_df)

SD_df <- PEPP2_df %>% subset(pin <= 857) %>% select(across(starts_with(c('sap7_', 'sap20_', 'sap25_', 'sap34_','san8_', 'san13_', 'san17_', 'san22_'))))

## Percent of missing data per columns 
miss <- {unlist(lapply(SAPNS_df, function(x) sum(is.na(x)))) / nrow(SAPNS_df) * 100} %>% view()

# MULTIPLE IMPUTATION ####
## Dataset to imput 
df2imput <- PEPP2_df %>% 
  subset(n == 1 & pin <= 857) %>%
  select(c('pin', SD_num, SD_cat, items)) #impute raw scores is thought to be better than transformed scores

## Imputation method 
# Instructions 
# PMM (Predictive Mean Matching)  – For numeric variables
# logreg(Logistic Regression) – For Binary Variables( with 2 levels)
# polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
# Proportional odds model (ordered, >= 2 levels)

library(mice)
df2imput <- mice(df2imput, maxit = 0)
pred <- df2imput$predictorMatrix #variables included in the prediction moded
meth <- df2imput$method #method choose for imputation per variables
df2imput$loggedEvents

meth <- make.method(df2imput)# make default method

poly <- c() #add vars, logical expression
meth[poly] <- 'polr'
log <- c()
meth[log] <- 'logreg'
poly2 <- c()
meth[poly2] <- 'polyreg'

removed <- c(names(subset(miss, miss > 50))) #exclude variables from imputation
meth[removed] <- ''
as.data.frame(meth)

## Imputation predictors 
# Instructions 
# select 15-25 variables
# include all variables that appear in the complete-data model
# include the variables that are related to the nonresponse
# include variables that explain a considerable amount of variance
# Remove pred variables that have too many missing values within the subgroup of incomplete cases
# A value of 1 indicates that the column variable is a predictor to impute the target (row) variable, and a 0 means that it is not used

pred <- make.predictorMatrix(df2imput)#make default prediction matrix
pred <- quickpred(df2imput, mincor=0.2, minpuc=0.5, include=c(SD_num, SD_cat), exclude=c('pin')) 

pred[, c(SD_num, SD_cat)] <- 1 #include predictors
pred[removed, c('dx_spect', 'ageonset')] <- 0 #exclude variables not imputed from prediction matrix


pred %>% rowSums %>% mean

## Compute imputation 
imput_df <- mice(
  df2imput,
  m = 2, #m: number of multiple imputations = average percentage of missing data to impute (up to 50%)
  maxit = 1, #m: number of iterations = 5-20
  predictorMatrix = pred,
  method = meth,
  seed = 22,
  print =  FALSE
)

## Compute passive imputation 
library(glue)
for (i in c(0, 1, 2, 3, 6, 9, 12, 18, 24)) {
#meth[str_c('SAPS_', i)] <- str_c(''~I(rowSums(across(all_of(', str_c(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), i), ')))))'')
# Work in progress
  }


imput_df <- mice.impute.passive(
  df2imput,
  m = 1, #m: number of multiple imputations = average percentage of missing data to impute (up to 50%)
  maxit = 0, #m: number of iterations = 5-20
  predictorMatrix = pred, #should remove ageonset and dx_spect (colinerarity)
  method = meth,
  seed = 22,
  print =  FALSE
)

for (i in c(0, 1, 2, 3, 6, 9, 12, 18, 24)) {
  imput_df <- imput_df %>%
    mutate(!!str_c('SAPS_', i) :=
             rowSums(across(all_of(str_c(
               c('sap7_', 'sap20_', 'sap25_', 'sap34_'), i
             ))))) %>%
    mutate(!!str_c('SANS_', i) :=
             rowSums(across(all_of(str_c(
               c('sap8_', 'sap13_', 'sap17_', 'sap22_'), i
             ))))) %>%
    mutate(!!str_c('HAS_', i) :=
             rowSums(across(all_of(str_c(
               'ha', c(1:13), '_', i)
             )))) %>%
    mutate(!!str_c('CDS_', i) :=
             rowSums(across(all_of(str_c(
               'cd', c(1:9), '_', i)
             )))) %>%
    mutate(!!str_c('YMRS_', i) :=
             rowSums(across(all_of(str_c(
               'ymrs', c(1:11), '_', i)
             ))))
}

## Check Imputation 
warnings <- imput_df$loggedEvents

densityplot(imput_df)
stripplot(imput_df)

comp1_df <- complete(imput_df, 1) #return the 1st of the imputed dataset
na.descript(comp1_df)

miss_comp1 <- unlist(lapply(comp1_df, function(x) sum(is.na(x)))) / nrow(comp1_df) * 100
miss_comp1 <- sort(miss_comp1, decreasing = TRUE)
as.data.frame(miss_comp1) 

library(sjmisc)
merged_df <- merge_imputations(df2imput, imput_df, summary = 'dens')

## Analyses with imputed datasets 
library(jtools)
library(MKmisc)
library(limma)
library(miceadds)

#X2 
X2 <- with(imput_df, chisq.test(K_SOFAS, gender, correct = FALSE)) %>%
  summary() %>%
  as.numeric(X2$statistic) %>% 
  micombine.chisquare(1)

#t-test 
DUP_t <- with(imput_df, lm(duponset ~ K_SOFAS))
summary(pool(DUP_t))

summary(with(subSOFAS_df, lm(duponset ~ K_SOFAS)))

#ANOVA 
DUP_lm <- with(imput_df, lm(CP1_SOFAS ~ duponset))
summ(pool(DUP_lm))

DUP_posthoc <- with(imput_df, pairwise.t.test(duponset, K_SANS, paired = FALSE, p.adjust.method = 'bonferroni'))
posthoc_p <- as.list(DUP_posthoc$analyses)
summ(pool(posthoc_p))


age_anova <- mi.anova(imput_df,'ageentry ~ K_SANS')
FIQ_anova <- mi.anova(imput_df, 'FIQ ~ K_SANS')


# SAVE DATASET -----------------------------------------------------------
## CSV file
  write_csv(PEPP2_df, paste0('/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/PEPP2_', today(), '.csv'))

## SAV file 
  # write_sav(PEPP2_df, paste('/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/PEPP2_', today(), '.sav',  sep=''))

# SAVE IMAGE --------------------------------------------------------------
save.image(glue(getwd(), 'PEPP2', 'PEPP2_{today()}.RData', .sep = '/'))


  
# JUNK #### 
### Linear models 
library(lavaan) #SEM
library(brms)
library(lme4)
library(car)

lmer(SAPS ~ CP1_SAPS*time +  (1|pin), SAPNS_Ldf, REML=FALSE) %>% summary  
lmer(SAPS ~ CP2_SAPS*time +  (1|pin), SAPNS_Ldf, REML=FALSE) %>% summary  
lmer(SAPS ~ K_SAPS + (1|pin), SAPNS_Ldf, REML=FALSE) %>% summary #%>% Anova

gls(FIQ ~ CP1_SAPS, SAPNS_df, method='ML', na.action=na.omit) %>% summary #GLS method?

lm(FIQ ~ CP1_SAPS, SAPNS_df) %>% summary
lm(FIQ ~ CP2_SAPS, SAPNS_df) %>% summary

lm(FIQ ~ K_SAPS, SAPScov_df) %>% summary
glm(K_SAPS ~ FIQ, binomial, SAPScov_df) %>% summary

library(nlme)
lme(SAPS_0 ~ CP1_SAPS + CP2_SAPS, data=SAPNS_df, method = 'ML', na.action= 'na.pass' )

### Mixed models 
lme(
  fixed = SANS ~ CP1_SAPS + time + CP1_SAPS:time, 
  random = ~1|pin,
  #correlation = corAR1(), #0, form = ~time|pin
  data = SAPNS_Ldf, 
  method = 'ML', 
  na.action = na.pass(SAPNS_Ldf)
) %>% summary

### Prepare dataset for Mplus 
# remove characters from variables names > 8 characters 
names(PEPP2_df) <- str_sub(names(PEPP2_df), 1, 8) 
namesx2(PEPP2_df) # Function to check if column names are unique 
namesx2 <- function(df) { 
  
  length1 <- length(colnames(df))
  length2 <- length(unique(colnames(df)))        
  if (length1 - length2 > 0 ) {
    print(paste('There are', length1 - length2, ' duplicates', sep=' '))
  }     
}
anyDuplicated(colnames(PEPP2_df)) # locate column of the first duplicate)
data.frame(colnames(PEPP2_df))

# Create tab-delimited file and Mplus input syntax from R data.frame 
prepareMplusData(
  SAPScov_df,
  filename = str_c(getwd(),'/MplusfromR/cov.dat'),
  inpfile = FALSE,
  writeData = 'always'
)

# Create the Mplus input text for an mplusObject 
inp <- createSyntax(
  object = m, 
  filename = str_c(getwd(),'/MplusfromR/GBTM.inp'),
  check = FALSE,
  add = FALSE) %>% 
  writeLines(str_c(getwd(),'/MplusfromR/GBTM.inp'))

# Run Mplus Models 
runModels('/Users/olivierpercie/Desktop/MplusLGM/MplusfromR/GMM1_i s-cub@0.inp')




