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
SUDx <-  str_c('secdx', c(1:6))
SUDpc <-  str_c('secdx', c(1:6), 'pc')

SX_0 <-  c('SAPS_0', 'SANS_0', 'SOFAS_0', 'HAS_0', 'CDS_0', 'YMRS_0')
SX <- c(SAPS, SANS, SOFAS, HAS, CDS, YMRS)

### Remission 
PSR <- str_c('PSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
NSR <- str_c('NSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SR <- c('PSR_BY3', 'NSR_BY3', 'NSR_at3', 'NSR_at3')
SR_C <- c('PSR_24C', 'NSR_24C')

### Trajectory 
C <- c('C_SAPS', 'C_SANS', 'C_SOFAS')
CP <-  c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')
t <- str_c('t', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

### Miscellaneous 
# appmiss <- names(select(PEPP2_df, starts_with('appmiss')))
# datedue <- names(select(PEPP2_df, starts_with('datedue')))
# sxb <- names(select(PEPP2_df, ends_with('_0')))
# items <- names(select(PEPP2_df, sap1_0:ymrs11_24))

# LOAD DATASET ------------------------------------------------------------
## CSV file 
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
  modify_at(c(SD_cat, SR, NSR, PSR, C, misc_cat, SUDx, SUDpc), as.factor) # Recode variables as factor 
  
# PREPARE DATASET ---------------------------------------------------------
  
## Get and Set labels  from most updated SPSS file
labelled_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/Older/PEPP2_2022-03-07.sav") %>% 
  read_spss()

  # labels <- look_for(original_df)
  labels <- look_for(labelled_df)
  
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
PEPP2_df <- PEPP2_df %>% 
  select(-("PSR_0":"t24"))
  
## Add variables from original dataset
original_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/1-992 (N=762) Jan2003-Jan2020/Merged_25Feb2022.sav") %>% 
  read_spss() # read.spss(use.value.labels = TRUE, to.data.frame = TRUE)

PEPP2_df <- original_df %>% 
  select('pin', starts_with('dsfs'), starts_with(c('datedue', 'app')), 'dsofas_b') %>% 
  merge(PEPP2_df, ., by = 'pin')

#added = 'starts_with('dsfs')' 'starts_with('datedue'), starts_with('app'), 'dsofas_b', 'CRsofas12'

## Clean dataset - history
PEPP2_df <- PEPP2_df %>%
  setNames(., gsub('_b', '_0', names(.))) %>%
  mutate(across(c(dsofas_0, dsfs_0, dsfs_12), ~ replace(.x, .x < 2000-01-01, NA))) #some rows in dsfs_0 and dsfs_12 = 1582-10-14 (issue with spss import?)
  # setNames(., gsub('_M', '_', names(.))) %>%
  # setNames(., gsub('ROB', '', names(.))) %>%
  # mutate(SOFAS_12 = coalesce(SOFAS_12, CRsofas12)) #replace missing SOFAS_12 from case review
  # setnames(old = c('minority_status', 'housing_status', 'working_status'), new = c('minority', 'housing', 'work'))
  # PEPP2_df <-PEPP2_df %>% mutate(across(case_when(is.numeric(.) ~ round(., digits = 2)))) #to fix
  # vars8 <- names(PEPP2_df) %>% grep('.{9,}', ., value = TRUE) 

## Individually varying time of observation 
### Replace missing in date of assessment with due date 
DMY_df <- PEPP2_df %>% 
{list(select(., starts_with('dsfs')), select(., all_of(SAPS)), select(., 'dsfs_0', starts_with('datedue')), c(0, 1, 2, 3, 6, 9, 12, 18, 24))} %>% 
pmap_dfc(
  \(x, y, z, n) {mutate(PEPP2_df, "dmy_{n}" := case_when(!is.na(y) ~ coalesce(x, z))) %>% 
  select(starts_with('dmy'))})

### Compute individually varying time of observation t0:t24 
T_df <- DMY_df %>%
  {map2_dfc(., 
            c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(x, y) {mutate(., "t{y}" := as.numeric(difftime(x, dmy_0, 'days')) / (365.25 / 12)) %>%
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
  bind_cols(select(PEPP2_df, "pin", all_of(t)), .)

PSR_lf <- PSR_df %>% 
  pivot_longer(cols = starts_with("PSR"), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(PSR_t = time[which(PSR == 1)[1]]) %>% 
  ungroup()

PSR_wf <- PSR_lf %>%
  pivot_wider(names_from = time,
              names_glue = "PSR_{time}",
              values_from = PSR)

PSR_wf <- PSR_wf %>% 
  mutate(PSR_1st = map_dbl(PSR_t, \(x) ifelse(!is.na(x), get(glue("t{x}")), NA)))
           
### NSR
NSR <-  map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(t) mutate(PEPP2_df, "NSR_{t}" := case_when(
              if_all(paste0(c("san8_", "san13_", "san17_", "san22_"), t), ~ .x < 3) ~ 1,
              if_any(paste0(c("san8_", "san13_", "san17_", "san22_"), t), ~ .x > 2) ~ 0,
              TRUE ~ NA))) %>% 
  map_dfc(\(x) select(x, starts_with("NSR"))) %>% 
  bind_cols(select(PEPP2_df, "pin", all_of(t)), .)

NSR_lf <- NSR_df %>% 
  pivot_longer(cols = starts_with("NSR"), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(NSR_t = time[which(NSR == 1)[1]]) %>% 
  ungroup()

NSR_wf <- NSR_lf %>%
  pivot_wider(names_from = time,
              names_glue = "NSR_{time}",
              values_from = NSR)

NSR_wf <- NSR_wf %>% 
  mutate(NSR_1st = map_dbl(NSR_t, \(x) ifelse(!is.na(x), get(glue("t{x}")), NA)))
  
PEPP2_df <- PEPP2_df %>% 
  select(starts_with("PSR", "NSR")) %>% 
  factor(levels = c(0, 1), labels = c("not remitting", "remitting"))

  
  



## SUD dx
val_labels(PEPP2_df$secdx1) %>% as.data.frame

scid <- c(seq(from = 303.90, to = 305.90, 0.1), 999.99)

PEPP2_df <- PEPP2_df %>% 
  {pmap_dfc(list(
    select(., SUDx), 
    select(., SUDpc), 
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

## Compute missings count per outcome 
  PEPP2_df <- PEPP2_df %>% # could use rowwise() and sum()
  mutate(miss_SOFAS = rowSums(is.na(across(SOFAS)))) %>%
  mutate(miss_SAPS =rowSums(is.na(across(SAPS)))) %>%
  mutate(miss_SANS =rowSums(is.na(across(SANS))))

# SAVE DATASET  -----------------------------------------------------------
## SAV file 
  write_sav(PEPP2_df, paste('/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/PEPP2_', today(), '.sav',  sep=''))
  
## CSV file
  write_csv(PEPP2_df, paste0('/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/PEPP2_', today(), '.csv'))


# SUBSET DATASET ----------------------------------------------------------
## SD 
SD_df <- PEPP2_df %>%
  subset(pin <= 857) %>%
  select('pin', SD_num, SD_cat, 'doe', SAPS, SANS, PSR, NSR, SOFAS, HAS, CDS, YMRS, SR_C, SR_BY, t)

## Traj 
SAPNS_df <- PEPP2_df %>%
  subset(miss_SAPS <= 4 & n == 1) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, SOFAS, HAS, CDS, YMRS, K, CP, SR_C, SR_BY, t)

SOFAS_df <- PEPP2_df %>%
  subset(miss_SOFAS <= 1 & n == 1) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, SOFAS, HAS, CDS, YMRS, K, CP, SR_C, SR_BY, t)

## MI 
df2imput <- PEPP2_df %>% 
  subset(n == 1 & pin <= 857) %>%
  select(c('pin', SD_num, SD_cat, items)) #impute raw scores is thought to be better than transformed scores


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

# mean folow-up

SD_df <- SD_df %>% 
  mutate(FU = as.numeric(difftime(ymd('2023-01-01'), doe, 'days') / 365.25)) 

# MISSING DATA ANALYSES ####
## Little's MCAR 
library(MissMech)

df2imput %>% 
  select('vars') %>% 
  TestMCARNormality

## Average percentage of missing data 
library(misty)
na.descript(SD_df)

SD_df <- PEPP2_df %>% subset(pin <= 857) %>% select(across(starts_with(c('sap7_', 'sap20_', 'sap25_', 'sap34_','san8_', 'san13_', 'san17_', 'san22_'))))

## Percent of missing data per columns 
miss <- {unlist(lapply(SAPNS_df, function(x) sum(is.na(x)))) / nrow(SAPNS_df) * 100} %>% view()

# MULTIPLE IMPUTATION ####
library(mice)
df2imput <- mice(df2imput, maxit = 0)
pred <- df2imput$predictorMatrix #variables included in the prediction moded
meth <- df2imput$method #method choose for imputation per variables
df2imput$loggedEvents

## Imputation method 
# Instructions 
# PMM (Predictive Mean Matching)  – For numeric variables
# logreg(Logistic Regression) – For Binary Variables( with 2 levels)
# polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
# Proportional odds model (ordered, >= 2 levels)


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




# SAVE #### 
save.image(glue(getwd(), 'PEPP2_{today()}.RData', .sep = '/'))

X