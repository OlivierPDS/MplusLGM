# LOAD PACKAGES -----------------------------------------------------------
library (tidyverse)
library (magrittr)
library(foreign) 
library (haven)
library(readxl)
library (lubridate)
library(labelled)
library(magrittr)
library(glue)

# VARS OF INTEREST ---------------------------------------------------------
## Sociodemographics
SD_num <- c('ageentry', 'educ_num', 'FIQ', 'holltotp')
SD_cat <- c('gender', 'minority', 'marital2', 'housing', 'work')
MISC_cat <- c('Txsitn')
SD <- c(SD_num, SD_cat, MISC_cat)

## Clinical characteristics
CLIN_num <- c('PAS_tot2', 'ageonset', 'duponset', 'conv')
CLIN_cat <- c('CAYRconv', 'mode', 'referral', 'transfR', 'dx_spect', 'SUD')
CLIN <- c(CLIN_num, CLIN_cat, 'doe')

## Psychopathology
SAPS <- str_c('SAPS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SANS <- str_c('SANS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SOFAS <- str_c('SOFAS_', c(0, 12, 24))
HAS <- str_c('HAS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
CDS <- str_c('CDS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
YMRS <- str_c('YMRS_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
INS8 <- str_c('G12_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

## SUD
SUDx <-  str_c('secdx', c(1:6))
SUDpc <-  str_c('secdx', c(1:6), 'pc')

## Suicide
scd_CDS <- str_c('cd8_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
scd_BPRS <- str_c('bprs4_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))

## Neurocognition
COG <- c('verbm_z', 'wm_z', 'ef_z', 'sop_z', 'vism_z', 'va_z', 'sc_z')

## Summary
SX_0 <- paste(c('SAPS', 'SANS', 'SOFAS', 'HAS', 'CDS', 'YMRS', 'G12', 'cd8', 'bprs4'), '0', sep = '_')
SX_24 <- paste(c('SAPS', 'SANS', 'SOFAS', 'HAS', 'CDS', 'YMRS', 'G12', 'cd8', 'bprs4'), '24', sep = '_')
SX <- c(SAPS, SANS, SOFAS, HAS, CDS, YMRS, INS8, scd_CDS, scd_BPRS)

## Remission & Recovery
PSR <- str_c('PSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
NSR <- str_c('NSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
JSR <- str_c('JSR_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
SR_0 <- paste(c('PSR', 'NSR', 'JSR'), '0', sep = '_')
SR_24 <- paste(c('PSR', 'NSR', 'JSR'), '24', sep = '_')
SR_t <- paste(c('PSR', 'NSR', 'JSR'), 't', sep = '_')
SR_1st <- paste(c('PSR', 'NSR', 'JSR'), '1st', sep = '_')
SR <- c(PSR, NSR, JSR, SR_t, SR_1st, 'JSR_by3', 'JSR_24C', 'RECOV_24')

## Rx
adh <- str_c('comp_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
ord <- str_c('txm', c(0, 1, 2, 3, 6, 9, 12, 18, 24), "co")
CPZw <- str_c('CPZw_', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
RX_0 <- c('comp_0', 'txm0co', 'CPZw_0')
RX_24 <- c('comp_24', 'txm24co', 'CPZw_24')
RX <-  c(ord, adh, CPZw)
# AP_yn <- starts_with('anti')
# AP_type <- starts_with('atype')

## Trajectory 
C <- c('C_SAPS', 'C_SANS', 'C_SOFAS')
CP <-  c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')

## Categorical variables to recode as factors
cat <- c(SD_cat, MISC_cat, CLIN_cat, SUDx, SUDpc, PSR, NSR, JSR, 'JSR_by3', 'RECOV_24', ord, C)

## Miscellaneous
# miss <- paste('miss', c('SOFAS','SAPS', 'SANS'))
# dsfs <- paste('dsfs_' c(1, 2, 3, 6, 9, 12, 18, 24))
# appmiss <- names(select(PEPP2_df, starts_with('appmiss')))
# datedue <- names(select(PEPP2_df, starts_with('datedue')))
# items <- names(select(PEPP2_df, sap1_0:ymrs11_24))

# DATABASE ----------------------------------------------------------------
OG_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/1-992 (N=762) Jan2003-Jan2020/Merged_25Feb2022.sav") %>%
  read_spss()

CORS_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/1-992 (N=762) Jan2003-Jan2020/Cors-Demo March 2020.sav") %>%
  read_spss()

COG_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/2021-09-16_neuropsyc_PEPP.sav") %>%
  read_spss() %>% 
  rename(pin = PEPP_ID)

CPZeq_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/0. PEPP/Data/Databases/CPZeq_2016-10-17.xlsx") %>%
  read_excel(na = ".")

database <- list(OG_df, CORS_df, COG_df, CPZeq_df) %>% 
  reduce(full_join, by = "pin")

labels_db <- look_for(database)

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
    full.names = TRUE) %>%
  file.info() %>%
  filter(isdir == FALSE) %>% 
  slice_max(mtime) %>% # get the most updated file
  rownames() %>%
  read_csv() %>% 
  modify_at(cat, as.factor) # Recode variables as factor 

# PREPARE DATASET ---------------------------------------------------------
## Get and set labels from most updated SPSS file
# labelled_df <- paste("/Users/olivierpercie/Library/CloudStorage/OneDrive-McGillUniversity/CRISP/PhD/1. PEPP2/Data/Datasets/Older/PEPP2_2022-03-07.sav") %>%
#   read_spss() # read.spss(use.value.labels = TRUE, to.data.frame = TRUE)

  
  for (var in names(PEPP2_df)) {
    if (var %in% labels_db$variable) {
      if (!is.factor(PEPP2_df[[var]])) {
        var_label(PEPP2_df[[var]]) <- var_label(database[[var]])
        val_labels(PEPP2_df[[var]]) <- val_labels(database[[var]])
        
      } else if (is.factor(PEPP2_df[[var]])) {
        PEPP2_df[[var]] <- PEPP2_df[[var]] %>%
          to_labelled
        
        var_label(PEPP2_df[[var]]) <- var_label(database[[var]])
        val_labels(PEPP2_df[[var]]) <- val_labels(database[[var]])
        
        PEPP2_df[[var]] <- PEPP2_df[[var]] %>%
          to_factor(levels = "labels", nolabel_to_na = TRUE)
      }
    }
  }
  
  labels_PEPP2 <- look_for(PEPP2_df)
  
## Add variables
PEPP2_df <- database %>%
  select(any_of(c(SD, conv, path, SX, COG, SR, RX)), starts_with('pang12'), -any_of(c(as.vector(names(PEPP2_df)))), 'pin') %>%
  merge(PEPP2_df, ., all.x = TRUE, by = 'pin')

### Already added
  # select('pin', starts_with('dsfs'), starts_with('datedue'), starts_with('app'), 'dsofas_b', 'CRsofas12')

## Remove variables
# PEPP2_df <- PEPP2_df %>%
# select(-any_of(c(SR)))
# select(-any_of(c(C, CP)))
# select(-starts_with('dsfs'), -starts_with(c('datedue', 'app')), -'dsofas_b')

## Relocate variables
PEPP2_df <- PEPP2_df %>% 
  select('pin', all_of(c(SD, CLIN)), everything()) %>% 
  relocate(starts_with('miss'), starts_with('complete'), all_of(c(SX, COG, SR, RX)), .after = last_col())
  
## Modification  history
### Rename variables (inconsistent / > 8 characters)
# PEPP2_df <- PEPP2_df %>%
#   rename(transfR = Transfcat)
# rename_with(~ str_c('G12_', c(0, 1, 2, 3, 6, 9, 12, 18, 24)), starts_with('pang12'))
# setNames(., str_remove(names(.), 'CogState_'))
# rename(txm0co = txordb, txm1co = txordm1)
# setNames(., gsub('_b', '_0', names(.)))
# setNames(., gsub('_M', '_', names(.)))
# setnames(old = c('minority_status', 'housing_status', 'working_status'),
#          new = c('minority', 'housing', 'work'))
# vars8 <- names(PEPP2_df) %>% grep('.{9,}', ., value = TRUE)

### Recode categorical variables
PEPP2_df <- PEPP2_df %>%
  fct_collapse(referral,
             community = c(),
             line_1st = c(),
             line_2nd = c(),
             line_3rd = c())

### Recode missings
# PEPP2_df <- PEPP2_df %>%
# mutate(across(c('CAYRconv', all_of(ord)), ~ coalesce(.x, '0')))
# mutate(across(where(is.Date), ~ replace(.x, .x < 1900-01-01, NA)))
# mutate(SOFAS_12 = coalesce(SOFAS_12, CRsofas12)) #replace missing SOFAS_12 from case review

### Reformat variables
# PEPP2_df <- PEPP2_df %>%
# mutate(across(where(is.numeric), ~ round(.x, digits = 2)))

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
NSR_df <- map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
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
  
### JSR
JSR_df <- merge(PSR_df, NSR_df, all = TRUE)

JSR_df <- map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
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
  mutate(JSR_by3 = case_when(
    if_any(num_range('JSR_', 1:3), ~ .x == 1) ~ 1,
    if_all(num_range('JSR_', 1:3), ~ .x == 0) ~ 0,
    TRUE ~ NA)) %>% 
  rowwise() %>% 
  mutate(JSR_24C = sum(c_across(all_of(JSR)), na.rm = TRUE)) %>% #INCLUDE NA
  select(-all_of(t))
  
PEPP2_df <- list(PEPP2_df, PSR_df, NSR_df, JSR_df) %>% 
  reduce(merge, all = TRUE) %>% 
  mutate(across(c('JSR_by3', all_of(c(PSR, NSR, JSR))), ~ factor(.x, levels = c(0, 1), labels = c("no remission", "remission"))))

## Recovery
PEPP2_df <- PEPP2_df %>% 
  mutate(RECOV_24 = case_when(JSR_24 == 'remission' & SOFAS_24 >= 61 ~ 1, 
                              JSR_24 == 'no remission' | SOFAS_24 < 61 ~ 0, 
                              .default = NA))

PEPP2_df$RECOV_24 <- factor(PEPP2_df$RECOV_24, levels = c(0, 1), labels = c("no recovery", "recovery"))

recov <- PEPP2_df %>% 
  select(JSR_24, SOFAS_24, RECOV_24)

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
  mutate(age = as.numeric(difftime(Sys.Date(), DOB, 'days') / 365.25))

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

## Mode of onset
PEPP2_df <- PEPP2_df %>% 
  mutate(conv = as.numeric(difftime(onset, date_pr, units = 'days'))) %>% 
  mutate(mode = case_when(conv <= 30 ~ "acute",
                          conv > 30 ~ "insidious"))
  
## Mean folLow-up 
SD_df <- SD_df %>% 
  mutate(FU = as.numeric(difftime(ymd('2023-01-01'), doe, 'days') / 365.25))
  
## Compute missings count per outcome 
  PEPP2_df <- PEPP2_df %>% # could use rowwise() and sum()
  mutate(miss_SOFAS = rowSums(is.na(across(all_of(SOFAS))))) %>%
  mutate(miss_SAPS = rowSums(is.na(across(all_of(SAPS))))) %>%
  mutate(miss_SANS = rowSums(is.na(across(all_of(SANS)))))

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
  select('pin', all_of(c(SD, 'doe', SX, SR, )))

val_labels(labelled_df$dx_spect) %>% as.data.frame
val_labels(labelled_df$Txsitn) %>% as.data.frame

## Traj 
SOFAS_df <- SD_df %>%
  filter(miss_SOFAS <= 1)

SAPNS_df <- SD_df %>%
  filter(miss_SAPS <= 4)

# RESHAPE DATASET ---------------------------------------------------------
long_df <- pivot_longer(SAPNS_df,
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

# Work in progress
## Compute passive imputation 
# for (i in c(0, 1, 2, 3, 6, 9, 12, 18, 24)) {
# meth[str_c('SAPS_', i)] <- str_c(''~I(rowSums(across(all_of(', str_c(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), i), ')))))
#   }

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
