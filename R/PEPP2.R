library (tidyverse)
library (magrittr)
library (lubridate)
library (haven)


# PREPARE DATASET ####
### Load dataset 
  # SAV file 
  merged <- paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Databases/1-992 (N=762) Jan2003-Jan2020/Merged_25Feb2022.sav') %>% 
    read_spss() 

  # CSV file 
  PEPP2_df <-
    list.files(
      "/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2",
      full.names = T
    ) %>%
    file.info() %>%
    slice_max(mtime) %>% # get the most updated file
    rownames() %>%
    read_csv()


### Define variables of interest 
  # SD  
SD_num <- c('ageentry', 'educ_num', 'FIQ', 'holltotp', 'ageonset', 'duponset', 'PAS_tot2')
SD_cat <- c('gender', 'minority', 'marital2', 'housing', 'work', 'dx_spect', 'SUD')

  # Sx 
SAPS <- names(select(PEPP2_df, num_range('SAPS_', 0:24)))
SANS <- names(select(PEPP2_df, num_range('SANS_', 0:24)))
SOFAS <- names(select(PEPP2_df, num_range('SOFAS_', 0:24)))
HAS <- names(select(PEPP2_df, num_range('HAS_', 0:24)))
CDS <- names(select(PEPP2_df, num_range('CDS_', 0:24)))
YMRS <- names(select(PEPP2_df, num_range('YMRS_', 0:24)))
SX_0 <-  c('SAPS_0', 'SANS_0', 'SOFAS_0', 'HAS_0', 'CDS_0', 'YMRS_0')
SX <- c(SAPS, SANS, SOFAS, HAS, CDS, YMRS)

  # Remission 
PSR <- names(select(PEPP2_df, num_range('PSR_', 0:24)))
NSR <- names(select(PEPP2_df, num_range('NSR_', 0:24)))
SR_C <- c('PSR_24C', 'NSR_24C')
SR_BY <- c('PSR_BY3', 'NSR_BY3')

  # Trajectory 
C <- c('C_SAPS', 'C_SANS', 'C_SOFAS')
CP <-  c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')
t <- names(select(PEPP2_df, num_range('t', 0:24)))

  # Miscellaneous 
appmiss <- names(select(PEPP2_df, starts_with('appmiss')))
datedue <- names(select(PEPP2_df, starts_with('datedue')))
dsfs <- names(select(PEPP2_df, starts_with('dsfs'), -'dsfs_0'))
sxb <- names(select(PEPP2_df, ends_with('_0')))
items <- names(select(PEPP2_df, sap1_0:ymrs11_24))


### Recode variables as factor 
PEPP2_df <- PEPP2_df %>%
  modify_at(c(SD_cat, SR_BY, C), as.factor) # attr(), attributes() to check label and levels

### Add variables 
vars <- "starts_with('dsfs')"
vars <- "starts_with('datedu'), starts_with('app'), 'dsofas_b', 'CRsofas12'"

addvar_df <-
  paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Databases/#1-992 (N=762) Jan2003-Jan2020/Merged_25Feb2022.sav') %>%
  read_spss(user_na = FALSE, col_select = c('pin', starts_with('datedu'), starts_with('app'), 'dsofas_b', 'CRsofas12'))

PEPP2_df <- merge(PEPP2_df, addvar_df, by = 'pin')

### Clean dataset 
PEPP2_df <- PEPP2_df %>%
  # setNames(., gsub("_b", "_0", names(.))) %>%
  # setNames(., gsub("_M", "_", names(.))) %>%
  # setNames(., gsub("ROB", "", names(.))) %>%
  # mutate(across(c(dsofas_0, dsfs_0, dsfs_12), function(x) replace(x, x < 2000-01-01, NA)))  %>% #some rows in dsfs_0 and dsfs_12 = 1582-10-14 (issue with spss import?)
  # mutate(SOFAS_12 = coalesce(SOFAS_12, CRsofas12)) #replace missing SOFAS_12 from case review
  setnames(old = c('minority_status', 'housing_status', 'working_status'), new = c('minority', 'housing', 'work'))

vars8 <- names(PEPP2_df) %>% grep('.{9,}', ., value = TRUE) 

### Individually varying time of observation 
  # Replace missing in date of assessment with due date 
PEPP2_df <- PEPP2_df %>%
  mutate(dsfs_1 = case_when(!is.na(SAPS_1) ~ coalesce(dsfs_1, datedue1))) %>%
  mutate(dsfs_2 = case_when(!is.na(SAPS_2) ~ coalesce(dsfs_2, datedue2))) %>%
  mutate(dsfs_3 = case_when(!is.na(SAPS_3) ~ coalesce(dsfs_3, datedue3))) %>%
  mutate(dsfs_6 = case_when(!is.na(SAPS_6) ~ coalesce(dsfs_6, datedue6))) %>%
  mutate(dsfs_9 = case_when(!is.na(SAPS_9) ~ coalesce(dsfs_9, datedue9))) %>%
  mutate(dsfs_12 = case_when(!is.na(SAPS_12) ~ coalesce(dsfs_12, datedue12))) %>%
  mutate(dsfs_18 = case_when(!is.na(SAPS_18) ~ coalesce(dsfs_18, datedue18))) %>%
  mutate(dsfs_24 = case_when(!is.na(SAPS_24) ~ coalesce(dsfs_24, datedue24)))

#  #Not working
# for (i in c(1, 2, 3, 6, 9, 12, 18, 24)) {
# PEPP_df <- PEPP2_df %>% 
# mutate(str_c('dsfs_', i) = case_when(!is.na(str_c('SAPS_', i)) ~ coalesce(str_c('dsfs_', i), str_c('datedue', i))))
# }

  # Compute individually varying time of observation t0:t24 
TSCORES_df <-  PEPP2_df %>%
  mutate (t0 = 0) %>%
  mutate(t1 = as.numeric(difftime(dsfs_1, dsfs_0, 'days')) / (365.25 / 12)) %>%
  mutate(t2 = as.numeric(difftime(dsfs_2, dsfs_0, 'days')) / (365.25 / 12)) %>%
  mutate(t3 = as.numeric(difftime(dsfs_3, dsfs_0, 'days')) / (365.25 / 12)) %>%
  mutate(t6 = as.numeric(difftime(dsfs_6, dsfs_0, 'days')) / (365.25 / 12)) %>%
  mutate(t9 = as.numeric(difftime(dsfs_9, dsfs_0, 'days')) / (365.25 / 12)) %>%
  mutate(t12 = as.numeric(difftime(dsfs_12, dsfs_0, 'days')) / (365.25 / 12)) %>%
  mutate(t18 = as.numeric(difftime(dsfs_18, dsfs_0, 'days')) / (365.25 / 12)) %>%
  mutate(t24 = as.numeric(difftime(dsfs_24, dsfs_0, 'days')) / (365.25 / 12))

#Not working
# library(lubridate)
# for (i in c(1, 2, 3, 6, 9, 12, 18, 24)) {
# realtime_df <-  realtime_df %>%
# mutate(str_c('t', i) = as.numeric(difftime((str_c('dsfs_', i)), dsfs_0, 'days')) /(365.25/12))
# }

  # Replace missing with mean timescores 
TSCORES_df <- TSCORES_df %>%
  subset(n == 1 & pin <= 857) %>% 
  mutate(t1= case_when(!is.na(SAPS_1) ~ replace_na(t1, mean(t1, na.rm=TRUE)))) %>% 
  mutate(t2= case_when(!is.na(SAPS_2) ~ replace_na(t2, mean(t2, na.rm=TRUE)))) %>% 
  mutate(t3= case_when(!is.na(SAPS_3) ~ replace_na(t3, mean(t3, na.rm=TRUE)))) %>% 
  mutate(t6= case_when(!is.na(SAPS_6) ~ replace_na(t6, mean(t6, na.rm=TRUE)))) %>% 
  mutate(t9= case_when(!is.na(SAPS_9) ~ replace_na(t9, mean(t9, na.rm=TRUE)))) %>% 
  mutate(t12= case_when(!is.na(SAPS_12) ~ replace_na(t12, mean(t12, na.rm=TRUE)))) %>% 
  mutate(t18= case_when(!is.na(SAPS_18) ~ replace_na(t18, mean(t18, na.rm=TRUE)))) %>% 
  mutate(t24= case_when(!is.na(SAPS_24) ~ replace_na(t24, mean(t24, na.rm=TRUE)))) %>% 
  select(c('pin', t))

PEPP2_df <- merge(PEPP2_df, TSCORES_df, by = 'pin', all.x = TRUE)

# # Not working
# for (i in t) {
#   PEPP2_df <- PEPP2_df %>%
#   mutate(!!i := replace_na(strc_c(i, mean(i))))
# }


### Total scores 
library(stringr)
for (i in c(0, 1, 2, 3, 6, 9, 12, 18, 24)) {
  PEPP2_df <- PEPP2_df %>%
    mutate(!!str_c("SAPS_", i) :=
             rowSums(across(all_of(str_c(
               c("sap7_", "sap20_", "sap25_", "sap34_"), i
             ))))) %>%
    mutate(!!str_c("SANS_", i) :=
             rowSums(across(all_of(str_c(
               c("sap8_", "sap13_", "sap17_", "sap22_"), i
             ))))) %>%
    mutate(!!str_c("HAS_", i) :=
             rowSums(across(all_of(str_c(
               "ha", c(1:13), '_', i)
             )))) %>%
    mutate(!!str_c("CDS_", i) :=
             rowSums(across(all_of(str_c(
               "cd", c(1:9), '_', i)
             )))) %>%
    mutate(!!str_c("YMRS_", i) :=
             rowSums(across(all_of(str_c(
               "ymrs", c(1:11), '_', i)
             ))))
}

### Compute missings count per outcome 
  PEPP2_df <- PEPP2_df %>% # could use rowwise() and sum()
  mutate(miss_SOFAS = rowSums(is.na(across(SOFAS)))) %>%
  mutate(miss_SAPS =rowSums(is.na(across(SAPS)))) %>%
  mutate(miss_SANS =rowSums(is.na(across(SANS))))

### Save new dataset 
  # SAV file 
  write_sav(PEPP2_df, paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_', today(), '.sav',  sep=""))
  
  # CSV file 
write_csv(PEPP2_df, paste0('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/PEPP2_', today(), '.csv'))

### Subset dataset 
  # SD 
SD_df <- PEPP2_df %>%
  subset(pin <= 857) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, SOFAS, HAS, CDS, YMRS, K, CP, SR_C, SR_BY, t)

  # Traj 
SAPNS_df <- PEPP2_df %>%
  subset(miss_SAPS <= 4 & n == 1) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, SOFAS, HAS, CDS, YMRS, K, CP, SR_C, SR_BY, t)

SOFAS_df <- PEPP2_df %>%
  subset(miss_SOFAS <= 1 & n == 1) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, SOFAS, HAS, CDS, YMRS, K, CP, SR_C, SR_BY, t)

  # MI 
df2imput <- PEPP2_df %>% 
  subset(n == 1 & pin <= 857) %>%
  select(c('pin', SD_num, SD_cat, items)) #impute raw scores is thought to be better than transformed scores


str()

### Reshape wide to long form 
SAPNS_Ldf <- pivot_longer(SAPNS_df,
            cols =c(SAPS, SANS, PSR, NSR, HAS, CDS, YMRS), 
            names_to = c('.value', 'time'),
            names_pattern = '(.+)_(\\d+)'
)


# DESCRIPTIVES STATS ####
### Df description 
summary <- summary(SD_df)
dfSummary()

### Sociodemographics 
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
  out = "return",
)

SOFAS_tb <- sumtable(
  data = SOFAS_df,
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group = 'K_SOFAS',
  group.test = TRUE,
  out = "return",
)

SAPS_tb <- sumtable(
  data = SAPNS_df,
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group = 'K_SAPS',
  group.test = TRUE,
  out = "return",
)

SANS_tb <- sumtable(
  data = SAPNS_df,
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group = 'K_SANS',
  group.test = TRUE,
  out = "return",
)


# MISSING DATA ANALYSES ####
### Little's MCAR 
install.packages("https://cran.r-project.org/src/contrib/Archive/MissMech/MissMech_1.0.2.tar.gz", repos=NULL, type="source")
library(MissMech)

df2imput %>% 
  select("vars") %>% 
  TestMCARNormality

### Average percentage of missing data 
library(misty)
na.descript(PEPP2_df)

### Percent of missing data per columns 
miss <- {unlist(lapply(SAPNS_df, function(x) sum(is.na(x)))) / nrow(SAPNS_df) * 100} %>% view()


# MULTIPLE IMPUTATION ####
library(mice)
df2imput <- mice(df2imput, maxit = 0)
pred <- df2imput$predictorMatrix #variables included in the prediction moded
meth <- df2imput$method #method choose for imputation per variables
df2imput$loggedEvents

### Imputation method 
  # Instructions 
  # PMM (Predictive Mean Matching)  – For numeric variables
  # logreg(Logistic Regression) – For Binary Variables( with 2 levels)
  # polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
  # Proportional odds model (ordered, >= 2 levels)


meth <- make.method(df2imput)# make default method

poly <- c() #add vars, logical expression
meth[poly] <- "polr"
log <- c()
meth[log] <- "logreg"
poly2 <- c()
meth[poly2] <- "polyreg"

removed <- c(names(subset(miss, miss > 50))) #exclude variables from imputation
meth[removed] <- ""
as.data.frame(meth)

### Imputation predictors 
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
pred[removed, c("dx_spect", "ageonset")] <- 0 #exclude variables not imputed from prediction matrix


pred %>% rowSums %>% mean

### Compute imputation 
imput_df <- mice(
  df2imput,
  m = 2, #m: number of multiple imputations = average percentage of missing data to impute (up to 50%)
  maxit = 1, #m: number of iterations = 5-20
  predictorMatrix = pred,
  method = meth,
  seed = 22,
  print =  FALSE
)

### Compute passive imputation 
library(glue)
for (i in c(0, 1, 2, 3, 6, 9, 12, 18, 24)) {
#meth[str_c('SAPS_', i)] <- str_c('"~I(rowSums(across(all_of(', str_c(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), i), ')))))"')
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
    mutate(!!str_c("SAPS_", i) :=
             rowSums(across(all_of(str_c(
               c("sap7_", "sap20_", "sap25_", "sap34_"), i
             ))))) %>%
    mutate(!!str_c("SANS_", i) :=
             rowSums(across(all_of(str_c(
               c("sap8_", "sap13_", "sap17_", "sap22_"), i
             ))))) %>%
    mutate(!!str_c("HAS_", i) :=
             rowSums(across(all_of(str_c(
               "ha", c(1:13), "_", i)
             )))) %>%
    mutate(!!str_c("CDS_", i) :=
             rowSums(across(all_of(str_c(
               "cd", c(1:9), "_", i)
             )))) %>%
    mutate(!!str_c("YMRS_", i) :=
             rowSums(across(all_of(str_c(
               "ymrs", c(1:11), "_", i)
             ))))
}

### Check Imputation 
warnings <- imput_df$loggedEvents

densityplot(imput_df)
stripplot(imput_df)

comp1_df <- complete(imput_df, 1) #return the 1st of the imputed dataset
na.descript(comp1_df)

miss_comp1 <- unlist(lapply(comp1_df, function(x) sum(is.na(x)))) / nrow(comp1_df) * 100
miss_comp1 <- sort(miss_comp1, decreasing = TRUE)
as.data.frame(miss_comp1) 

library(sjmisc)
merged_df <- merge_imputations(df2imput, imput_df, summary = "dens")

### Analyses with imputed datasets 
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

DUP_posthoc <- with(imput_df, pairwise.t.test(duponset, K_SANS, paired = FALSE, p.adjust.method = "bonferroni"))
posthoc_p <- as.list(DUP_posthoc$analyses)
summ(pool(posthoc_p))


age_anova <- mi.anova(imput_df,"ageentry ~ K_SANS")
FIQ_anova <- mi.anova(imput_df, "FIQ ~ K_SANS")



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
  method = "ML", 
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
runModels('/Users/olivierpercie/Desktop/MplusLGM/MplusfromR/GMM1_i s-cub@0.inp')




# SAVE #### 
save.image(glue(getwd(), 'PEPP2_{today()}.RData', .sep = "/"))
