
library(magrittr)
library(haven)
library(dplyr)

#---------------#
#PREPARE DATASET
#---------------#

# Load dataset
PEPP2_df <- 
  paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/PEPP2/764K_05Nov2021.sav') %>%
    read_spss(user_na = FALSE, skip = 0, n_max = Inf)

# Define variables of interest
SD_num <- c('ageentry', 'educ_num', 'FIQ', 'holltotp', 'ageonset', 'duponset', 'PAS_tot2')
SD_cat <- c('gender', 'minority_status', 'marital2', 'housing_status', 'working_status', 'dx_spect', 'SUD')
SAPS <- c('SAPS_0', 'SAPS_1', 'SAPS_2', 'SAPS_3', 'SAPS_6', 'SAPS_9', 'SAPS_12', 'SAPS_18', 'SAPS_24')
SANS <- c('SANS_0', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24')
SOFAS <- c('SOFAS_0', 'SOFAS_12', 'SOFAS_24')
HAS <- c('HAS_0', 'HAS_1', 'HAS_2', 'HAS_3', 'HAS_6',  'HAS_9',  'HAS_12',  'HAS_18',  'HAS_24')
CDS <- c('CDS_0', 'CDS_1', 'CDS_2', 'CDS_3', 'CDS_6',  'CDS_9',  'CDS_12',  'CDS_18',  'CDS_24')
YMRS <-c('YMRS_0', 'YMRS_1', 'YMRS_2', 'YMRS_3', 'YMRS_6', 'YMRS_9', 'YMRS_12', 'YMRS_18', 'YMRS_24') 
PSR <- c('PSR_0', 'PSR_1', 'PSR_2', 'PSR_3', 'PSR_6', 'PSR_9', 'PSR_12', 'PSR_18', 'PSR_24')
NSR <- c('NSR_0', 'NSR_1', 'NSR_2', 'NSR_3', 'NSR_6', 'NSR_9', 'NSR_12', 'NSR_18', 'NSR_24')
MISC_num <- c('PSR_24C', 'NSR_24C')
MISC_cat <- c('n', 'PSR_BY3', 'NSR_BY3')
K <- c('K_SAPS', 'K_SANS', 'K_SOFAS')

#recode variables as factors or num
PEPP2_df[, c(SD_cat, K, PSR, NSR, MISC_cat)] <-lapply(PEPP2_df[, c(SD_cat, K, PSR, NSR, MISC_cat)], as.factor)
PEPP2_df[, c('pin', SD_num, SAPS, SANS, SOFAS, HAS, CDS, YMRS, CP, MISC_num, items)] <-lapply(PEPP2_df[, c('pin', SD_num, SAPS, SANS, SOFAS, HAS, CDS, YMRS, CP, MISC_num, items)], as.numeric)
CP <-  c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')
SXB <-  c('SAPS_0', 'SANS_0', 'SOFAS_0', 'HAS_0', 'CDS_0', 'YMRS_0')
# Add variable
vars <- "starts_with('dsfs')"
vars <- "starts_with('datedu'), starts_with('app'), 'dsofas_b', 'CRsofas12'"

addvar_df <-
  paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Databases/#1-992 (N=762) Jan2003-Jan2020/Merged_25Feb2022.sav') %>%
  read_spss(user_na = FALSE, col_select = c('pin', starts_with('datedu'), starts_with('app'), 'dsofas_b', 'CRsofas12'))

PEPP2_df <- merge(PEPP2_df, addvar_df, by = 'pin')

# Clean dataset
library(data.table)
PEPP2_df <- PEPP2_df %>%
  setNames(., gsub("_b","_0",names(.))) %>%
  setNames(.,gsub("_M","_",names(.))) %>%
  setNames(.,gsub("ROB","",names(.))) %>%

##Subset dataset
  #SD
SD_df <- PEPP2_df %>%
  subset(n == 1 & pin <= 857) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, num, SOFAS, HAS, CDS, YMRS, K, CP, MISC_cat)

  #Traj
SAPNS_df <- PEPP2_df %>%
  subset(miss_SAPS <= 4 & n_SAPNS == 1) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, num, SOFAS, HAS, CDS, YMRS, K, CP, MISC_num, MISC_cat)

SOFAS_df <- PEPP2_df %>%
  subset(miss_SOFAS <= 1 & n_SOFAS == 1) %>%
  select('pin', SD_num, SD_cat, SAPS, SANS, PSR, NSR, SOFAS, HAS, CDS, YMRS, K, CP, MISC_num, MISC_cat)

df2imput <- PEPP2_df %>% 
  subset(n == 1 & pin <= 857) %>%
  select(c('pin', SD_num, SD_cat, items)) #impute raw scores is thought to be better than transformed scores

str(df2imput)

#Reshape wid to long form
library(reshape2)
long_df <- reshape(
  df,
  direction = "long",
  idvar = c('pin'),
  new.row.names = 1:3645,
  varying = c(SAPS, SANS, PSR, NSR, HAS, CDS, YMRS),
  v.names = c('SAPS', 'SANS', 'PSR', 'NSR', 'HAS', 'CDS', 'YMRS'),
  timevar = 'time',
  times = c('0', '1', '2', '3', '6', '9', '12', '18', '24'),
  drop = SOFAS,
  sep = '_'
)

#-----------------#
#DESCRIPTIVES STATS
#-----------------#

# Sociodemographics
summary(SD_df)

library(psych)
describeBy(SAPNS_df ~ K_SAPS, skew = FALSE, ranges = FALSE)

library(kableExtra)
library(vtable)
SD_tb <- sumtable(
  data = SD_df,
  vars = c(SD, cat, 'SAPS_0', 'SANS_0', 'SOFAS_0'),
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
  data = subSAPNS_df,
  summ = c('notNA(x)', 'mean(x)', 'sd(x)'),
  summ.names = c('N', 'Mean / percentage', 'SD'),
  group = 'K_SANS',
  group.test = TRUE,
  out = "return",
)


#Average percentage of missing data
library(misty)
na.descript(df2imput)

#Percent of missing data per columns
miss <- unlist(lapply(df2imput, function(x) sum(is.na(x)))) / nrow(df2imput) * 100
miss <- sort(miss[miss > 0], decreasing = TRUE)
as.data.frame(miss) 

#-------------------#
#MULTIPLE IMPUTATION
#-------------------#

library(mice)
df2imput <- mice(SD_df, maxit = 0)
pred <- df2imput$predictorMatrix #variables included in the prediction moded
meth <- df2imput$method #method choose for imputation per variables
df2imput$loggedEvents

##Imputation method
#     - PMM (Predictive Mean Matching)  – For numeric variables
#     - logreg(Logistic Regression) – For Binary Variables( with 2 levels)
#     - polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
#     - Proportional odds model (ordered, >= 2 levels)

meth <- make.method(df2imput)

poly <- c() #add vars, logical expression
meth[poly] <- "polr"
log <- c()
meth[log] <- "logreg"
poly2 <- c()
meth[poly2] <- "polyreg"

removed <- c(CP, K) #exclude variables from imputation #names(subset(miss, miss > 25 | miss < 5))
meth[removed] <- ""
as.data.frame(meth)


###Imputation predictors - select 15-25 variables
#     - include all variables that appear in the complete-data model
#     - include the variables that are related to the nonresponse
#     - include variables that explain a considerable amount of variance
#     - Remove pred variables that have too many missing values within the subgroup of incomplete cases

pred <- make.predictorMatrix(df2imput)
pred <- quickpred(df2imput)

pred[, 'pin'] <-1

pred[, ] <- 0 #exclude variables to impute from prediction matrix
pred[, c(CP, K, 'pin', 'n', 'ageonset', 'NSR_24C', 'PSR_24C')] <-0 #exclude predictors 
pred[, c(sxb, SD_num, SD_cat)] <- 1 #include predictors

print(pred) 

##Compute imputation
imput_df <- mice(
  df2imput,
  m = 1, #m: number of multiple imputations = average percentage of missing data to impute
  maxit = 1, #m: number of iterations = 5-20
  predictorMatrix = pred,
  method = meth,
  seed = 22,
  print =  FALSE
)

##Check Imputation
imput_df$loggedEvents

densityplot(imput_df)
stripplot(imput_df)

comp1_df <- complete(imput_df, 1) #return the 1st of the imputed dataset
na.descript(comp1_df)

miss_comp1 <- unlist(lapply(comp1_df, function(x) sum(is.na(x)))) / nrow(comp1_df) * 100
miss_comp1 <- sort(miss_comp1, decreasing = TRUE)
as.data.frame(miss_comp1) 

library(sjmisc)

merged_df <- merge_imputations(df2imput, imput_df, summary = "dens")


#Group comparisons
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

lm(CP1_SOFAS ~ duponset, sub_df) %>% summ()

lm(CP1_SOFAS ~ gender, sub_df) %>% summ()

DUP_posthoc <- with(imput_df, pairwise.t.test(duponset, K_SANS, paired = FALSE, p.adjust.method = "bonferroni"))
posthoc_p <- as.list(DUP_posthoc$analyses)
summ(pool(posthoc_p))

#SAPS
age_anova <- mi.anova(imput_df,"ageentry ~ K_SANS")
FIQ_anova <- mi.anova(imput_df, "FIQ ~ K_SANS")
educ_anova <- mi.anova(imput_df, "educ_num ~ K_SANS")
holl_anova <- mi.anova(imput_df, "holltotp ~ K_SANS")
PAS_anova <- mi.anova(imput_df, "PAS_tot2 ~ K_SANS")
DUP_anova <- mi.anova(imput_df,"duponset ~ K_SANS")
onset_anova <- mi.anova(imput_df, "ageonset ~ K_SANS")
SAPS_anova <- mi.anova(imput_df, "SAPS_0 ~ K_SANS")

#Mixed models
library(brms)
library(nlme)

SANS_lmm <- lme(
    fixed = SANS ~ K_SAPS + time + K_SAPS:time, 
    random = ~1|pin,
    #correlation = corAR1(), #0, form = ~time|pin
    data = long_df, 
    method = "ML", 
    na.action = na.pass
    )
summary(SANS_lmm)

SANS_lm <- lm(SANS ~ K_SAPS + time + K_SAPS:time,
              data = long_df,
              na.action = na.omit)
summary(SANS_lm)

# Regressions
library(lm.beta)
library(confint)


lm(SOFAS_24 ~ CP2_SAPS, subSOFAS_df) %>% summ()  
lm(SOFAS_24 ~ CP1_SANS, subSOFAS_df) %>% lm.beta()  

lm(scale(SOFAS_24) ~ scale(CP2_SAPS), subSOFAS_df) %>% confint()
lm(scale(SOFAS_24) ~ scale(CP1_SANS), subSOFAS_df) %>% confint()

