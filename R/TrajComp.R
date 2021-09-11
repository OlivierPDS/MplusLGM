


# Load dataset
#spss.sav = paste(getwd(), 'data', 'Dataset533_03Sept2021.sav', sep = '/')
library(haven)
spss.sav = paste('/Users/olivierpercie/OneDrive - McGill University/CRISP_Lab/LTOS/Data/Datasets/2-year Follow-up/533K_10Sept2021.sav')
data533K_df <- read_spss(spss.sav, user_na = FALSE, skip = 0, n_max = Inf)
sub405K_df <- subset(data533K_df, miss_SAPS <= 4)
sub345K_df <- subset(data533K_df, miss_SOFAS <= 1)

# Define variables of interest
SD <- c('ageentry', 'educ_num', 'FIQ', 'holltotp', 'ageonset', 'duponset', 'PAS_tot2')
SAPS <- c('SAPS_b', 'SAPS_1', 'SAPS_2', 'SAPS_3', 'SAPS_6', 'SAPS_9', 'SAPS_12', 'SAPS_18', 'SAPS_24')
SANS <- c('SANS_b', 'SANS_1', 'SANS_2', 'SANS_3', 'SANS_6', 'SANS_9', 'SANS_12', 'SANS_18', 'SANS_24')
SOFAS <- c('sofas_b', 'sofas_12', 'sofas_24')
HAS <- c('HAS_B', 'HAS_1', 'HAS_2', 'HAS_3', 'HAS_6',  'HAS_9',  'HAS_12',  'HAS_18',  'HAS_24')
CDS <- c('CDS_B', 'CDS_1', 'CDS_2', 'CDS_3', 'CDS_6',  'CDS_9',  'CDS_12',  'CDS_18',  'CDS_24')
YMRS <-c('YMRS_B', 'YMRS_1', 'YMRS_2', 'YMRS_3', 'YMRS_6', 'YMRS_9', 'YMRS_12', 'YMRS_18', 'YMRS_24') 
PSR <- c('PSR_B', 'PSR_M1', 'PSR_M2', 'PSR_M3', 'PSR_M6', 'PSR_M9', 'PSR_M12', 'PSR_M18', 'PSR_M24')
NSR <- c('NSR_B', 'NSR_M1', 'NSR_M2', 'NSR_M3', 'NSR_M6', 'NSR_M9', 'NSR_M12', 'NSR_M18', 'NSR_M24')
num <- c('PSR_24C', 'NSR_24C')
cat <- c('gender', 'minority_status', 'marital2', 'newliving', 'newwork', 'dx_b2', 'SUD', 'PSR_BY3', 'NSR_BY3')
K <- c('K_SAPS', 'K_SANS', 'K_SOFAS')
CP <-  c('CPROB1_SOFAS', 'CPROB2_SOFAS', 'CPROB1_SAPS', 'CPROB2_SAPS', 'CPROB1_SANS', 'CPROB2_SANS', 'CPROB3_SANS')

#Subset dataset
sub_df <- sub345K_df[, c('pin', SD, cat, SAPS, SANS, PSR, NSR, num, SOFAS, HAS, CDS, YMRS, K, CP)]
sub_df <- sub405K_df[, c('pin', SD, cat, SAPS, SANS, PSR, NSR, num, SOFAS, HAS, CDS, YMRS, K, CP)]
#dataset2imput <- SAPS405_K %>% select(c('pin', sap1_b:ymrs11_24)) #impute raw scores is thought to be better than transformed scores

#recode categorical variables as factors
sub_df[, c(cat, K)] <- lapply(sub_df[, c(cat, K)], as.factor)
sub_df[, c('pin', SD, SAPS, SANS, PSR, NSR, num, SOFAS, HAS, CDS, YMRS, CP)] <- lapply(sub_df[, c('pin', SD, SAPS, SANS, PSR, NSR, num, SOFAS, HAS, CDS, YMRS, CP)], as.numeric)
str(sub_df)

#long_df[, c('pin', SD, 'SAPS', 'SANS', 'PSR', 'NSR', num, 'HAS', 'CDS', 'YMRS', 'time', K)] <- lapply(long_df[, c('pin', SD, 'SAPS', 'SANS', 'PSR', 'NSR', num, 'HAS', 'CDS', 'YMRS', 'time', K)], as.numeric)
#str(long_df)

df2imput <- sub_df

#reshape wid to long form
library(reshape2)
long_df <- reshape(sub_df, 
                direction="long", 
                idvar=c('pin'),
                new.row.names = 1:3645,
                varying=c(SAPS, SANS, PSR, NSR, HAS, CDS, YMRS), 
                v.names=c('SAPS', 'SANS', 'PSR', 'NSR', 'HAS', 'CDS', 'YMRS'), 
                timevar='time', 
                times=c('0', '1', '2', '3', '6', '9', '12', '18', '24'), 
                drop=SOFAS,
                sep = '_')

#Descriptives stats
summary(df2imput)

library(psych)
describeBy(df2imput~K_SAPS, skew=FALSE, ranges=FALSE)

library(kableExtra)
library(vtable)

SANSK_tb <- sumtable(
  data = sub_df,
  summ = c('notNA(x)','mean(x)','sd(x)'),
  summ.names = c('N','Mean / percentage','SD'),
  group = 'K_SANS',
  group.test = TRUE,
  out = "return",
  )

#Percent of missing data per columns
miss <- unlist(lapply(df2imput, function(x) sum(is.na(x))))/nrow(df2imput)*100
sort(miss[miss > 0], decreasing = TRUE)
print(miss) 

#Multiple Imputation
  #PMM (Predictive Mean Matching)  – For numeric variables
  #logreg(Logistic Regression) – For Binary Variables( with 2 levels)
  #polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
  #Proportional odds model (ordered, >= 2 levels)

library(mice)

imput_df <- mice(df2imput, maxit=0)
predM <- imput_df$predictorMatrix #variables included in the prediction model
meth <- imput_df$method #method choose for imputation per variables
print(meth)


predM[,] <- 0 #exclude all variables from the prediction model 
predM[, c(SD, cat, "SAPS_b", "SANS_b", "sofas_b", "HAS_B", "CDS_B", "YMRS_B", K )] <- 1 #include variables to the prediction model
predM[,'ageonset'] <- 0 #exclude all variables from the prediction model 

# change imputation method
poly <- c()
log <- c()
poly2 <- c()
 


meth[poly] <- "polr"
meth[log] <- "logreg"
meth[poly2] <- "polyreg"

library(dbplyr)
library(magrittr)

#remove variable from imputation
imput_df$method[names(subset(miss, miss > 25 | miss < 5))] <- ""
meth <- imput_df$method



#Compute imputation
imput_df <- mice(df2imput, m = 20, maxit=20,
             predictorMatrix = predM, 
             method = meth, print =  FALSE)

densityplot(imput_df)

comp1_df <- complete(imput_df, 1) #return one of the imputed dataset
comp_df <- complete(imput_df) #return the original dataset

miss_imput <- unlist(lapply(comp1_df, function(x) sum(is.na(x))))/nrow(comp1_df)*100
print(miss_imput)
sort(miss_imput[miss_imput > 0], decreasing = TRUE)
as.data.frame(miss_imput)

library(sjmisc)

merged_df <- merge_imputations(df2imput, imput_df, summary = "dens")

library(jtools)
#Group comparisons
#t-test
DUP_t <- with(imput_df, t.test(duponset ~ K_SAPS))
summary(pool(DUP_t$analyses))


library(MKmisc)
library(limma)

attach(imput_df$imp)
mi.t.test(imput_df$data, x="duponset", y="K_SAPS", var.equal = FALSE)
detach(imput_df$imp)

#ANOVA

DUP_lm <- with(imput_df, lm(CPROB1_SOFAS ~ duponset))
summ(pool(DUP_lm))

lm(CPROB1_SOFAS ~ duponset, sub_df) %>% summ()


DUP_posthoc <- with(imput_df, pairwise.t.test(duponset, K_SANS, paired = FALSE, p.adjust.method = "bonferroni"))
posthoc_p <- as.list(DUP_posthoc$analyses)
summ(pool(posthoc_p))



#SAPS
library(miceadds)
age_anova <- mi.anova(imput_df,"ageentry ~ K_SANS")
FIQ_anova <- mi.anova(imput_df, "FIQ ~ K_SANS")
educ_anova <- mi.anova(imput_df, "educ_num ~ K_SANS")
holl_anova <- mi.anova(imput_df, "holltotp ~ K_SANS")
PAS_anova <- mi.anova(imput_df, "PAS_tot2 ~ K_SANS")
DUP_anova <- mi.anova(imput_df,"duponset ~ K_SANS")
onset_anova <- mi.anova(imput_df, "ageonset ~ K_SANS")
SAPS_anova <- mi.anova(imput_df, "SAPS_b ~ K_SANS")

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

#X2
attach(imput_df)
X2 <- with(imput_df, chisq.test(K_SAPS, gender, correct = FALSE))
summary(X2)
summary(pool(X2))
detach(imput_df)
