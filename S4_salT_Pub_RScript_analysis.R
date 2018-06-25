#Supplemental section 4
#R script for Magid K., Chatterton RT, Ahamed FU, Bentley GR, "Childhood ecology influences salivary testosterone and pubertal age of Bangladeshi UK migrant men"

#Packages used in analysis----------------------------------------------------------------------------------------------------------------------------
library(multcomp)
library(foreign, pos=4)
library(reshape)
library(car)
library(RCurl)
library(Hmisc)
library(mitools)
#packages used in plotting and tables:
library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
library(lsmeans)
library(stargazer)

#Run following script to load:
#download from source file
repro.data <- read.csv(text = getURL("https://raw.githubusercontent.com/kessonovitch/BHAI_Data/master/reproData.csv"))
#or load from manuscript supplemental file "S3_reproData"
#repro.data <- read.csv("reproData.csv")
#relevel factor variables to make sedentees or oldest migrant groups the reference levels (if necessary)
repro.data$residence19pub <- factor(repro.data$residence19pub, levels=c("Bangladeshi sedentees", "Adult migrants", "Child migrants", "Second generation migrants", "British European"))

#inspect
# str(repro.data)
#for list of variables write.csv(names(repro.data), file = "varList.csv", row.names = FALSE)

#to include imputed BMI values
#imputation at population mean
repro.data$MssBMI <- ifelse(is.na(repro.data$BMI), mean(repro.data$BMI, na.rm = T), repro.data$BMI)
repro.data$z.mssbmi=as.vector(scale(repro.data$MssBMI))

#BMI imputed from package "mi", for all groups
ci1.mig.imp.data <- read.csv(text = getURL("https://raw.githubusercontent.com/kessonovitch/BHAI_Data/master/imp_bmi_data2_ci1"))
ci2.mig.imp.data <- read.csv(text = getURL("https://raw.githubusercontent.com/kessonovitch/BHAI_Data/master/imp_bmi_data2_ci2"))
ci3.mig.imp.data <- read.csv(text = getURL("https://raw.githubusercontent.com/kessonovitch/BHAI_Data/master/imp_bmi_data2_ci3"))
ci4.mig.imp.data <- read.csv(text = getURL("https://raw.githubusercontent.com/kessonovitch/BHAI_Data/master/imp_bmi_data2_ci4"))

#For list of imputed chain databases
ci.bmi.ls <- list(ci1.mig.imp.data, ci2.mig.imp.data, ci3.mig.imp.data, ci4.mig.imp.data)
repro.data.i <- imputationList(ci.bmi.ls)
summary(ci1.mig.imp.data)

#BMI imputed from package "mi", for CHI groups only
ci1.mig.imp.data.chi <- subset(ci1.mig.imp.data, residence19pub=="Child migrants")
ci2.mig.imp.data.chi <- subset(ci2.mig.imp.data, residence19pub=="Child migrants")
ci3.mig.imp.data.chi <- subset(ci3.mig.imp.data, residence19pub=="Child migrants")
ci4.mig.imp.data.chi <- subset(ci4.mig.imp.data, residence19pub=="Child migrants")

#For intra-group analysis with CHI, subset each chain database to include only CHI
ci.bmi.chi.ls <- list(ci1.mig.imp.data.chi, ci2.mig.imp.data.chi, ci3.mig.imp.data.chi, ci4.mig.imp.data.chi)
mig.19pub.data.i <- imputationList(ci.bmi.chi.ls)

#inspect to verify imputed data 
#str(repro.data.i)
#str(mig.19pub.data.i)

#SUBSETS---------------------------------------------------------------------------------------------------------------------------------------

#subset databases of only those who migrated prior to completion of puberty
mig.19pub.data <- subset(repro.data, repro.data$residence19pub=="Child migrants")

#subset of data of child migrants who migrated before age 9
mig.8.data <- subset(repro.data, age.8.19.mig=="Birth-9y")

#create a database of only those who migrated as adults 
mig.adu.data <- subset(repro.data, repro.data$residence19pub=="Adult migrants")
#create a database of only those who migrated as adults and age recruit < 40 
mig.adu40a.data <- subset(repro.data, repro.data$residence19pub=="Adult migrants"& AgeRecruit <= 40)
#create a database of only those who migrated as adults and age recruit > 40
mig.adu40b.data <- subset(repro.data, repro.data$residence19pub=="Adult migrants"& AgeRecruit > 40)
#create a database of only those who migrated as adults 18yo cutoff 
mig.adu18.data <- subset(repro.data, repro.data$residence18=="Adult migrants")

#create database of only UK resident groups (divided at ≤19)
ukres19.data <- subset(repro.data, residence19pub=="Adult migrants" | residence19pub=="Child migrants" | residence19pub=="Second generation migrants" | residence19pub== "British European")
#create database of only ethnic Bengali UK resident groups
ukresbds19.data <- subset(repro.data, residence19pub=="Adult migrants" | residence19pub=="Child migrants" | residence19pub=="Second generation migrants")

#create a database of only those recruited prior to age 40, and those after age 40
age40a.data <- subset(repro.data, repro.data$AgeRecruit <= 40)
age40b.data <- subset(repro.data, repro.data$AgeRecruit > 40)

#LISTS----------------------------------------------------------------------------------------------------------------------------

#list of 2 (WAKE and BED) logged z. trans. salT varaibles
z.log.2salt <- repro.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
unt.2salt <- repro.data[,c("MeanS1D1D2", "MeanS3D1D2")]
unt.ukres.2salt <- ukres19.data[,c("MeanS1D1D2", "MeanS3D1D2")]
z.log.8.2salt <- mig.8.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.19.2salt <- mig.19pub.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.19.2salt.i <- ci1.mig.imp.data.chi[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.adu.2salt <- mig.adu.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.adu40a.2salt <- mig.adu40a.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.adu40b.2salt <- mig.adu40b.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
unt.40a.2salt <- age40a.data[,c("MeanS1D1D2", "MeanS3D1D2")]
unt.40b.2salt <- age40b.data[,c("MeanS1D1D2", "MeanS3D1D2")]
unt.ukres.40a.2salt <- subset(age40a.data, residence19pub=="Adult migrants" | residence19pub=="Child migrants" | residence19pub=="Second generation migrants" | residence19pub== "British European")[,c("MeanS1D1D2", "MeanS3D1D2")]
z.log.age40a.2salt <- age40a.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.age40b.2salt <- age40b.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]

#list of z. trans. puberty varaibles
z.pub <- repro.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.8.pub <- mig.8.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.19.pub <- mig.19pub.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.adu.pub <- mig.adu.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.ukbd.pub <- ukresbds19.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.sed.pub <- subset(repro.data, residence19pub=="Bangladeshi sedentees")[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.age40a.pub <- age40a.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.age40b.pub <- age40b.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]

#DESCRIPTIVES--------------------------------------------------------------------------------------------------------------------
#age differences between residence groups, migrants split at reported age at puberty or age 19
age.res <- lm(AgeRecruit~residence19pub, data=repro.data)
age.sres <- summary(age.res)
##All migrant groups significantly different from sedentees (ADU older, CHI and 2NG younger). EUR not different from SED

#post-hoc analysis
age.res.ph <- summary(glht(age.res, linfct=mcp(residence19pub="Tukey")))
## Age: ADU > CHI, 2NG; EUR > CHI > 2NG

#within migrants, does age at migration correlate with age at recruitment? 
#In adult migrants
cor.test(subset(repro.data, residence19pub=="Adult migrants")$z.log.age, as.vector(scale(log(subset(repro.data, residence19pub=="Adult migrants")$AgeMigUK))))
#no indication of correlation between age at recruitment and age at migration in Adult migrants
#n.s. correlation:
cor.test(subset(repro.data, residence19pub=="Child migrants")$AgeRecruit, as.vector(scale(log(subset(repro.data, residence19pub=="Child migrants")$AgeMigUK))))
#correlated, suggests the effects of peak migration.

#note a difference in age at recruitment by two sample t-test
mig.19pub.data$age.8.19.mig <- droplevels(mig.19pub.data)$age.8.19.mig
cbind(mig.19pub.data$pub.compos, mig.19pub.data$AgeMigUK)
t.test(mig.19pub.data$AgeRecruit~mig.19pub.data$age.8.19.mig)
##therefore the mean age at recruitment mean in group 9-19y (36.9) mean in group Birth-9y (27.95) is different betweent these migration cohorts t = 3.09, df = 33.62, p-value = 0.004029. 
##(those who migrated at earlier ages are sig. younger, likely reflective of the historical patterns of migration: peak migration of child migrants occured around the same time).
age40a.res <- lm(AgeRecruit~residence19pub, data=age40a.data)
age40a.sres <- summary(age40a.res)
##Within restricted <40 age cohort, significant differences between sedentees (ADU older, 2NG younger). EUR and CHI not different from SED
#post-hoc analysis
age40a.res.ph <- summary(glht(age40a.res, linfct=mcp(residence19pub="Tukey")))
## Age: ADU > CHI, 2NG, EUR; EUR > 2NG

#bmi differences between residence groups
bmi.res <- lm(BMI~residence19pub, data=repro.data)
bmi.sres <- summary(bmi.res)
##All migrant groups significantly different from sedentees (all higher).
#post-hoc analysis
bmi.res.ph <- summary(glht(bmi.res, linfct=mcp(residence19pub="Tukey")))
##No differences in BMI bewteen men resident in the UK

#height differences between residence groups, split at age 19
height.res <- lm(Height~residence19pub, data=repro.data)
height.sres <- summary(height.res)
confint(height.res)
hist(repro.data$Height)
##all but adult migrants are taller than sedentees.
#post-hoc analysis
height.res.ph <- summary(glht(height.res, linfct=mcp(residence19pub="Tukey")))
confint(height.res.ph)
coefficients(height.res.ph)
summary(model.frame(height.res))
##EUR>2NG>CHI=ADU. No difference between CHI and ADU
##for reporting exact p value of EUR-2NG comparison in text (0.00002): height.res.ph$test$pvalues[9]

#height differences between residence groups, split at age 19, adjusting for secular trend with age at recruitment
height.age.res <- lm(z.height~z.log.age+residence19pub, data=repro.data)
height.age.sres <- summary(height.age.res)
##adjusting for age, all groups are taller than sedentees.
#post-hoc analysis
height.age.res.ph <- summary(glht(height.age.res, linfct=mcp(residence19pub="Tukey")))
##EUR>2NG>CHI=ADU. No difference between CHI and ADU

#do groups differ from sedentees in their relationship between height and age (examine interaction between age at recruitment and height)
height.ageint.res <- lm(z.height~z.log.age*residence19pub, data=repro.data)
height.ageint.sres <- summary(height.ageint.res)
confint(height.ageint.res)
summary(model.frame(height.ageint.res))
##significant interaction between age at recruitment and being in child migrant group, compared to sedentees.

#secular trends in height of men who shared same childhood ecology?
height.bdadu.res <- lm(z.height~z.log.age, data=subset(repro.data, ukbd.born.adu=="Reached adulthood in Bangladesh"))
height.bdadu.sres <- summary(height.bdadu.res)
confint(height.bdadu.res)
height.ukadu.res <- lm(z.height~z.log.age, data=subset(repro.data, ukbd.born.adu=="Reached adulthood in UK"))
height.ukadu.sres <- summary(height.ukadu.res)
confint(height.ukadu.res)
##in both groups, no secular trend in height.

#weight differences between residence groups, split at age 19
weight.res <- lm(Weight~residence19pub, data=repro.data)
weight.sres <- summary(weight.res)
##all groups are heavier than sedentees.
#post-hoc analysis
weight.res.ph <- summary(glht(weight.res, linfct=mcp(residence19pub="Tukey")))
##EUR>all groups. No difference between all other migrant groups

#is there a secular trend/age effect on recalled age at puberty?
pub.res <- lm(z.pub.compos~z.log.age, data=repro.data)
pub.sres <- summary(pub.res)
confint(pub.res)
##overall, older men recall age at puberty later. This may be secular trend, systematic recall bias or developmental effect
pub.bdadu.res <- lm(z.pub.compos~z.log.age, data=subset(repro.data, ukbd.born.adu=="Reached adulthood in Bangladesh"))
pub.bdadu.sres <- summary(pub.bdadu.res)
confint(pub.bdadu.res)
pub.ukadu.res <- lm(z.pub.compos~z.log.age, data=subset(repro.data, ukbd.born.adu=="Reached adulthood in UK"))
pub.ukadu.sres <- summary(pub.ukadu.res)
#in both groups sharing similar childhood conditions, older men recall age at puberty later, suggesting secular trend or a systematic recall bias.

#MODELS---------------------------------------------------------------------------------------------------------------------------

#model1.1: sd units (log)age and bmi as covariates
#create function
#"age.bmi.lm" function for model1
age.bmi.lm <- function(x, y, db){
  lm(x~z.log.age+z.bmi+y, data=db)	
}

#model1.2: sd units (log)age and (imputed)bmi as covariates. 
#create function
#"age.mss.bmi.lm" function for model1, imputed values for within CHI analysis only
age.mss.bmi.lm <- function(x, y, db){
  lm(x~z.log.age+z.mssbmi+y, data=db)	
}

#Model 2.1, for migrants only: number of years "nyu" in the UK
## note: inclusion of age and number of years in UK in same model
# nyu.age.bmi.lm <- function(x, y, db){
# lm(x ~NumYearsUK+z.log.age+z.bmi+y, data=db)	
# }
# is not possible as they are collinear. 
# running the above model with vif returns the following values: NYU: 79.13; z.log.age: 73.8; z.bmi: 1.0; z.AgeMigUK: 18.19
# Either use one or the other. 

#"nyu.bmi.lm" function for model 2.1 with number of years in UK and sd units of bmi
nyu.bmi.lm <- function(x, y, db){
  lm(x ~NumYearsUK+z.bmi+y, data=db)	
}

#model 2.2 with number of years in UK and sd units of (imputed) bmi,
#"nyu.mssbmi.lm" function for imputed values for within CHI analysis only
nyu.mssbmi.lm <- function(x, y, db){
  lm(x ~NumYearsUK+z.mssbmi+y, data=db)	
}

#model3.1, z.log.age as only covariate, for puberty comparisons
#create function
#"age.lm" function for model1, age, no other variables
age.lm <- function(x, y, db){
  lm(x~z.log.age+y, data=db)	
}

#model3.2, age at recruitment (untransformed) as only covariate, for puberty comparisons
#create function
#"age.lm" function for model1, age, no other variables
agerec.lm <- function(x, db){
  lm(x~AgeRecruit, data=db)	
}

#model3.2, age at recruitment and bmi (untransformed), for comparisons with published declines
#create function
#"age.lm" function for model1, age, no other variables
agerec.bmi.lm <- function(x, db){
  lm(x~AgeRecruit+BMI, data=db)	
}

#Model 4, for those who migrated: number of years "nyu" in the UK lm(phys ~z.NumYearsUK, data=mig.19pub.data)
## note: inclusion of age at migration and number of years in UK in same model
# nyu.bmi.lm <- function(x, y, db){
# lm(x ~NumYearsUK+z.log.age+z.bmi+y, data=db)	
# }
# is not possible as they are collinear. Either use one or the other. According to adult ecology hypothesis, number of years in the UK will not have an influence on traits that become fixed at cessation of puberty, therefore age will be used as covariate for height analysis, number of years in UK for MUA, BMI analysis

#Model 4.1 sd units age at migration and sd (log) age as covariates, for puberty regression (within adult migrants only)
#"amu.lm" function
amu.age.lm <- function(x, y, db){
  lm(x ~z.AgeMigUK+z.log.age+y, data=db)	
}

#Model 4.2 sd units Number of years in UK and age at migration as covariates, for puberty regression (within child migrants only)
#"nyu.lm" function
nyu.amu.lm <- function(x, y, db){
  lm(x ~NumYearsUK+z.AgeMigUK+y, data=db)	
}

#Model 4.3 age at recruitment for puberty regressions ANCOVA
agerec.pub.aov <- function(x, y, db){
  aov(x ~AgeRecruit+y, data=db)	
}

#Model 5.1, age at recruitment ANCOVA
agerec.aov <- function(x, y, db){
  aov(x ~AgeRecruit+residence19pub, data=db)	
}

#Model 5.2, age at recruitment interaction ANCOVA
agerecxres.aov <- function(x, y, db){
  aov(x ~AgeRecruit*residence19pub, data=db)	
}


#SALIVARY TESTOSTERONE HYPOTHESES-------------------------------------------------------------------------------------------------
#Hypothesis 1: Cohorts separated by ethnicity and developmental exposure to ecological conditions will differ in salivary T-----------

#Hypot 1.1 SalT by residence: sd units (log)age and bmi as covariates-----
res.2salt.res <- lapply(z.log.2salt, y=repro.data$residence19pub, db=repro.data, age.bmi.lm)
res.2salt.sres <- lapply(res.2salt.res, summary)
#sample sizes: summary(model.frame(res.2salt.res$z.log.meanS1D1D2)$y)
## for Wake and Bed samples CHI and 2NG salT is higher compared to SED, Waking ADU salT is sig. lower than SED, BMI also sig. covariate

#for post-hoc multiple comparison Tukey correction of all-pair multiple comparison
res.salt1.res.ph <- summary(glht(res.2salt.res$z.log.meanS1D1D2, linfct=mcp(y="Tukey")))
## Wake: CHI > ADU; 2NG > ADU; 2NG > EUR.
confint(res.salt1.res.ph)
res.salt3.res.ph <- summary(glht(res.2salt.res$z.log.meanS3D1D2, linfct=mcp(y="Tukey")))
confint(res.salt3.res.ph)
#Bed: 2NG > ADU 

#Hypot 1.2 age at migration "z.AgeMigUK" as continuous predictor after adjusting for number of years in uk "nyu" and age at migration regressions in all migs---------
nyu.amu.2salt.res <- lapply(z.log.2salt, y=repro.data$z.AgeMigUK, db=repro.data, nyu.bmi.lm)
nyu.amu.2salt.sres <- lapply(nyu.amu.2salt.res, summary)
##Number of years in UK and age of migration significant negative predictors of salivary T for all migrants

#Hypotheses 2: Childhood age at migration is a predictor of adult salivary T------------

#Hypot 2.1.1 within CHI only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 19years "y19" regressions-----------
y19a.2salt.res <- lapply(z.log.19.2salt, y=mig.19pub.data$z.AgeMigUK, db=mig.19pub.data, age.bmi.lm)
y19a.2salt.sres <- lapply(y19a.2salt.res, summary)
##for Age Mig within CHI only, waking and bed time points, no sig covariates

#Hypot 2.1.2 within CHI only, age at migration "z.AgeMigUK" as continuous predictor who arrived before 19years "y19" regressions, with mean imputed BMI----------------
y19a.2salt.mssbmi.res <- lapply(z.log.19.2salt, y=mig.19pub.data$z.AgeMigUK, db=mig.19pub.data, age.mss.bmi.lm)
y19a.2salt.mssbmi.sres <- lapply(y19a.2salt.mssbmi.res, summary)
##Age Mig n.s. within CHI only, Waking (p=0.9) and Bed sample (p=0.069). Age sig. neg. covariate for Wake sample

#Hypot 2.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only--------
nyu.yom.amu.2salt.res <- lapply(z.log.19.2salt, y=mig.19pub.data$z.AgeMigUK, db=mig.19pub.data, nyu.bmi.lm)
nyu.yom.amu.2salt.sres <- lapply(nyu.yom.amu.2salt.res, summary)
##for Age Mig within Child migs only, number of years in the UK and bmi as covariates, waking n.s. but evening time point sig (p=0.17; p=0.03) sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK

#Hypot 2.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with imputed BMI---------
nyu.yom.amu.2salt.mssbmi.res <- lapply(z.log.19.2salt, y=mig.19pub.data$z.AgeMigUK, db=mig.19pub.data, nyu.mssbmi.lm)
nyu.yom.amu.2salt.mssbmi.sres <- lapply(nyu.yom.amu.2salt.mssbmi.res, summary)
lapply(nyu.yom.amu.2salt.mssbmi.res, confint)
##for Age Mig within Child migs only, number of years in the UK and imputed bmi as covariates, waking (p=0.073) n.s. but evening time point (p=0.0024) sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK

#Hypot 2.3.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 regression with age at recruitment-------
y19b.age.2salt.res <- lapply(z.log.19.2salt, y=mig.19pub.data$age.8.19.mig, db=mig.19pub.data, age.bmi.lm)
y19b.age.2salt.sres <- lapply(y19b.age.2salt.res, summary)
##for Wake and Bed n.s. for all covariates 

#Hypot 2.3.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression with age at recruitment and imputed BMI-------
y19b.age.2salt.mssbmi.res <- lapply(z.log.19.2salt, y=mig.19pub.data$age.8.19.mig, db=mig.19pub.data, age.mss.bmi.lm)
y19b.age.2salt.mssbmi.sres <- lapply(y19b.age.2salt.mssbmi.res, summary)
##for Wake age recruit neg. sig. relationship. For Bed, n.s. (p=0.053) Migration cohort n.s. for both sample times

#Hypot 2.4.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only-------
y19c.nyu.2salt.res <- lapply(z.log.19.2salt, y=mig.19pub.data$age.8.19.mig, db=mig.19pub.data, nyu.bmi.lm)
y19c.nyu.2salt.sres <- lapply(y19c.nyu.2salt.res, summary)
##for Wake and Bed, Birth to age 9 n.s. diff from 9-19y migs (p=0.085-0.089 for both), Num Years neg. sig. relationship. for Wake, n.s. for Bed, but note small sample size (n=25, 26) for Wake and Bed sample times
#sample size: summary(model.frame(y19c.nyu.2salt.res$z.log.meanS1D1D2)$y)
#vif(y19c.nyu.2salt.res$z.log.meanS1D1D2)

#Hypot 2.4.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with mean imputed BMI-----
y19c.nyu.2salt.mssbmi.res <- lapply(z.log.19.2salt, y=mig.19pub.data$age.8.19.mig, db=mig.19pub.data, nyu.mssbmi.lm)
y19c.nyu.2salt.mssbmi.sres <- lapply(y19c.nyu.2salt.mssbmi.res, summary)
y19c.nyu.2salt.mssbmi.ci <- lapply(y19c.nyu.2salt.mssbmi.res, confint)
summary(model.frame(y19c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2)$y)
##for Wake (p=0.06) n.s. and Bed sig. (p=0.008), Birth to age 9 > 9-19y migs, Num Years neg. sig. relationship. for wake only.

#Hypot 3 Adult conditions influence salivary testosterone-------

#Hypot 3.1.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi amu regressions in ADU over 19yo migs only-------
nyu.adu.amu.2salt.res <- lapply(z.log.adu.2salt, y=mig.adu.data$z.AgeMigUK, db=mig.adu.data, nyu.bmi.lm)
nyu.adu.amu.2salt.sres <- lapply(nyu.adu.amu.2salt.res, summary)
##Within all Adult migs, number of years in the UK and bmi as covariates, both waking and evening time points were sig. predicted by number of years in the UK, while age at migration was n.s. this could be an age effect, an ecological effect or a combination of both.

#Hypot 3.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 19yo migs only, <40yo age at recruitment cohort-------
nyu.adu40a.amu.2salt.res <- lapply(z.log.adu40a.2salt, y=mig.adu40a.data$z.AgeMigUK, db=mig.adu40a.data, nyu.bmi.lm)
nyu.adu40a.amu.2salt.sres <- lapply(nyu.adu40a.amu.2salt.res, summary)
##for Age Mig within younger Adult migs <40 only, number of years in the UK and bmi as covariates, neither waking nor evening time points were sig. predicted by any of the covariates

#Hypot 3.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 19yo migs only, >40yo age at recruitment cohort-------
nyu.adu40b.amu.2salt.res <- lapply(z.log.adu40b.2salt, y=mig.adu40b.data$z.AgeMigUK, db=mig.adu40b.data, nyu.bmi.lm)
nyu.adu40b.amu.2salt.sres <- lapply(nyu.adu40b.amu.2salt.res, summary)
##for Age Mig within older Adult migs >40 only, number of years in the UK sig. neg correlation with evening time point only. No other sig. predicted by any of the covariates

#AGE AT RECRUITMENT HYPOTHESES-----------------------------------------------------------------

#Hypot 4.1.1 age at recruitment is continuous predictor of salivary testosterone across all populations (untransformed for comparison to other published declines)------
agerec.2salt.ut.res <- lapply(unt.2salt, db=repro.data, agerec.lm)
agerec.2salt.ut.sres <- lapply(agerec.2salt.ut.res, summary)
agerec.2salt.ut.ci <- lapply(agerec.2salt.ut.res, confint)
agerec.wksalt.n <- length(resid(agerec.2salt.ut.res$MeanS1D1D2))
agerec.bdsalt.n <- length(resid(agerec.2salt.ut.res$MeanS3D1D2))
##sig. negative decline in wake salT of .67 pg/mL per year, sig. negative decline in bed salT of .47 pg/mL per year. 

#also applying transformed data
agerec.2salt.res <- lapply(z.log.2salt, db=repro.data, function(x, db){
  lm(x~z.log.age, data=db)	
})
agerec.2salt.sres <- lapply(agerec.2salt.res, summary)
agerec.2salt.ci <- lapply(agerec.2salt.res, confint)
##z.log.age sig. neg. correltion with waking and evening salT 

#Hypot 4.1.2 age at recruitment is continuous predictor of salivary testosterone across all populations, including bmi as a covaraite (untransformed for comparison to other published declines)-----
age.bmi.2salt.ut.res <- lapply(unt.2salt, db=repro.data, agerec.bmi.lm)
age.bmi.2salt.ut.sres <- lapply(age.bmi.2salt.ut.res, summary)
age.bmi.2salt.ut.ci <- lapply(age.bmi.2salt.ut.res, confint)
age.bmi.wksalt.n <- length(resid(age.bmi.2salt.ut.res$MeanS1D1D2))
age.bmi.bdsalt.n <- length(resid(age.bmi.2salt.ut.res$MeanS3D1D2))
##adjusting for BMI, sig. negative decline in wake salT of -0.79 (-1.22, -0.357) pg/mL per year, sig. negative decline in bed salT of -0.55 (-0.949, -0.156) pg/mL per year.

#applying transformed data with bmi
agerec.bmi.2salt.res <- lapply(z.log.2salt, db=repro.data, function(x, db){
  lm(x~z.log.age+z.bmi, data=db)	
})
agerec.bmi.2salt.sres <- lapply(agerec.bmi.2salt.res, summary)
agerec.bmi.2salt.ci <- lapply(agerec.bmi.2salt.res, confint)
##z.log.age and z.bmi sig. for both

#Hypot 4.2.1 are the slopes significantly different between groups for salT?------
#method: ANCOVA of salT as dependent variable with age as predictor including all groups
res.agerec.res <- lapply(unt.2salt, db=repro.data, agerec.aov)
lapply(res.agerec.res, summary)
##indicates a signficant effect of both age and residence for both time points

#also test for interaction
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, as an interaction, then test for differences post-hoc
res.agerecxres <- lapply(unt.2salt, db=repro.data, agerecxres.aov)
lapply(res.agerecxres, summary)
##indicates a signficant effect of age*residence interaction, therefore suggesting there are differences between slopes of sedentees and all other residence groups.

#post-hoc testing of waking salT only
summary(glht(res.agerecxres$MeanS1D1D2, linfct = mcp(residence19pub = "Tukey")))
##in post hoc testing, only the child migrants differ significantly in slope the EUR (p=0.053) and ADU, however caution should be taken as there is also an interaction effect present
#sample size: summary(model.frame(res.agerecxres$MeanS1D1D2)$residence19pub)

#linear models 
summary(lm(MeanS1D1D2~BMI+AgeRecruit*residence19pub, data=repro.data))

#also applying transformed data
res.agerecxres <- lapply(z.log.2salt, db=repro.data, agerecxres.aov)
lapply(res.agerecxres, summary)

#linear model for values of differences in slopes in waking testosterone, using transformed data and bmi to match other regressions in the paper, reported values in fig. 3
resxagerec.res <- lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=repro.data)
resxagerec.sres <- summary(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=repro.data))
resxagerec.n <- summary(model.frame(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=repro.data))$residence19pub)
resxagerec.ci <- confint(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=repro.data))

#list of coefficients and paste to add to text
resxageres.coef <- as.list(resxagerec.res$coefficients[c("z.log.age:residence19pubAdult migrants", "z.log.age:residence19pubChild migrants", "z.log.age:residence19pubSecond generation migrants", "z.log.age:residence19pubBritish European")])
resxagerec.ci.ls <- as.list(resxagerec.ci[c("z.log.age:residence19pubAdult migrants", "z.log.age:residence19pubChild migrants", "z.log.age:residence19pubSecond generation migrants", "z.log.age:residence19pubBritish European"),])
paste(sprintf("%.3f", resxageres.coef),", 95%CI=", paste(round(as.numeric(resxagerec.ci.ls[1:4]),3), ", ", round(as.numeric(resxagerec.ci.ls[5:8]),3), sep=""),sep="")

#post-hoc testing of waking salT only
summary(glht(resxagerec.res, linfct = mcp(residence19pub = "Tukey")))
##in post hoc testing, only the child migrants differ significantly in slope the EUR and ADU, however caution should be taken as there is also an interaction effect present

#Hypot 4.3.1 age at recruitment is continuous predictor of salivary testosterone within residence groups---------
#Bengali Sedentees
#Wake salT
sed.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Bangladeshi sedentees"))
sed.s1d1d2.age.sres <- summary(sed.s1d1d2.age.res)
sed.s1d1d2.age.ci <- confint(sed.s1d1d2.age.res)
##positive trend in wake salT of 1.09 pg/mL per year
#Evening salT
sed.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Bangladeshi sedentees"))
sed.s3d1d2.age.sres <- summary(sed.s3d1d2.age.res)
sed.s3d1d2.age.ci <- confint(sed.s3d1d2.age.res)
##positive trend in bed salT of 1.04 pg/mL per year

#Adult migrants
#AM salT
adu.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Adult migrants"))
adu.s1d1d2.age.sres <- summary(adu.s1d1d2.age.res)
adu.s1d1d2.age.ci <- confint(adu.s1d1d2.age.res)
##n.s. (p=0.06) negative trend in wake salT of .64 pg/mL per year
#Evening salT
adu.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Adult migrants"))
adu.s3d1d2.age.sres <- summary(adu.s3d1d2.age.res)
adu.s3d1d2.age.ci <- confint(adu.s3d1d2.age.res)
##negative trend in bed salT of .91 pg/mL per year

#Child migrants 
#AM salT
chi.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Child migrants"))
chi.s1d1d2.age.sres <- summary(chi.s1d1d2.age.res)
chi.s1d1d2.age.ci <- confint(chi.s1d1d2.age.res)
##neg trend in wake salT of 2.66 pg/mL per year
#Evening salT
chi.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Child migrants"))
chi.s3d1d2.age.sres <- summary(chi.s3d1d2.age.res)
chi.s3d1d2.age.ci <- confint(chi.s3d1d2.age.res)
##neg trend in bed salT of 1.90 pg/mL per year

#Second Generation Migrants 
#AM salT
sg.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Second generation migrants"))
sg.s1d1d2.age.sres <- summary(sg.s1d1d2.age.res)
sg.s1d1d2.age.ci <- confint(sg.s1d1d2.age.res)
##no sig. trend in wake salT (n.s. of -0.60 pg/mL per year)
#Evening salT
sg.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="Second generation migrants"))
sg.s3d1d2.age.sres <- summary(sg.s3d1d2.age.res)
sg.s3d1d2.age.ci <- confint(sg.s3d1d2.age.res)
##no sig. trend in evening salT (n.s. of -2.63 pg/mL per year)

#British European
#AM salT
eur.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="British European"))
eur.s1d1d2.age.sres <- summary(eur.s1d1d2.age.res)
eur.s1d1d2.age.ci <- confint(eur.s1d1d2.age.res)
##no sig. trend in wake salT (n.s. of -.70 pg/mL per year)
eur.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence19pub=="British European"))
eur.s3d1d2.age.sres <- summary(eur.s3d1d2.age.res)
eur.s3d1d2.age.ci <- confint(eur.s3d1d2.age.res)
##no sig. trend in bed salT (n.s. of -.35 pg/mL per year)

#alternative linear model using transformed data and bmi to match other regressions in the paper
summary(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=repro.data))
confint(lm(z.log.meanS1D1D2~z.log.age+z.bmi+residence19pub+z.log.age*residence19pub, data=repro.data))
summary(glht(resxagerec.res, linfct = mcp(residence19pub = "Tukey")))
##ADU, CHI and EUR all differ in age by salivary T trend compared to SED, no sig. differences in post-hoc tests

##4.3.1 conclusions: relationship between salT and age varies widely between groups. 
##The overall population decline is less steep than that reported elsewhere in longitudinal studies by Harman et al 2001 The rate of decline of serum testosterone in Caucasion men according to Harman et al is 0.110 nmol/L per year.  This is 0.11 x 0.288 = 0.032 ng/ml per year or 32 pg/ml per year.  
##Salivary testosterone according to Wang et al is 1/15 that of the serum concentration across a wide range of values.  Therefore, this gives 32 x 1/15 = 2.133 pg/ml per year, times 17.2 years is 36.69 pg/ml. 

#Hypot 4.4.1 are the slopes significantly different between groups for salT, for those showing a decline (UK resident men only)?---------
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, set Europeans as reference group
ukres19.data$residence19pub <- relevel(ukres19.data$residence19pub, ref="British European")
res.agerec.ukres <- lapply(unt.ukres.2salt, db=ukres19.data, agerec.aov)
lapply(res.agerec.ukres, summary)
##indicates a signficant effect of both age and residence for waking salT only

#also test for interaction
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, as an interaction, then test for differences post-hoc
res.agerecxukres <- lapply(unt.ukres.2salt, db=ukres19.data, agerecxres.aov)
lapply(res.agerecxukres, summary)
##age*residence interaction n.s. (p=0.08) for waking, therefore suggesting there are not sig. differences in the age-related decline of men separated by residence group within UK.

#alternative linear model using transformed data and bmi to match other regressions in the paper
summary(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=ukres19.data))
confint(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=ukres19.data))
##no differences in slope within UK residence groups.
summary(glht(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=ukres19.data), linfct = mcp(residence19pub = "Tukey")))

#alternative linear model using transformed data and imputed mssbmi to match other regressions in the paper
summary(lm(z.log.meanS1D1D2~z.log.age+z.mssbmi+z.log.age*residence19pub, data=ukres19.data))
confint(lm(z.log.meanS1D1D2~z.log.age+z.mssbmi+z.log.age*residence19pub, data=ukres19.data))
summary(glht(lm(z.log.meanS1D1D2~z.mssbmi+z.log.age*residence19pub, data=ukres19.data), linfct = mcp(residence19pub = "Tukey")))
#sample size: summary(model.frame(lm(z.log.meanS1D1D2~z.log.age+z.mssbmi+z.log.age*residence19pub, data=ukres19.data))$residence19pub)

##with imputed BMI, CHI interaction effect is sig. (p=0.03), suggests a steeper decline in CHI compared to EUR

#Hypot 4.5.1 When confnining analysis to men who did not show a sig relationship between age and salT (UK born men) is there a significant age related decline in salT, when not considering residence group?------
#AM salT
res.age.ukborn.s1 <- lm(MeanS1D1D2~AgeRecruit, data=subset(repro.data, residence19pub=="British European"| residence19pub=="Second generation migrants"))
summary(res.age.ukborn.s1)
confint(res.age.ukborn.s1)
length(resid(res.age.ukborn.s1))
#significant age realted decline of waking salT -1.22 (95%CI: -2.011599, -0.4347596) within men born and raised in the UK

#Eve salT
res.age.ukborn.s3 <- lm(MeanS3D1D2~AgeRecruit, data=subset(repro.data, residence19pub=="British European"| residence19pub=="Second generation migrants"))
summary(res.age.ukborn.s3)
confint(res.age.ukborn.s3)
length(resid(res.age.ukborn.s3))
#non-sig (p=0.055) age realted decline of evening salT of -0.90 pg/ml per year (95%CI: -1.839, 0.0208) within men born and raised in the UK

#Hypot 4.5.2 Does adjustment of waking salT to decline found in UK born groups eliminate differences between Bengali and European groups?-------
#create an adjusted variable for waking salT adding 1.22pg/ml for each year over age 22 men were at recruitment
ukres19.data$S1D1D2.uk.adj <- ifelse(ukres19.data$AgeRecruit<=22, ukres19.data$MeanS1D1D2, ((ukres19.data$AgeRecruit-22)*1.22)+ukres19.data$MeanS1D1D2)
#inspect to see if this adjusted value needs to be log transformed for normal distriboution
hist(subset(ukres19.data, residence19pub=="British European"| residence19pub=="Second generation migrants")[,"S1D1D2.uk.adj"])
##normally distributed, include untransformed value

#linear model comparing UK-born groups with adjusted salT and z.bmi
summary(lm(S1D1D2.uk.adj~z.log.age+z.bmi+residence19pub, data=subset(ukres19.data, residence19pub=="British European"| residence19pub=="Second generation migrants")))
confint(lm(S1D1D2.uk.adj~z.log.age+z.bmi+residence19pub, data=subset(ukres19.data, residence19pub=="British European"| residence19pub=="Second generation migrants")))
##after adjusting correction factor, second generation waking salT remain 34.14 pg/ml higher (p=.049) than British Eur

##PUBERTY HYPOTHESES-----------------------------------------------------

#Hypothesis 5: Cohorts separated by ethnicity and developmental exposure to ecological conditions will differ in recalled markers of age at puberty-----

#Hypot 5.1.1 age at puberty determined by residence after adjustment for recall bias and demographic trends by age at recruitment.----
res.pub.res <- lapply(z.pub, y=repro.data$residence19pub, db=repro.data, age.lm)
res.pub.sres <- lapply(res.pub.res, summary)
#sample sizes: summary(model.frame(res.pub.res$z.pub.compos)$y)
#compared to sedentees, 2NG and EUR are both sig. earlier age of puberty in composite and all measures (besides voice for 2NG), CHI recall earlier voice change

#test for interaction effect between age and residence group
resxpub.res <- lapply(z.pub, y=repro.data$residence19pub, db=repro.data, function(x, y, db){
  aov(x~z.log.age*y, data=db)	
})
resxpub.sres <- lapply(resxpub.res, summary)
##interaction effect between age and residence group for nocturnal emmission recall only (p=0.046). Composite n.s.
summary(lm(z.pub.ne~z.log.age*residence19pub, data = repro.data))
##compared to sedentees, older CHI more likely to recall later age at first nocturnal emmission.

#for post-hoc multiple comparison Tukey correction of all-pair multiple comparison
res.pub.res.ph <- summary(glht(res.pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
## Composite value: EUR > SED, ADU, CHI; 2NG > SED, ADU

#Hypot 5.2.1, age at puberty determined by age at migration of child migrants (age 19 cutoff)------
amu.19pub.res <- lapply(z.19.pub, y=mig.19pub.data$z.AgeMigUK, db=mig.19pub.data, function(x, y, db){
  lm(x~y, data=db)	
})
amu.19pub.sres <- lapply(amu.19pub.res, summary)
confint(amu.19pub.res$z.pub.compos)
#within child migrants (<19 years) only, sig. positive correlation between age at migration and composite age at puberty, age at shaving, n.e. and pubic hair. Negative relationship between age at recruitment and age at shaving (older men recall shaving earlier)
nrow(model.frame(amu.19pub.res$z.pub.compos))
#note sample size: n=19 for composite

#Hypot 5.2.2, age at puberty determined by age at migration of child migrants (age 19 cutoff), after adjustment for recall bias and demographic trends by age at recruitment.------
amu.age.19pub.res <- lapply(z.19.pub, y=mig.19pub.data$z.AgeMigUK, db=mig.19pub.data, age.lm)
amu.age.19pub.sres <- lapply(amu.age.19pub.res, summary)
amu.age.19pub.ci <- lapply(amu.age.19pub.res, confint)
#sample size: nrow(model.frame(amu.age.19pub.res$z.pub.compos))
#within child migrants (<19 years) only, non sig. (p=0.07) positive correlation between age at migration and composite age at puberty, sig. for age at shaving and pubic hair. 
#sample sizes: n=19

#Hypot 5.3.1, age at puberty determined by migration cohort 0-8 years child migrants and 9-19 years (age 19 cutoff), compared to <19 after adjustment for recall bias and demographic trends by age at recruitment.----
cat.19pub.res <- lapply(z.pub, y=repro.data$age.8.19.mig, db=repro.data, age.lm)
cat.19pub.sres <- lapply(cat.19pub.res, summary)
cat.19pub.ci <- lapply(cat.19pub.res, confint)
summary(model.frame(cat.19pub.res$z.pub.compos)$y)

cat.19pub.res.ph <- summary(glht(cat.19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
confint(glht(cat.19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
##migration cohort 0-8 years child migrants recall sig. earlier shaving, n.s. pubic hair (p=0.057), compared to 9-19, n.s. composite. Note that if not adjusting for age, migrants 0-9 recall earlier composite than 9-19 (not shown)

#Hypot 5.3.2, age at puberty determined by migration cohort pre-birth-8 years (Second generation and child migrants) and 9-19 years (age 19 cutoff), after adjustment for recall bias and demographic trends by age at recruitment.----
cat.b19pub.res <- lapply(z.pub, y=repro.data$age.b8.19.mig, db=repro.data, age.lm)
cat.b19pub.sres <- lapply(cat.b19pub.res, summary)
cat.b19pub.ci <- lapply(cat.b19pub.res, confint)
summary(model.frame(cat.b19pub.res$z.pub.compos)$y)

cat.b19pub.res.ph <- summary(glht(cat.b19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
confint(glht(cat.b19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))

#Hypot 5.4.1, age at puberty determined by age at migration of adult migrants.----
amu.adu.pub.res <- lapply(z.adu.pub, y=mig.adu.data$z.AgeMigUK, db=mig.adu.data, function(x, y, db){
  lm(x~y, data=db)	
})
amu.adu.pub.sres <- lapply(amu.adu.pub.res, summary)
confint(amu.adu.pub.res$z.pub.compos)
##within adult migrants only, no indication of an age at migration effect on recalled puberty

#Hypot 5.4.2, age at puberty determined by age at migration of adult migrants, after adjustment for recall bias and demographic trends by age at recruitment.----
amu.adu.age.pub.res <- lapply(z.adu.pub, y=mig.adu.data$z.AgeMigUK, db=mig.adu.data, age.lm)
amu.adu.age.pub.sres <- lapply(amu.adu.age.pub.res, summary)
confint(amu.adu.age.pub.res$z.pub.compos)
nrow(model.frame(amu.adu.age.pub.res$z.pub.compos))
##within adult migrants only, no indication of an age at migration effect on recalled puberty

#Hypotheses 6: Across all groups, without separation by ethnicity or developmental exposure to ecological conditions, adult salivary T is a predictor of recalled age at puberty-------

#Hypot 6.1.1 waking adult salT is a predictor of pub traits across all groups, after adjustment for recall bias and demographic trends by age at recruitment.-------
salt1.pub.res <- lapply(z.pub, y=repro.data$z.log.meanS1D1D2, db=repro.data, age.lm)
salt1.pub.sres <- lapply(salt1.pub.res, summary)
length(resid(salt1.pub.res$z.pub.compos))
confint(salt1.pub.res$z.pub.compos)

#Hypot 6.1.2 evening adult salT is a predictor of pub traits across all groups, after adjustment for recall bias and demographic trends by age at recruitment.-----
salt3.pub.res <- lapply(z.pub, y=repro.data$z.log.meanS3D1D2, db=repro.data, age.lm)
salt3.pub.sres <- lapply(salt3.pub.res, summary)
length(resid(salt3.pub.res$z.pub.compos))
confint(salt3.pub.res$z.pub.compos)

#Hypotheses 7: Within UK Bangladeshis only, adult salivary T is a predictor of recalled age at puberty, after adjustment for recall bias and demographic trends by age at recruitment.-----

#Hypot 7.1.1 waking adult salT as a predictor of pub traits within UK BDs, after adjustment for recall bias and demographic trends by age at recruitment.------
salt1.ukbd.pub.res <- lapply(z.ukbd.pub, y=ukresbds19.data$z.log.meanS1D1D2, db=ukresbds19.data, age.lm)
salt1.ukbd.pub.sres <- lapply(salt1.ukbd.pub.res, summary)
##within UK BDs wake salT n.s. predictor of composite age at puberty, only earlier nocturnal emmisions recall is sig. predicted by higher salT, pos. association between age at recruitment and compos. as well as voice breaking (younger men recall earlier age)

#Hypot 7.1.2 evening adult salT is a predictor of pub traits within UK BDs, after adjustment for recall bias and demographic trends by age at recruitment.--------
salt3.ukbd.pub.res <- lapply(z.ukbd.pub, y=ukresbds19.data$z.log.meanS3D1D2, db=ukresbds19.data, age.lm)
salt3.ukbd.pub.sres <- lapply(salt3.ukbd.pub.res, summary)
#within all UK BD groups, Bed salT n.s. predictor of composite recalled age of puberty, positive relationships between age at recruitment and voice breaking as well as compos. Younger men recall both at earlier ages.

#Hypotheses 8: Within Sedentees only, adult salivary T is a predictor of recalled age at puberty, after adjustment for recall bias and demographic trends by age at recruitment.-----

#Hypot 8.1.1 waking adult salT as a predictor of pub traits within sedentees, after adjustment for recall bias and demographic trends by age at recruitment.------
salt1.sed.pub.res <- lapply(z.sed.pub, y=subset(repro.data, residence19pub=="Bangladeshi sedentees")$z.log.meanS1D1D2, db=subset(repro.data, residence19pub=="Bangladeshi sedentees"), age.lm)
salt1.sed.pub.sres <- lapply(salt1.sed.pub.res, summary)
##within sedentees wake salT n.s. (p=0.063) neg. predictor of composite age at puberty, only earlier nocturnal emmisions recall is sig. predicted by higher salT, pos. association between age at recruitment and compos. as well as voice breaking (younger men recall earlier age)

#Hypot 8.1.2 evening adult salT is a predictor of pub traits within sedentees, after adjustment for recall bias and demographic trends by age at recruitment.--------
salt3.sed.pub.res <- lapply(z.sed.pub, y=subset(repro.data, residence19pub=="Bangladeshi sedentees")$z.log.meanS3D1D2, db=subset(repro.data, residence19pub=="Bangladeshi sedentees"), age.lm)
salt3.sed.pub.sres <- lapply(salt3.sed.pub.res, summary)
#within seds, Bed salT n.s. predictor of composite recalled age of puberty, positive relationships between age at recruitment and pubic hair as well as compos. Younger men recall both at earlier ages.

###HEIGHT HYPOTHESES-------
#Hypotheses 9: Childhood age at migration is a predictor of adult height------------

#Hypot 9.1.1 within CHI only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 19years, no covariates-----------
height.agemig.chi.res <- lm(z.height~z.AgeMigUK, data=subset(repro.data, residence19pub=="Child migrants"))
height.agemig.chi.sres <- summary(height.agemig.chi.res)
confint(height.agemig.chi.res)
nrow(model.frame(height.agemig.chi.res))
##for Age Mig within CHI only, height is sig. negatively predicted by age at migration.
##a clear demographic effect was seen in hight of CHI, so run with age at recruitment as a covariate

#Hypot 9.1.2 within CHI only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 19years, age at recruitment as covariate-----------
height.agemig.agerec.chi.res <- lm(z.height~z.log.age+z.AgeMigUK, data=subset(repro.data, residence19pub=="Child migrants"))
height.agemig.agerec.chi.sres <- summary(height.agemig.agerec.chi.res)
#when age at recruitment and age at migration are both included, neither shows a significant age at migration effect. Possibly due to highly collinear effect of peak migration on the demography of this group. Check VIF:
vif(height.agemig.agerec.chi.res)
#both are 1.54, which is high but not substantially above a guideline threshold of 1. These are two significantly correlated predictors, but not collinear.

#Hypot 9.1.3 within CHI only, age at recruitment as continuous predictor for migrants who arrived before 19years no covariate-----------
height.agerec.chi.res <- lm(z.height~z.log.age, data=subset(repro.data, residence19pub=="Child migrants"))
height.agerec.chi.sres <- summary(height.agemig.chi.res)
confint(height.agerec.chi.res)
#age at recruitment alone shows a significant neg. effect on height. -0.71 SD, (95%CI: -0.840, -0.125), n=37

#Hypot 9.1.4 within CHI only, number of years in the uk as continuous predictor for migrants who arrived before 19years-----------
height.agemig.nyu.chi.res <- lm(z.height~NumYearsUK+z.AgeMigUK, data=subset(repro.data, residence19pub=="Child migrants"))
height.agemig.nyu.chi.sres <- summary(height.agemig.nyu.chi.res)
#when number of years in the UK and age at migration are both included, a significant age at migration effect on height. 

#Hypot 9.2.1 Height within CHI migs, migration before age 9 as compared to migration age 9-19 regression-------
y19b.height.res <- lm(z.height~age.8.19.mig, data=subset(repro.data, residence19pub=="Child migrants"))
y19b.height.sres <- summary(y19b.height.res)
##n.s. difference in height between cohorts split at age 9 (p=0.056)

#Hypot 9.2.2 Height within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression with age at recruitment-------
y19b.age.height.res <- lm(z.height~z.log.age+age.8.19.mig, data=subset(repro.data, residence19pub=="Child migrants"))
y19b.age.height.sres <- summary(y19b.age.height.res)
##n.s. difference in height between cohorts split at age 9 (p=0.26)

#Hypot 9.2.3 Height within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression with number of years in the uk-------
y19b.nyu.height.res <- lm(z.height~NumYearsUK+age.8.19.mig, data=subset(repro.data, residence19pub=="Child migrants"))
y19b.nyu.height.sres <- summary(y19b.nyu.height.res)
##sig. difference in height between cohorts split at age 9 (p=0.043)

#Hypotheses 10: Adult age at migration is a predictor of adult height------------

#Hypot 10.1.1 within ADU only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived after 19years, no covariates-----------
height.agemig.adu.res <- lm(z.height~z.AgeMigUK, data=subset(repro.data, residence19pub=="Adult migrants"))
height.agemig.adu.sres <- summary(height.agemig.adu.res)
confint(height.agemig.adu.res)
##for Age Mig within ADU only, height is n.s. predicted by age at migration.

#Hypot 10.1.2 within ADU only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived after 19years, age at recruitment as covariate-----------
height.agemig.agerec.adu.res <- lm(z.height~z.log.age+z.AgeMigUK, data=subset(repro.data, residence19pub=="Adult migrants"))
height.agemig.agerec.adu.sres <- summary(height.agemig.agerec.adu.res)
#when age at recruitment and age at migration are both included, neither shows a significant age at migration effect.

#Hypot 10.1.3 within ADU only, z.log.age at recruitment as continuous predictor for migrants who arrived after 19years no covariate-----------
height.agerec.adu.res <- lm(z.height~z.log.age, data=subset(repro.data, residence19pub=="Adult migrants"))
height.agerec.adu.sres <- summary(height.agemig.adu.res)
confint(height.agerec.adu.res)
#age at recruitment alone shows a significant neg. effect on height. -0.71 SD, (95%CI: -0.840, -0.125), n=37

###CHECKS OF ASSUMPTIONS-----------------------------------------------------------------------------------------------------------

#a1.1 normal distribution of response variables
#salT
#mean wake sample
hist(repro.data$MeanS1D1D2)
##positive skew, therefore log transform
hist(log(repro.data$MeanS1D1D2))
##normal distribution

#mean bed sample
hist(repro.data$MeanS3D1D2)
##positive skew, therefore log transform
hist(log(repro.data$MeanS1D1D2))
##normal distribution

#composite age at puberty
hist(repro.data$pub.compos)
##normal distribution

#a1.2 normal distribution of predictor variables
#age at recrutment
hist(repro.data$AgeRecruit)
##positive skew, therefore log transform
hist(log(repro.data$AgeRecruit))
##normal distribution

#BMI
hist(repro.data$BMI)
##normal distribution

#a.2 normal distribution of model residuals, no systematic bias

#Assumptions 1.1 SalT by residence: sd units (log)age and bmi as covariates
#normality of distribution of residuals
hist(residuals(res.2salt.res$z.log.meanS1D1D2), col="darkgray")
hist(residuals(res.2salt.res$z.log.meanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(res.2salt.res$z.log.meanS1D1D2), residuals(res.2salt.res$z.log.meanS1D1D2))
plot(fitted(res.2salt.res$z.log.meanS3D1D2), residuals(res.2salt.res$z.log.meanS3D1D2))
##no systematic bias
#check for homogeneity of variance
leveneTest(z.log.meanS1D1D2 ~ residence19pub, center = mean, data = repro.data)
##levene test n.s.
leveneTest(z.log.meanS3D1D2 ~ residence19pub, center = mean, data = repro.data)
##levene test sig. but visual inspection of residuals suggests two outlying points. No observational or laboratory reason to eliminate these points, and other measures appear broadly within assumptions.

#Assumptions 1.2 age at migration "z.AgeMigUK" as continuous predictor after adjusting for number of years in uk "nyu" and age at migration regressions in all migs
#normality of distribution of residuals
hist(residuals(nyu.amu.2salt.res$z.log.meanS1D1D2), col="darkgray")
hist(residuals(nyu.amu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(nyu.amu.2salt.res$z.log.meanS1D1D2), residuals(nyu.amu.2salt.res$z.log.meanS1D1D2))
plot(fitted(nyu.amu.2salt.res$z.log.meanS3D1D2), residuals(nyu.amu.2salt.res$z.log.meanS3D1D2))
##no systematic bias
#check for homogeneity of variance restricted within migrant groups only
leveneTest(z.log.meanS1D1D2 ~ residence19pub, center = mean, data = ukresbds19.data)
##levene test n.s.
leveneTest(z.log.meanS3D1D2 ~ residence19pub, center = mean, data = ukresbds19.data)
##levene test n.s.

#Assumptions 2: Childhood age at migration is a predictor of adult salivary T

#a1.2 normal distribution of predictor variables
#age at migration
hist(subset(repro.data, residence19pub=="Child migrants")$AgeMigUK)
#While distorted by a high number of early childhood migrants in first year of life, for age-related data is generally normal.

#Assumptions 2.1.1 within CHI only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 19years "y19" regressions
#normality of distribution of residuals
hist(residuals(y19a.2salt.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic
hist(residuals(y19a.2salt.res$z.log.meanS3D1D2), col="darkgray")
##sample size limits on interpretation
#check for bias or heteroscedasticy
plot(fitted(y19a.2salt.res$z.log.meanS1D1D2), residuals(y19a.2salt.res$z.log.meanS1D1D2))
plot(fitted(y19a.2salt.res$z.log.meanS3D1D2), residuals(y19a.2salt.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#check for homogeneity of variance
leveneTest(z.log.meanS1D1D2 ~ age.8.19.mig, center = mean, data = mig.19pub.data)
##levene test sig. but visual inspection of residuals suggests limitations of sample size as opposed to systematic bias. Interpretation with caveats of sample size
leveneTest(z.log.meanS3D1D2 ~ age.8.19.mig, center = mean, data = repro.data)
##levene test just sig. (p=0.05) but proceed with inference with caveats of sample size

#Assumptions 2.1.2 within CHI only, age at migration "z.AgeMigUK" as continuous predictor who arrived before 19years "y19" regressions, with imputed BMI
#normality of distribution of residuals
hist(residuals(y19a.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic. Better normal distribution with additional points added by imputation.
hist(residuals(y19a.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##better normal distribution with additional points added by imputation.
#check for bias or heteroscedasticy
plot(fitted(y19a.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(y19a.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(y19a.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(y19a.2salt.mssbmi.res$z.log.meanS3D1D2))
##sample size limits on interpretation, possibly some narrowing of distribution at higher fitted values
#checks for homogeneity of variance same as above. Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 2.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" regressions in CHI under 19yo migs only
#nyu.yom.amu.2salt.sres
#normality of distribution of residuals
hist(residuals(nyu.yom.amu.2salt.res$z.log.meanS1D1D2), col="darkgray")
hist(residuals(nyu.yom.amu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(nyu.yom.amu.2salt.res$z.log.meanS1D1D2), residuals(nyu.yom.amu.2salt.res$z.log.meanS1D1D2))
plot(fitted(nyu.yom.amu.2salt.res$z.log.meanS3D1D2), residuals(nyu.yom.amu.2salt.res$z.log.meanS3D1D2))
##no systematic bias, some interpretation limited by sample size

#Assumptions 2.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with imputed BMI
#nyu.yom.amu.2salt.mssbmi.res
#normality of distribution of residuals
hist(residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
hist(residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS3D1D2))
##no systematic bias, Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 2.3.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 regression with age at recruitment
#y19b.age.2salt.res 
#normality of distribution of residuals
hist(residuals(y19b.age.2salt.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic
hist(residuals(y19b.age.2salt.res$z.log.meanS3D1D2), col="darkgray")
##sample size limits on interpretation
#check for bias or heteroscedasticy
plot(fitted(y19b.age.2salt.res$z.log.meanS1D1D2), residuals(y19b.age.2salt.res$z.log.meanS1D1D2))
plot(fitted(y19b.age.2salt.res$z.log.meanS3D1D2), residuals(y19b.age.2salt.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#check for homogeneity of variance same
leveneTest(z.log.meanS1D1D2 ~ age.8.19.mig, center = mean, data = mig.19pub.data)
##levene test sig. but visual inspection of residuals suggests limitations of sample size as opposed to systematic bias. Interpretation with caveats of sample size
leveneTest(z.log.meanS3D1D2 ~ age.8.19.mig, center = mean, data = repro.data)
##levene test just sig. (p=0.05) but proceed with interpretation with caveats of sample size

#Assumptions 2.3.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.19.mig.b" regression with age at recruitment and imputed BMI
#y19b.age.2salt.mssbmi.res
#normality of distribution of residuals
hist(residuals(y19b.age.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic. Better normal distribution with additional points added by imputation.
hist(residuals(y19b.age.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##better normal distribution with additional points added by imputation.
#check for bias or heteroscedasticy
plot(fitted(y19b.age.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(y19b.age.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(y19b.age.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(y19b.age.2salt.mssbmi.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#checks for homogeneity of variance same as above. Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 2.4.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.19.mig.b" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only
#y19c.nyu.2salt.res
#normality of distribution of residuals
hist(residuals(y19c.nyu.2salt.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic
hist(residuals(y19c.nyu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##sample size limits on interpretation
#check for bias or heteroscedasticy
plot(fitted(y19c.nyu.2salt.res$z.log.meanS1D1D2), residuals(y19c.nyu.2salt.res$z.log.meanS1D1D2))
plot(fitted(y19c.nyu.2salt.res$z.log.meanS3D1D2), residuals(y19c.nyu.2salt.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#check for homogeneity of variance same
leveneTest(z.log.meanS1D1D2 ~ age.8.19.mig, center = mean, data = mig.19pub.data)
##levene test sig. but visual inspection of residuals suggests limitations of sample size as opposed to systematic bias. Interpretation with caveats of sample size
leveneTest(z.log.meanS3D1D2 ~ age.8.19.mig, center = mean, data = repro.data)
##levene test just sig. (p=0.05) but proceed with interpretation with caveats of sample size

#Assumptions 2.4.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.19.mig.b" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with imputed BMI
#y19c.nyu.2salt.mssbmi.res
#normality of distribution of residuals
hist(residuals(y19c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic. Better normal distribution with additional points added by imputation.
hist(residuals(y19c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##better normal distribution with additional points added by imputation.
#check for bias or heteroscedasticy
plot(fitted(y19c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(y19c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(y19c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(y19c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#checks for homogeneity of variance same as above. Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 3 Adult conditions influence salivary testosterone

#Assumptions 3.1.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi amu regressions in ADU over 19yo migs only
#nyu.adu.amu.2salt.res
#normality of distribution of residuals
hist(residuals(nyu.adu.amu.2salt.res$z.log.meanS1D1D2), col="darkgray")
##normal dist
hist(residuals(nyu.adu.amu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##normal dist
#check for bias or heteroscedasticy
plot(fitted(nyu.adu.amu.2salt.res$z.log.meanS1D1D2), residuals(nyu.adu.amu.2salt.res$z.log.meanS1D1D2))
plot(fitted(nyu.adu.amu.2salt.res$z.log.meanS3D1D2), residuals(nyu.adu.amu.2salt.res$z.log.meanS3D1D2))
##no indication of bias. Proceed to inference

#Assumptions 3.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 19yo migs only, <40yo age at recruitment cohort
#nyu.adu40a.amu.2salt.res
#normality of distribution of residuals
hist(residuals(nyu.adu40a.amu.2salt.res$z.log.meanS1D1D2), col="darkgray")
##normal dist some right skew
hist(residuals(nyu.adu40a.amu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##normal dist
#check for bias or heteroscedasticy
plot(fitted(nyu.adu40a.amu.2salt.res$z.log.meanS1D1D2), residuals(nyu.adu40a.amu.2salt.res$z.log.meanS1D1D2))
plot(fitted(nyu.adu40a.amu.2salt.res$z.log.meanS3D1D2), residuals(nyu.adu40a.amu.2salt.res$z.log.meanS3D1D2))
##no indication of bias. Proceed to inference

#Assumptions 3.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 19yo migs only, >40yo age at recruitment cohort
#nyu.adu40b.amu.2salt.res
#normality of distribution of residuals
hist(residuals(nyu.adu40b.amu.2salt.res$z.log.meanS1D1D2), col="darkgray")
##normal dist some left skew
hist(residuals(nyu.adu40b.amu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##normal dist
#check for bias or heteroscedasticy
plot(fitted(nyu.adu40b.amu.2salt.res$z.log.meanS1D1D2), residuals(nyu.adu40b.amu.2salt.res$z.log.meanS1D1D2))
plot(fitted(nyu.adu40b.amu.2salt.res$z.log.meanS3D1D2), residuals(nyu.adu40b.amu.2salt.res$z.log.meanS3D1D2))
##no indication of bias. Proceed to inference

#Age at recruitment hypotheses-----------------------------------------------------------------

#Assumptions 4.1.1 age at recruitment is continuous predictor of salivary testosterone across all populations
#agerec.2salt.res
#normality of distribution of residuals
hist(residuals(agerec.2salt.ut.res$MeanS1D1D2), col="darkgray")
##normal dist some left skew
hist(residuals(agerec.2salt.ut.res$MeanS3D1D2), col="darkgray")
##normal dist with some left skew
#check for bias or heteroscedasticy
plot(fitted(agerec.2salt.ut.res$MeanS1D1D2), residuals(agerec.2salt.ut.res$MeanS1D1D2))
plot(fitted(agerec.2salt.ut.res$MeanS3D1D2), residuals(agerec.2salt.ut.res$MeanS3D1D2))
##no indication of sysematic bias. Proceed to inference

#Assumptions 4.2.1 are the slopes significantly different between groups for salT?
#checking assumptions for untransformed models
#normality of distribution of residuals
hist(residuals(res.agerec.res$MeanS1D1D2), col="darkgray")
hist(residuals(res.agerec.res$MeanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(res.agerec.res$MeanS1D1D2), residuals(res.agerec.res$MeanS1D1D2))
plot(fitted(res.agerec.res$MeanS3D1D2), residuals(res.agerec.res$MeanS3D1D2))
##no indication of systematic bias
#check for homogeneity of variance
leveneTest(MeanS1D1D2 ~ residence19pub, center = mean, data = repro.data)
##levene test n.s.
leveneTest(MeanS3D1D2 ~ residence19pub, center = mean, data = repro.data)
##levene test sig. Run with z.log mean salT for evening samples to verify any inferences based on slope (see salT decline by year regressions for details of these tests)

#Alternative linear model of interaction including transformed data
hist(residuals(resxagerec.res), col="darkgray")
##residuals appear normally distributed
#check for bias or heterscedasticity
plot(fitted(resxagerec.res), residuals(resxagerec.res))
##no indication of systematic bias

#Assumptions 4.3.1 are the slopes significantly different between groups for salT, for those showing a decline (UK resident men only)?
#res.agerec.ukres
#checking assumptions for untransformed models
#normality of distribution of residuals
hist(residuals(res.agerec.ukres$MeanS1D1D2), col="darkgray")
hist(residuals(res.agerec.ukres$MeanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(res.agerec.ukres$MeanS1D1D2), residuals(res.agerec.ukres$MeanS1D1D2))
plot(fitted(res.agerec.ukres$MeanS3D1D2), residuals(res.agerec.ukres$MeanS3D1D2))
##no indication of systematic bias
#check for homogeneity of variance
leveneTest(MeanS1D1D2 ~ residence19pub, center = mean, data = ukres19.data)
##levene test n.s.
leveneTest(MeanS3D1D2 ~ residence19pub, center = mean, data = ukres19.data)
##levene test sig. Run with z.log mean salT for evening samples to verify any inferences based on slope (see salT decline by year regressions for details of these tests)

#Assumptions 4.4.1 When confnining analysis to men who did not show a sig relationship between age and salT (UK born men) is there a significant age related decline in salT, when not considering residence group?
#AM salT
#res.age.ukborn.s1
#normality of distribution of residuals
hist(residuals(res.age.ukborn.s1), col="darkgray")
hist(residuals(res.age.ukborn.s3), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(res.age.ukborn.s1), residuals(res.age.ukborn.s1))
plot(fitted(res.age.ukborn.s3), residuals(res.age.ukborn.s3))
##no indication of systematic bias

##Puberty Hypotheses-----------------------------------------------------

#Assumptions 5: Cohorts separated by ethnicity and developmental exposure to ecological conditions will differ in recalled markers of age at puberty.

#checking assumptions for untransformed models
#normality of distribution of residuals
hist(residuals(res.pub.res$z.pub.compos), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(res.pub.res$z.pub.compos), residuals(res.pub.res$z.pub.compos))
##no indication of systematic bias
#check for homogeneity of variance
leveneTest(z.pub.compos ~ residence19pub, center = mean, data = repro.data)
##levene test n.s.

#PLOTS--------------------------------------------------------------------------------------------------------------------------- 
#figure 1: estimated marginal means plot------ 
#from Using lsmeans, Russell V. Lenth https://cran.r-project.org/web/packages/lsmeans/vignettes/using-lsmeans.pdf

#Plot 1.1 lsmeans by residence groups
#establish reference grid for each sample time
salt1.rg <-ref.grid(res.2salt.res$z.log.meanS1D1D2)
salt2.rg <- ref.grid(res.2salt.res$z.log.meanS3D1D2)

#for effect of residence after adjustment for covariates
salt1.lsm <- lsmeans(salt1.rg, "y")
salt2.lsm <- lsmeans(salt2.rg, "y")

#for pairwise contrasts
contrast(salt1.rg, method = "pairwise")
contrast(salt2.rg, method = "pairwise")

#note that only the summary can be subsetted or made into a dataframe object
summary(salt1.lsm)
summary(salt2.lsm)

#extract coefficients from each time point, label them by sample time and make into a dataframe for plotting
salt.lsm.data <- cbind(rbind(data.frame(summary(salt1.lsm)), data.frame(summary(salt2.lsm))), c(rep("morning", 5), rep("evening", 5)))
#rename variable for sample time
names(salt.lsm.data) <- c("residence", "lsmeans", "SE", "df", "lower.CL", "upper.CL", "sample")
#relevel sample time
salt.lsm.data$sample <- relevel(salt.lsm.data$sample, "morning")
#relevel residence groups, 
salt.lsm.data$residence <- factor(salt.lsm.data$residence, levels=c("Second generation migrants", "Child migrants", "British European", "Bangladeshi sedentees", "Adult migrants" ))
levels(salt.lsm.data$residence)[levels(salt.lsm.data$residence)=="Low Status White British"] <- "British-born European"

salt.lsm.plt <- ggplot(data=salt.lsm.data, aes(x=sample, y=lsmeans, group = residence, colour = residence, line = residence))

salt.lsm.plt +
  geom_line(position=position_dodge(0.17), size=1.5, aes(linetype=residence)) +
  geom_point( size=4, shape=19, position=position_dodge(0.17)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.2, size=1.5, position=position_dodge(0.17)) +
  labs(x="Sampling time", y=expression(paste("z-Testosterone (log)"))) +
  theme_minimal() +
  theme(legend.position = c(.5, .9)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) +
  theme(axis.text = element_text(size = rel(1.5))) +
  theme(axis.title = element_text(size = rel(1.5))) +
  theme(legend.text = element_text(size = rel(1.35)))

##To paste into figure legend
# paste(sprintf("%.3f", salt.lsm.data$lsmeans[1:5]),", 95%CI=", paste(round(as.numeric(salt.lsm.data$lower.CL[1:5]),3), ", ", round(as.numeric(salt.lsm.data$upper.CL[1:5]),3), sep=""),sep="")
# paste(sprintf("%.3f", salt.lsm.data$lsmeans[6:10]),", 95%CI=", paste(round(as.numeric(salt.lsm.data$lower.CL[6:10]),3), ", ", round(as.numeric(salt.lsm.data$upper.CL[6:10]),3), sep=""),sep="")
# sample size: summary()

#Figure 2. Daily average salivary testosterone by age at migration----
#create subset data with age.8.mig levels reordered for plot
repro.plt.data <- subset(subset(repro.data, residence19pub=="Adult migrants" | residence19pub=="Child migrants"), !is.na(AgeMigUK))
repro.plt.data$age.8.19.mig <- factor(repro.plt.data$age.8.19.mig, levels=c("Birth-9y", "9-19y", ">19y", "Pre-Birth (Born UK)"))
#create plot
ggplot(data = repro.plt.data) + 
  geom_point(mapping = aes(x = AgeMigUK, y = z.log.meanS1S3D1D2, colour = age.8.19.mig, shape = age.8.19.mig, size=z.mssbmi, alpha=z.log.age)) +
  geom_smooth(aes(x = AgeMigUK, y = z.log.meanS1S3D1D2, colour = age.8.19.mig), method = "lm", se = T, show.legend=F) +
  theme_minimal() + 
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) +
  theme(axis.text = element_text(size = rel(1.5))) +
  theme(axis.title = element_text(size = rel(1.5))) +
  theme(legend.text = element_text(size = rel(1.5))) +
  theme(legend.position = c(.9, .9)) +
  guides(size=FALSE, fill=FALSE, alpha=F, shape = guide_legend(override.aes = list(size = 5))) +
  scale_shape_manual(breaks=c("Birth-9y", "9-19y", ">19y"),
                     labels=c("Birth-9y", "9-19y", ">19y"),
                     values=c(16,15,17)) +
  ylab("z-Testosterone (log)") +
  xlab("Age at migration to the UK (years)") 
#caption (not used)
# labs(caption  = paste0("\nSalivary testosterone of migrants arriving in the UK in early childhood (Birth-9y), late childhood (9-19y), and adulthood (19y+). \n", "Age at recruitment is indicated by darkness of point (older=darker), and point size indicates (imputed) BMI. \n", "Line indicates linear regression with SE. \n"), hjust=0.5)

#for regression coefficients to include in figure caption
repro.plt.data$age.8.19.mig <- relevel(repro.plt.data$age.8.19.mig, ">19y")
summary(lm(z.log.meanS1S3D1D2~z.mssbmi+z.log.age+relevel(repro.plt.data$age.8.19.mig, ">19y"), data=repro.plt.data))
confint(lm(z.log.meanS1S3D1D2~z.mssbmi+z.log.age+relevel(repro.plt.data$age.8.19.mig, ">19y"), data=repro.plt.data))
#sig.diff between adult migrants and 0-9y migrants: 0.571 SD, 95%CI 0.0331, 1.109, p=0.038, n.s. diff between 9-19: 0.148 SD, 95%CI -0.344, 0.640, p=0.55

#for n of groups
tapply(repro.plt.data$z.bmi, repro.plt.data$age.8.19.mig, summary)
summary(repro.plt.data$age.8.19.mig)


#Figure 3.Scatterplot of recalled age at puberty by age at migration----
# Map group to point shape, and use larger points
pub.mig.2ng.plt <- ggplot(data=subset(repro.data, residence19pub=="Adult migrants" | residence19pub=="Child migrants"), aes(x=AgeMigUK, y=pub.compos, group=residence19pub, shape=residence19pub, colour=residence19pub)) 
pub.mig.2ng.plt +
  geom_point(size= 4) +
  geom_smooth(method = "lm", se = T, show.legend=F) +
  labs(title = element_blank()) +
  theme_minimal() + 
  theme(axis.text = element_text(size = rel(2))) +
  theme(axis.title = element_text(size = rel(2))) +
  theme(legend.text = element_text(size = rel(1.5))) +
  theme(legend.position = c(.8, .9)) + 
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) +
  guides(shape = guide_legend(override.aes = list(size = rel(4)))) +
  scale_shape_manual(breaks=c("Adult migrants", "Child migrants"),
                     labels=c("Adult migrants", "Child migrants"),
                     values=c(17,15)) +
  scale_colour_manual(breaks=c("Adult migrants", "Child migrants"),
                     labels=c("Adult migrants", "Child migrants"),
                     values=c("#619CFF", "#F8766D")) +
  labs(x="Age at migration (years)", y="Composite recalled age of puberty (years)")

#Figure 4a. secular trends in height--------
agerec.height.plt <- ggplot(data = repro.data) + 
  geom_point(mapping = aes(x = AgeRecruit, y = z.height, colour = ukbd.born.adu, shape = residence19pub), size=4) +
  geom_smooth(aes(x = AgeRecruit, y = z.height, colour = ukbd.born.adu, linetype = ukbd.born.adu), method = "lm", se = T) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) +
  theme(axis.text = element_text(size = rel(2))) +
  theme(axis.title = element_text(size = rel(2.2))) +
  theme(legend.text = element_text(size = rel(1.5))) +
  theme(legend.position = c(.505, .9)) +
  theme(legend.box = "horizontal") +
  guides(size=FALSE, fill=FALSE, alpha=F) + guides(shape = guide_legend(override.aes = list(fill=NA))) +
  scale_shape_discrete(name  ="Residence group",
                       breaks=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees"),
                       labels=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees")) +
  scale_colour_manual(name  ="Residence group",
                      breaks=c("Migrated in childhood", "Reached adulthood in UK", "Reached adulthood in UK", "Reached adulthood in Bangladesh", "Reached adulthood in Bangladesh"),
                      labels=c("Migrated in childhood", "Reached adulthood in UK", "Reached adulthood in UK", "Reached adulthood in Bangladesh", "Reached adulthood in Bangladesh"),
                      values=c("#F8766D", "#619CFF", "#00BA38")) +
  scale_linetype_discrete(name  ="Residence group",
                          breaks=c("Migrated in childhood", "Reached adulthood in UK", "Reached adulthood in UK", "Reached adulthood in Bangladesh", "Reached adulthood in Bangladesh"),
                          labels=c("Migrated in childhood", "Reached adulthood in UK", "Reached adulthood in UK", "Reached adulthood in Bangladesh", "Reached adulthood in Bangladesh")) +
  ylab("z-standing height") +
  xlab("Age at recruitment (years)")

#for figure legend: height by age regression coefficients:
# summary(lm(z.height~z.log.age, data = subset(repro.data, residence19pub=="Child migrants")))
# confint(lm(z.height~z.log.age, data = subset(repro.data, residence19pub=="Child migrants")))
# height.bdadu.sres
# confint(height.bdadu.res)
# height.ukadu.sres
# confint(height.ukadu.res)

#Figure 4b. age at migration trends in height
agemig.height.plt <- ggplot(data = subset(repro.data, residence19pub=="Child migrants"|residence19pub=="Adult migrants")) + 
  geom_point(mapping = aes(x = AgeMigUK, y = z.height, colour = ukbd.born.adu, shape = residence19pub), size=4) +
  geom_smooth(aes(x = AgeMigUK, y = z.height, colour = ukbd.born.adu, linetype = ukbd.born.adu), method = "lm", se = T) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) +
  theme(axis.title.y = element_blank())+
  theme(axis.text = element_text(size = rel(2))) +
  theme(axis.title = element_text(size = rel(2.2))) +
  theme(legend.text = element_text(size = rel(1.5))) +
  theme(legend.position = c(.75, .9)) +
  guides(size=FALSE, fill=FALSE, alpha=F) + guides(shape = guide_legend(override.aes = list(fill=NA))) +
  scale_shape_manual(name  ="Residence group",
                     breaks=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees"),
                     labels=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees"),
                     values=c(17,15)) +
  scale_colour_manual(name  ="Residence group",
                      breaks=c("Migrated in childhood", "Reached adulthood in UK", "Reached adulthood in UK", "Reached adulthood in Bangladesh", "Reached adulthood in Bangladesh"),
                      labels=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees"),
                      values=c("#F8766D", "#619CFF")) +
  scale_linetype_discrete(name  ="Residence group",
                          breaks=c("Migrated in childhood", "Reached adulthood in UK", "Reached adulthood in UK", "Reached adulthood in Bangladesh", "Reached adulthood in Bangladesh"),
                          labels=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees")) +
  ylab("z-standing height") +
  xlab("Age at migration to the UK (years)")

#Supplemental Figure 1. To visualise differences in morning salT, age at recruitment not adjusted----
ggplot(data = repro.data) + 
  geom_point(mapping = aes(x = AgeRecruit, y = z.log.meanS1D1D2, colour = residence19pub, shape = residence19pub), size=2.75) +
  geom_smooth(aes(x = AgeRecruit, y = z.log.meanS1D1D2, colour = residence19pub, linetype = residence19pub), method = "lm", se = T) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) +
  theme(axis.text = element_text(size = rel(1.5))) +
  theme(axis.title = element_text(size = rel(1.5))) +
  theme(legend.text = element_text(size = rel(1.2))) +
  theme(legend.position = c(.75, .9)) +
  guides(size=FALSE, fill=FALSE, alpha=F) + guides(shape = guide_legend(override.aes = list(fill=NA))) +
  scale_shape_discrete(name  ="Residence group",
                       breaks=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees"),
                       labels=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees")) +
  scale_colour_discrete(name  ="Residence group",
                        breaks=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees"),
                        labels=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees")) +
  scale_linetype_discrete(name  ="Residence group",
                          breaks=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees"),
                          labels=c("Child migrants", "Second generation migrants", "British European", "Adult migrants", "Bangladeshi sedentees")) +
  ylab("z-Testosterone (log)") +
  xlab("Age at recruitment, years")

#supplemental figure 2. plot of age 8 data number of years in UK-------
#from result: y19c.nyu.2salt.mssbmi.res

#establish reference grid for each sample time
salt1.rg <-ref.grid(y19c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2)
salt2.rg <- ref.grid(y19c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2)

#for effect of residence after adjustment for covariates
salt1.lsm <- lsmeans(salt1.rg, "y")
salt2.lsm <- lsmeans(salt2.rg, "y")

#for pairwise contrasts
contrast(salt1.rg, method = "pairwise")
contrast(salt2.rg, method = "pairwise")

#extract coefficients from each time point, label them by sample time and make into a dataframe for plotting
salt.lsm.data <- cbind(rbind(data.frame(summary(salt1.lsm)), data.frame(summary(salt2.lsm))), c(rep("morning", 2), rep("evening", 2)))
#rename variable for sample time
names(salt.lsm.data) <- c("mig.group", "lsmeans", "SE", "df", "lower.CL", "upper.CL", "sample")
#relevel sample time
salt.lsm.data$sample <- relevel(salt.lsm.data$sample, "morning")
#relevel residence groups
salt.lsm.data$mig.group <- factor(salt.lsm.data$mig.group, levels=c("Birth-9y", "9-19y"))


# LS mean Plots
pd = position_dodge(0.2)    ### How much to jitter the points on the plot

#plot of LS means of salT of child migs split at age 9 with number of years in UK
lsm.nyu.CHI.plt <- ggplot(salt.lsm.data,
                          aes(x     = sample,
                              y     = lsmeans,
                              color = mig.group)) +
  
  geom_point(shape  = 19,
             size   = 5,
             position = pd) +
  
  geom_errorbar(aes(ymin  =  lower.CL,
                    ymax  =  upper.CL),
                width =  0.2,
                size  =  0.7,
                position = pd) +
  
  theme_classic() +
  theme(axis.title   = element_text(size = rel(2)),
        axis.text    = element_text(size = rel(2)),
        legend.text  = element_text(size = rel(1.5)),
        legend.position = c(.9, .9),
        legend.title = element_blank()
  ) +
  
  ylab("Least square mean\n z-Testosterone (log)") +
  
  scale_color_manual(values = c("blue", "red"))
lsm.nyu.CHI.plt





#Figure S3 (not used). Boxplot of stature by residence group.
ggplot(repro.data, aes(x=residence19pub, y=z.height)) +
  theme_bw()+
  geom_boxplot(notch=TRUE)+
  geom_jitter(shape=16, size=1.5, position=position_jitter(0.1))+
  theme(axis.text.x = element_text(face="bold", size=14),
        axis.title.x = element_blank())+
  theme(axis.text.y = element_text(face="bold", size=15),
        axis.title.y = element_text(face="bold", size=18))


  
#TABLES-------------------------------------------------------------------------------

#summarize phys data using tapply
#Descriptives------------------------------------------------------
#create vectors of total, total-missing, mean and sd for vars "AgeRecruit", "Height", "Weight", "BMI", "MeanS1D1D2", "MeanS2D1D2", "MeanS3D1D2"

#total sample size and number of missing values for each varaible
n.tot<-tapply(X=repro.data$PartNum, INDEX=repro.data$residence19pub, FUN=length)
n.mss.age<-tapply(X=is.na(repro.data$AgeRecruit), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.height<-tapply(X=is.na(repro.data$Height), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.weight<-tapply(X=is.na(repro.data$Weight), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.bmi<-tapply(X=is.na(repro.data$BMI), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.s1d1d2<-tapply(X=is.na(repro.data$MeanS1D1D2), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.s3d1d2<-tapply(X=is.na(repro.data$MeanS3D1D2), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.pub.voice<-tapply(X=is.na(repro.data$PubVoice.n), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.pub.shave<-tapply(X=is.na(repro.data$PubShave.n), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.pub.pub<-tapply(X=is.na(repro.data$PubPub.n), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.pub.ne<-tapply(X=is.na(repro.data$PubNE.n), INDEX=repro.data$residence19pub, FUN=sum)
n.mss.pub.compos<-tapply(X=is.na(repro.data$pub.compos), INDEX=repro.data$residence19pub, FUN=sum)

#Percent responses to each pubertal measure
pct.pub.vo <- round((n.tot-n.mss.pub.voice)/n.tot*100, 1)
pct.pub.sh <- round((n.tot-n.mss.pub.shave)/n.tot*100, 1)
pct.pub.pub <- round((n.tot-n.mss.pub.pub)/n.tot*100, 1)
pct.pub.ne <- round((n.tot-n.mss.pub.ne)/n.tot*100, 1)
pct.pub.compos <- round((n.tot-n.mss.pub.compos)/n.tot*100, 1)
pct.bmi<-round((n.tot-n.mss.bmi)/n.tot*100, 1)

#table of missing values and percent responses
mss.tab.repro.data<-cbind(n.tot, n.mss.age, n.mss.height, n.mss.weight, n.mss.bmi, n.mss.s1d1d2, n.mss.s3d1d2, n.mss.pub.voice, pct.pub.vo, n.mss.pub.shave, pct.pub.sh, n.mss.pub.pub, pct.pub.pub, n.mss.pub.ne, pct.pub.ne, n.mss.pub.compos, pct.pub.compos)
mss.tab.repro.data
#convert to data frame and rename variables
mss.tab.repro.data<-as.data.frame(x=mss.tab.repro.data)
mss.tab.repro.data<-rename(mss.tab.repro.data, c("n.tot"="N", "n.mss.age"="Age at recruitment", "n.mss.height"="Height", "n.mss.weight"="Weight", "n.mss.bmi"="BMI", "n.mss.s1d1d2"="Waking Tsal", "n.mss.s3d1d2"="Evening Tsal", "n.mss.pub.vo"="Voice breaking", "pct.pub.vo"="Rsp % Voice breaking","n.mss.pub.sh"="Shaving", "pct.pub.sh"="Rsp % Shaving", "n.mss.pub.pub"="Pubic hair", "pct.pub.pub"="Rsp % Pubic hair", "n.mss.pub.ne"="Nocturnal emmision", "pct.pub.ne"="Rsp % Nocturnal emmision", "n.mss.pub.compos"="Composite age at puberty", "pct.pub.compos"="Rsp % Composite age at puberty"))
labels(mss.tab.repro.data)

cbind(pct.pub.vo, pct.pub.sh, pct.pub.pub, pct.pub.ne, pct.pub.compos)

#mean, sd, 95%ci

m.age<-round(tapply(X=repro.data$AgeRecruit, INDEX=repro.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.age<-round(tapply(X=repro.data$AgeRecruit, INDEX=repro.data$residence19pub, FUN=sd, na.rm=TRUE),1)
n.age<-round(tapply(X=repro.data$AgeRecruit, INDEX=repro.data$residence19pub, FUN=length),1)
ci.age <- aggregate(repro.data[,"AgeRecruit"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.height<-round(tapply(X=repro.data$Height, INDEX=repro.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.height<-round(tapply(X=repro.data$Height, INDEX=repro.data$residence19pub, FUN=sd, na.rm=TRUE),1)
n.height<-round(tapply(X=repro.data$Height, INDEX=repro.data$residence19pub, FUN=length),1)
ci.height <- aggregate(repro.data[,"Height"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.weight<-round(tapply(X=repro.data$Weight, INDEX=repro.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.weight<-round(tapply(X=repro.data$Weight, INDEX=repro.data$residence19pub, FUN=sd, na.rm=TRUE),1)
n.weight<-round(tapply(X=repro.data$Weight, INDEX=repro.data$residence19pub, FUN=length),1)
ci.weight <- aggregate(repro.data[,"Weight"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.bmi<-round(tapply(X=repro.data$BMI, INDEX=repro.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.bmi<-round(tapply(X=repro.data$BMI, INDEX=repro.data$residence19pub, FUN=sd, na.rm=TRUE),1)
ci.bmi <- aggregate(repro.data[,"BMI"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.s1d1d2<-round(tapply(X=repro.data$MeanS1D1D2, INDEX=repro.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.s1d1d2<-round(tapply(X=repro.data$MeanS1D1D2, INDEX=repro.data$residence19pub, FUN=sd, na.rm=TRUE),1)
ci.s1d1d2 <- aggregate(repro.data[,"MeanS1D1D2"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.s3d1d2<-round(tapply(X=repro.data$MeanS3D1D2, INDEX=repro.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.s3d1d2<-round(tapply(X=repro.data$MeanS3D1D2, INDEX=repro.data$residence19pub, FUN=sd, na.rm=TRUE),1)
ci.s3d1d2 <- aggregate(repro.data[,"MeanS3D1D2"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.pub.voice<-round(tapply(X=repro.data$PubVoice.n, INDEX=repro.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.pub.voice<-round(tapply(X=repro.data$PubVoice.n, INDEX=repro.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.pub.voice <- aggregate(repro.data[,"PubVoice.n"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.pub.shave<-round(tapply(X=repro.data$PubShave.n, INDEX=repro.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.pub.shave<-round(tapply(X=repro.data$PubShave.n, INDEX=repro.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.pub.shave <- aggregate(repro.data[,"PubShave.n"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.pub.pub<-round(tapply(X=repro.data$PubPub.n, INDEX=repro.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.pub.pub<-round(tapply(X=repro.data$PubPub.n, INDEX=repro.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.pub.pub <- aggregate(repro.data[,"PubPub.n"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.pub.ne<-round(tapply(X=repro.data$PubNE.n, INDEX=repro.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.pub.ne<-round(tapply(X=repro.data$PubNE.n, INDEX=repro.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.pub.ne <- aggregate(repro.data[,"PubNE.n"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.pub.compos<-round(tapply(X=repro.data$pub.compos, INDEX=repro.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.pub.compos<-round(tapply(X=repro.data$pub.compos, INDEX=repro.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.pub.compos <- aggregate(repro.data[,"pub.compos"], list(repro.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])

#all group totals/means
n.all <- nrow(repro.data)
m.age.all <- round(mean(repro.data$AgeRecruit, na.rm=TRUE),1)
m.height.all <- round(mean(repro.data$Height, na.rm=TRUE),1)
m.weight.all <- round(mean(repro.data$Weight, na.rm=TRUE),1)
m.bmi.all <- round(mean(repro.data$BMI, na.rm=TRUE),1)
m.s1d1d2.all <- round(mean(repro.data$MeanS1D1D2, na.rm=TRUE),1)
m.s3d1d2.all <- round(mean(repro.data$MeanS3D1D2, na.rm=TRUE),1)
m.pub.compos.all <- round(mean(repro.data$pub.compos, na.rm=TRUE),1)

#all group sds
sd.age.all <- round(sd(repro.data$AgeRecruit, na.rm=TRUE),2)
sd.height.all <- round(sd(repro.data$Height, na.rm=TRUE),2)
sd.weight.all <- round(sd(repro.data$Weight, na.rm=TRUE),2)
sd.bmi.all <- round(sd(repro.data$BMI, na.rm=TRUE),2)
sd.s1d1d2.all <- round(sd(repro.data$MeanS1D1D2, na.rm=TRUE),2)
sd.s3d1d2.all <- round(sd(repro.data$MeanS3D1D2, na.rm=TRUE),2)
sd.pub.compos.all <- round(sd(repro.data$pub.compos, na.rm=TRUE),2)

m.all <- c(m.age.all, m.height.all, m.weight.all, m.bmi.all, m.s1d1d2.all, m.s3d1d2.all, m.pub.compos.all)
sd.all <- c(sd.age.all, sd.height.all, sd.weight.all, sd.bmi.all, sd.s1d1d2.all, sd.s3d1d2.all, sd.pub.compos.all)

#function for pasting mean and SDs together
mean.sd.f <- function(m.var, sd.var){
  paste(sprintf("%.1f", m.var)," (",round(sd.var,1),")",sep="") 
}
msd.all <- c(n.all, mean.sd.f(m.all, sd.all))

#mean and standard deviation
msd.age <- paste(sprintf("%.1f", m.age)," (",round(sd.age,2),")",sep="") 
msd.height <- paste(sprintf("%.1f", m.height)," (",round(sd.height,2),")",sep="") 
msd.weight <- paste(sprintf("%.1f", m.weight)," (",round(sd.weight,2),")",sep="")
msd.bmi <- paste(sprintf("%.1f", m.bmi)," (",round(sd.bmi,2),")",sep="")
msd.s1d1d2 <- paste(sprintf("%.1f", m.s1d1d2)," (",round(sd.s1d1d2,2),")",sep="")
msd.s3d1d2 <- paste(sprintf("%.1f", m.s3d1d2)," (",round(sd.s3d1d2,2),")",sep="")
msd.pub.voice <- paste(sprintf("%.1f", m.pub.voice)," (",round(sd.pub.voice,2),")",sep="")
msd.pub.shave <- paste(sprintf("%.1f", m.pub.shave)," (",round(sd.pub.shave,2),")",sep="")
msd.pub.pub <- paste(sprintf("%.1f", m.pub.pub)," (",round(sd.pub.pub,2),")",sep="")
msd.pub.ne <- paste(sprintf("%.1f", m.pub.ne)," (",round(sd.pub.ne,2),")",sep="")
msd.pub.compos <- paste(sprintf("%.1f", m.pub.compos)," (",round(sd.pub.compos,2),")",sep="")


#mean and ci (for text)
#a function for pasting mean and ci together
mean.ci <- function(m.var, ci.var){
  paste(sprintf("%.1f", m.var),", 95%CI=", paste(round(as.numeric(ci.var$x)[1:5],1), ", ", round(as.numeric(ci.var$x)[6:10],1), sep=""),sep="")
}
m.var.ls <- c("m.age", "m.height", "m.weight", "m.bmi", "m.s1d1d2", "m.s2d1d2", "m.s3d1d2", "m.pub.voice", "m.pub.shave", "m.pub.pub", "m.pub.ne", "m.pub.compos")
ci.var.ls <- c("ci.age", "ci.height", "ci.weight", "ci.bmi", "ci.s1d1d2", "ci.s2d1d2", "ci.s3d1d2", "ci.pub.voice", "ci.pub.shave", "ci.pub.pub", "ci.pub.ne", "ci.pub.compos")
cbind(noquote(m.var.ls[1]), noquote(ci.var.ls))
mean.ci(m.age, ci.age)
mean.ci(m.s1d1d2, ci.s1d1d2)
mean.ci(m.pub.compos, ci.pub.compos)
mean.ci(m.s1d1d2, ci.s1d1d2)
mean.ci(m.s3d1d2, ci.s3d1d2)

#combine into table using "cbind"
mig.tab.data<-cbind(n.tot, m.age, sd.age, m.height, sd.height, m.weight, sd.weight, m.bmi, sd.bmi, m.s1d1d2, sd.s1d1d2, m.s3d1d2, sd.s3d1d2, m.pub.compos, sd.pub.compos)

#convert to data frame and rename variables
mig.tab.data<-as.data.frame(x=mig.tab.data)

#for a reduced table of mean (sd), including composite age of puberty only
rn <- names(m.age)  #rownames
cn <- c(c("N", "Age, y", "Height, cm", "Weight, kg", "BMI", "Waking, pg/mL", "Evening, pg/mL", "Recalled age at puberty, y")) #column names
mig.tab.msd <- matrix(c(n.tot, msd.age, msd.height, msd.weight, msd.bmi, msd.s1d1d2, msd.s3d1d2, msd.pub.compos),5,8,dimnames=list(rn,cn))
#reorder and rename of residence groups
res.order <- c("Bangladeshi sedentees", "Adult migrants", "Child migrants", "Second generation migrants", "British European")
mig.tab.msd <- mig.tab.msd[match(res.order, rn),]
mig.tab.msd <- rbind(mig.tab.msd, msd.all) 
rownames(mig.tab.msd) <- c("Bangladeshi sedentees", "Adult migrants", "Child migrants", "Second generation migrants", "British European", "All groups")

bhai.desc.tab.ht <- stargazer(mig.tab.msd, digits=1, out="bhai.desc.tab.html", summary=FALSE, type="html")

#REGRESSION TABLES--------------------------------------------------
##Note: for HTML tables, open file generated by htmlTable {knitr} or stargazer in Firefox browser, copy and paste into LibreOffice, then open as Word document

#Table 1.1 SalT by residence: lapply both sampling variables and get summary
res.2salt.tab.ht <- stargazer(res.2salt.res, 
                              title="Salivary testosterone by residence group", 
                              dep.var.caption  = "<em> sample time </em>", 
                              out="res.2salt.tab.html", 
                              column.labels=c("Waking", "Evening"), 
                              covariate.labels=c("Constant", "Age(log)", "BMI", "Adult migrants", "Child migrants", "Second generation migrants", "British European"), 
                              notes ="all values are z-transformed SD units, age and testosterone also log transformed. Reference category: Bangladeshi sedentees", 
                              type="html", 
                              dep.var.labels="", 
                              star.cutoffs = c(0.05, 0.01, 0.001), 
                              report=('vc*ps'), 
                              ci= T, 
                              intercept.bottom = FALSE, 
                              single.row = TRUE, 
                              model.numbers=F
)
## for Wake and Bed samples YOM and 2NG salT is higher compared to SED, BMI also sig. covariate

#for n by residence group
summary(model.frame(res.2salt.res$z.log.meanS1D1D2)$y)
summary(model.frame(res.2salt.res$z.log.meanS3D1D2)$y)

#Table 1.2 Puberty by residence group:
res.pub.tab.ht <- stargazer(res.pub.res$z.pub.compos, 
                            title="Recalled age at puberty by residence group", 
                            out="res.pub.tab.html",
                            column.labels=c("Composite age at puberty"), 
                            covariate.labels=c("Constant", "Age(log)", "Adult Migrants", "Child Migrants", "Second Generation Migrants", "British-born European"), 
                            notes ="all values are z-transformed SD units, age also log transformed. Reference category: Bangladeshi Sedentees", 
                            type="html", 
                            dep.var.caption  = "<em> measure </em>", dep.var.labels="", 
                            star.cutoffs = c(0.05, 0.01, 0.001), 
                            report=('vc*ps'),
                            ci= T, 
                            intercept.bottom = FALSE, 
                            model.numbers=F,  
                            single.row = TRUE
)
#compared to sedentees, 2NG and EUR are both sig. earlier age of puberty in composite and all measures (besides voice for 2NG)

#Table S1. Post-hoc table
# Table for export results of multiple comparison (post hoc Tukey)
# Source: Modified from https://gist.github.com/cheuerde/3acc1879dc397a1adfb0 
# x is a ghlt object

table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), 
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)
  
}

#save Wake and Bed contrasts as a new object, then save an html version of new contrasts for both time points (rows 5:10) with stargazer
res.salt1.res.ph.tab <- table_glht(res.salt1.res.ph)
res.salt1.res.ph.tab[5:10,]

res.salt3.res.ph.tab <- table_glht(res.salt3.res.ph)
res.salt3.res.ph.tab[5:10,]

res.salt.ph.tab <- cbind(res.salt1.res.ph.tab[5:10,], res.salt3.res.ph.tab[5:10,])

res.salt.ph.tab.ht <- stargazer(res.salt.ph.tab, digits=2, out="bhai.ph.tab.html", summary=FALSE, type="html")

#Table S2.
#Age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" amu regressions in CHI under 19yo migs only with imputed BMI
nyu.yom.amu.2salt.mssbmi.tab <- stargazer(nyu.yom.amu.2salt.mssbmi.res, 
                                          title="Salivary Testosterone by number of years in the UK and age of migration, child migrants &le;19y only, imputed BMI", 
                                          dep.var.caption  = "<em> sample time </em>",
                                          out="nyu.yom.amu.2salt.mssbmi.tab.html", 
                                          column.labels=c("Waking","Evening"), 
                                          covariate.labels=c("Constant", "Number of years in the UK", "BMI (imputed)", "Age at migration"), 
                                          notes =c("all values are z-transformed SD units, age and testosterone also log transformed", "BMI imputed for n=8 at population mean 23.9"), 
                                          type="html", 
                                          dep.var.labels="", 
                                          star.cutoffs = c(0.05, 0.01, 0.001),
                                          report=('vc*ps'),
                                          ci= T, 
                                          intercept.bottom = FALSE, 
                                          single.row = TRUE, 
                                          model.numbers=F
                                          )


#Table S3.
#Age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only
nyu.yom.amu.2salt.tab <- stargazer(nyu.yom.amu.2salt.res, 
                                   title="Salivary Testosterone by number of years in the UK and age of migration, Child migrants &le;19y", 
                                   dep.var.caption  = "<em> sample time </em>",
                                   out="nyu.yom.amu.2salt.tab.html", 
                                   column.labels=c("Waking","Evening"), 
                                   covariate.labels=c("Constant", "Number of years in the UK", "BMI", "Age at migration"), 
                                   notes ="all values are z-transformed SD units, age and testosterone also log transformed.",
                                   type="html", 
                                   dep.var.labels="", 
                                   star.cutoffs = c(0.05, 0.01, 0.001), 
                                   report=('vc*ps'),
                                   ci= T, 
                                   intercept.bottom = FALSE, 
                                   single.row = TRUE, 
                                   model.numbers=F
                                   )


#Table S4. 
#SalT within CHI only, with age at recruitment: migration before age 9 as compared to migration age 9-19 mssbmi regression
y19b.age.2salt.mssbmi.tab.ht <- stargazer(y19b.age.2salt.mssbmi.res, 
                                          title="Effect of age of migration (split at age 9) on salivary testosterone of child migrants with imputed BMI", 
                                          dep.var.caption  = "<em> sample time </em>", 
                                          out="y19b.age.2salt.mssbmi.tab.html", 
                                          column.labels=c("Waking", "Evening"), 
                                          covariate.labels=c("Constant", "Age(log)", "BMI (imputed)", "Infancy or early childhood migrants (birth-8 years)"), 
                                          notes ="all values are z-transformed SD units, age and testosterone also log transformed. BMI imputed for n=8 at population mean 23.9 Reference category: Late childhood migrants (9-19 years)", 
                                          type="html", 
                                          dep.var.labels="", 
                                          star.cutoffs = c(0.05, 0.01, 0.001),
                                          report=('vc*ps'),
                                          ci= T, 
                                          intercept.bottom = FALSE, 
                                          single.row = TRUE, 
                                          model.numbers=F
)

#Table S5. 
#SalT within CHI only, migration before age 9 as compared to migration age 9-19 "age.19.mig.b" regression
y19c.nyu.2salt.ht <- stargazer(y19c.nyu.2salt.res, 
                               title="Effect of age of migration (split at age 9) on salivary testosterone of child migrants", 
                               dep.var.caption  = "<em> sample time </em>", 
                               out="y19c.nyu.2salt.html", 
                               column.labels=c("Waking", "Evening"), 
                               covariate.labels=c("Constant", "Number of years in the UK", "BMI", "Infancy or early childhood migrants (birth-8 years)"), 
                               notes ="all values are z-transformed SD units, age and testosterone also log transformed. Reference category: Late childhood migrants (9-19 years)", 
                               type="html", 
                               dep.var.labels="", 
                               star.cutoffs = c(0.05, 0.01, 0.001),
                               report=('vc*ps'),
                               ci= T, 
                               intercept.bottom = FALSE, 
                               single.row = TRUE, 
                               model.numbers=F
                               )

#Table S6.
#puberty by age mig for 2NG and CHI groups above and below age 9
y19c.age.pub.res <- stargazer(cat.19pub.res$z.pub.compos, 
                              title="Effect of age of migration (split at age 9) on recalled age at puberty of child migrants", 
                              dep.var.caption  = "<em> sample time </em>", 
                              out="y19c.pub.tab.html", 
                              column.labels=c("Composite age at puberty"), 
                              covariate.labels=c("Constant", "Age at recruitment (log)", "Late childhood migrants (9-19 years)", "Infancy or early childhood migrants (birth-8 years)", "Pre-birth (Born in UK)"), 
                              notes ="all values are z-transformed SD units. Reference category: Adult migrants >19 years", 
                              type="html", 
                              dep.var.labels="", 
                              star.cutoffs = c(0.05, 0.01, 0.001), 
                              report=('vc*ps'),
                              ci= T, 
                              intercept.bottom = FALSE, 
                              single.row = TRUE, 
                              model.numbers=F
                              )

#Table S7.
#SalT within ADU only, age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi amu regressions in ADU over 19yo migs only
nyu.adu.amu.2salt.ht <- stargazer(nyu.adu.amu.2salt.res, 
                                  title="Effect of age of migration salivary testosterone of adult migrants >19y only", 
                                  dep.var.caption  = "<em> sample time </em>", 
                                  out="nyu.adu.amu.2salt.html", 
                                  column.labels=c("Waking", "Evening"), 
                                  covariate.labels=c("Constant", "Number of years in the UK", "BMI", "Age at migration"), 
                                  notes ="all values are z-transformed SD units, age and testosterone also log transformed.", 
                                  type="html", 
                                  dep.var.labels="", 
                                  star.cutoffs = c(0.05, 0.01, 0.001),
                                  report=('vc*ps'),
                                  ci= T, 
                                  intercept.bottom = FALSE, 
                                  single.row = TRUE, 
                                  model.numbers=F
                                  )

#Table S8a.
#age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 19yo migs only, <40yo age at recruitment cohort
nyu.adu40a.amu.2salt.tab <- stargazer(nyu.adu40a.amu.2salt.res, 
                                      title="Salivary testosterone by number of years in the UK and age of migration, adult migrants, aged &le;40 years at recruitment", 
                                      dep.var.caption  = "<em> sample time </em>",
                                      out="nyu.adu40a.amu.3salt.tab.html", 
                                      column.labels=c("Waking","Evening"), 
                                      covariate.labels=c("Constant", "Number of years in the UK", "BMI", "Age at migration"), 
                                      notes ="all values are z-transformed SD units, age and testosterone also log transformed",
                                      type="html", 
                                      dep.var.labels="", 
                                      star.cutoffs = c(0.05, 0.01, 0.001),
                                      report=('vc*ps'),
                                      ci= T, 
                                      intercept.bottom = FALSE, 
                                      single.row = TRUE, 
                                      model.numbers=F
                                      )
##for Age Mig within younger Adult migs <40 only, number of years in the UK and bmi as covariates, neither waking nor evening time points were sig. predicted by any of the covariates

#Table S8b.
#age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 19yo migs only, >40yo age at recruitment cohort
nyu.adu40b.amu.2salt.tab <- stargazer(nyu.adu40b.amu.2salt.res, 
                                      title="Salivary Testosterone by number of years in the UK and age of migration, adult migrants, aged >40 years at recruitment", 
                                      dep.var.caption  = "<em> sample time </em>",
                                      out="nyu.adu40b.amu.3salt.tab.html", 
                                      column.labels=c("Waking","Evening"), 
                                      covariate.labels=c("Constant", "Number of years in the UK", "BMI", "Age at migration"), 
                                      notes ="all values are z-transformed SD units, age and testosterone also log transformed",
                                      type="html", 
                                      dep.var.labels="", 
                                      star.cutoffs = c(0.05, 0.01, 0.001), 
                                      report=('vc*ps'),
                                      ci= T, 
                                      intercept.bottom = FALSE, 
                                      single.row = TRUE, 
                                      model.numbers=F
                                      )
##for Age Mig within older Adult migs >40 only, number of years in the UK and bmi as covariates, neither waking nor evening time points were sig. predicted by any of the covariates

#Table S9. Untransformed linear regression analysis of age-related slopes in salivary T
# list of lm objects
agerec.salT.res.l <- list(sed.s1d1d2.age.res, sed.s3d1d2.age.res, adu.s1d1d2.age.res, adu.s3d1d2.age.res, chi.s1d1d2.age.res, chi.s3d1d2.age.res, sg.s1d1d2.age.res, sg.s3d1d2.age.res, eur.s1d1d2.age.res, eur.s3d1d2.age.res, agerec.2salt.ut.res$MeanS1D1D2, agerec.2salt.ut.res$MeanS3D1D2, res.age.ukborn.s1, res.age.ukborn.s3)
#names for the list
names(agerec.salT.res.l) <- c("sed.s1d1d2.age.res", "sed.s3d1d2.age.res", "adu.s1d1d2.age.res", "adu.s3d1d2.age.res", "chi.s1d1d2.age.res", "chi.s3d1d2.age.res", "sg.s1d1d2.age.res", "sg.s3d1d2.age.res", "eur.s1d1d2.age.res", "eur.s3d1d2.age.res", "allgr.s1d1d2.age.res", "allgr.s3d1d2.age.res", "res.age.ukborn.s1", "res.age.ukborn.s3")

# inspect models
# agerec.salT.res.l

# function for combining values

agerec.fun <-
  function(lm)
  {
    out <- c(lm$coefficients[1],
             lm$coefficients[2],
             confint(lm)[2,],
             length(lm$model$AgeRecruit),
             summary(lm)$coefficients[2,2],
             pf(summary(lm)$fstatistic[1], summary(lm)$fstatistic[2],
                summary(lm)$fstatistic[3], lower.tail = FALSE),
             summary(lm)$r.squared)
    names(out) <- c("intercept","slope", "2.5% CI", "97.5% CI", "n","slope.SE","p.value","r.squared")
    return(out)}

# for single line
# agerec.fun(agerec.salT.res.l$sg.s3d1d2.age.res)

#for a list of lm models:

agerec.results <- list()
for (i in 1:length(agerec.salT.res.l)) agerec.results[[names(agerec.salT.res.l)[i]]] <- agerec.fun(agerec.salT.res.l[[i]])

agerec.df <- as.data.frame(agerec.results)
round(agerec.df["slope",], 2)
#select and transpose rounded waking values from agerec.results into table with slope, (95% CI), r2, and p for am samples (odd number columns)
agerec.wk.tab <- round(t(agerec.df[c("slope", "2.5% CI", "97.5% CI","p.value","r.squared"),c(1, 3, 5, 7, 9, 11, 13)]),3)
#select and transpose evening values from agerec.results into table with slope, (95% CI), r2, and p for am samples (even number columns)
agerec.bed.tab <- round(t(agerec.df[c("slope", "2.5% CI", "97.5% CI","p.value","r.squared"),c(2, 4, 6, 8, 10, 12, 14)]),3)
#combine 95% ci values into a single vector
agerec.wk.ci <- paste0(as.numeric(agerec.wk.tab[,"2.5% CI"]), ", ", as.numeric(agerec.wk.tab[,"97.5% CI"]))
agerec.bed.ci <- paste0(as.numeric(agerec.bed.tab[,"2.5% CI"]), ", ", as.numeric(agerec.bed.tab[,"97.5% CI"]))
agerec.tab <- as.data.frame(cbind(agerec.wk.tab[,"slope"], agerec.wk.ci, agerec.wk.tab[,c("r.squared", "p.value")], agerec.bed.tab[,"slope"], agerec.bed.ci, agerec.bed.tab[,c("r.squared", "p.value")]))

rn <- c(levels(repro.data$residence19pub), "All groups", "UK-born groups only")  #rownames
cn <- c("slope (pg/ml per year)", "95% CI", "r2", "p") #column names, note they cannot be repeated twice as labels of a dataframe, add in excel instead
row.names(agerec.tab) <- rn

stargazer(agerec.tab, summary=F, column.labels=c("slope (pg/ml per year)", "95% CI", "r2", "p"), out="agerec.tab.html", type = "html")

#Table S10a
#Waking salT is a predictor of pub traits (composite and voice)
salt1.pub.res.v <- salt1.pub.res[c("z.pub.compos", "z.pub.voice")]
salt1.pub.res.ht <- stargazer(salt1.pub.res.v,
                              title="Waking salivary testosterone as predictor of recalled age at puberty", 
                              out="salt1.pub.res.html", 
                              column.labels=c("Composite age at puberty", "Voice breaking at puberty"), 
                              covariate.labels=c("Constant", "Age(log)", "Waking Salivary Testosterone"), 
                              notes ="all values are z-transformed SD units, age also log transformed.", 
                              type="html", dep.var.caption  = "<em> measure </em>", 
                              dep.var.labels="", 
                              star.cutoffs = c(0.05, 0.01, 0.001), 
                              report=('vc*ps'),
                              ci= T, 
                              intercept.bottom = FALSE, 
                              model.numbers=F,  
                              single.row = TRUE
                              )
#across all groups, waking salT is sig neg predictor of NE and composite variables.

#Table S10b
#Bed adult salT is a predictor of pub traits
salt3.pub.res <- lapply(z.pub, y=repro.data$z.log.meanS3D1D2, db=repro.data, age.lm)
salt3.pub.sres <- lapply(salt3.pub.res, summary)
salt3.pub.res.ht <- stargazer(salt3.pub.res$z.pub.compos, 
                              title="Evening salivary testosterone as predictor of recalled age at puberty", 
                              out="salt3.pub.res.html", column.labels=c("Composite age at puberty"), 
                              covariate.labels=c("Constant", "Age(log)", "Evening Salivary Testosterone"), 
                              notes ="all values are z-transformed SD units, age also log transformed.", 
                              type="html", dep.var.caption  = "<em> measure </em>", 
                              dep.var.labels="", star.cutoffs = c(0.05, 0.01, 0.001), 
                              report=('vc*ps'),
                              ci= T, 
                              intercept.bottom = FALSE, 
                              model.numbers=F,  
                              single.row = TRUE
                              )
#across all groups, Bed salT is sig neg predictor of age at NE and compos

#Table S11.
#Height by age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only
height.agemig.nyu.chi.tab <- stargazer(height.agemig.nyu.chi.res, 
                                   title="Standing height by number of years in the UK and age of migration, Child migrants &le;19y",
                                   out="height.agemig.nyu.chi.tab.html",
                                   covariate.labels=c("Constant", "Number of years in the UK", "Age at migration"), 
                                   notes ="all values are z-transformed SD units.",
                                   type="html", 
                                   dep.var.labels="", 
                                   star.cutoffs = c(0.05, 0.01, 0.001), 
                                   report=('vc*ps'),
                                   ci= T, 
                                   intercept.bottom = FALSE, 
                                   single.row = TRUE, 
                                   model.numbers=F
)

#Table S12.
#Height by age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: age migrated to UK "amu" regressions in CHI under 19yo migs only
height.agemig.agerec.chi.tab <- stargazer(height.agemig.agerec.chi.res, 
                                       title="Standing height by age at recruitment and age of migration, Child migrants &le;19y",
                                       out="height.agemig.agerec.chi.tab.html",
                                       covariate.labels=c("Constant", "Age at recruitment", "Age at migration"), 
                                       notes ="all values are z-transformed SD units.",
                                       type="html", 
                                       dep.var.labels="", 
                                       star.cutoffs = c(0.05, 0.01, 0.001), 
                                       report=('vc*ps'),
                                       ci= T, 
                                       intercept.bottom = FALSE, 
                                       single.row = TRUE, 
                                       model.numbers=F
)

#SUPPLEMENTAL ANALYSES OF <40y age at recruitment subsets--------

#Descriptives: 
#age differences between residence groups <40y at recruitment, split at age 19
age.40a.res <- lm(AgeRecruit~residence19pub, data=age40a.data)
age.40a.sres <- summary(age.40a.res)
##Second generation and adult migrants significantly different from sedentees (ADU older, 2NG younger). EUR not different from SED
tapply()
#post-hoc analysis
age.40a.res.ph <- summary(glht(age.40a.res, linfct=mcp(residence19pub="Tukey")))
## Age: ADU > CHI, 2NG; EUR > 2NG: CHI > 2NG 
##Age not diff between EUR and CHI

#within migrants, does age at migration correlate with age at recruitment? 
#In adult migrants
cor.test(subset(age40a.data, residence19pub=="Adult migrants")$z.log.age, as.vector(scale(log(subset(age40a.data, residence19pub=="Adult migrants")$AgeMigUK))))
#no indication of correlation between age at recruitment and age at migration in Adult migrants
#n.s. correlation:
cor.test(subset(age40a.data, residence19pub=="Child migrants")$AgeRecruit, as.vector(scale(log(subset(age40a.data, residence19pub=="Child migrants")$AgeMigUK))))
#correlated, suggests the effects of peak migration.

#bmi differences between residence groups
bmi.40a.res <- lm(BMI~residence19pub, data=age40a.data)
bmi.40a.sres <- summary(bmi.40a.res)
##All migrant groups significantly different from sedentees (all higher).
#post-hoc analysis
bmi.40a.res.ph <- summary(glht(bmi.40a.res, linfct=mcp(residence19pub="Tukey")))
##No differences in BMI bewteen men resident in the UK

#height differences between residence groups, split at age 19
height.40a.res <- lm(Height~residence19pub, data=age40a.data)
height.40a.sres <- summary(height.40a.res)
confint(height.40a.res)
hist(age40a.data$Height)
##all but adult males are taller than sedentees.
#post-hoc analysis
height.40a.res.ph <- summary(glht(height.40a.res, linfct=mcp(residence19pub="Tukey")))
confint(height.40a.res.ph)
summary(model.frame(height.40a.res))
##EUR>2NG=CHI=ADU. 2NG>ADU Europeans taller than all other groups.

#height differences between residence groups, split at age 19, adjusting for secular trend with age at recruitment
height.age.40a.res <- lm(z.height~z.log.age+residence19pub, data=age40a.data)
height.age.40a.sres <- summary(height.age.40a.res)
##adjusting for age, all groups except ADU are taller than sedentees.
#post-hoc analysis
height.age.40a.res.ph <- summary(glht(height.age.40a.res, linfct=mcp(residence19pub="Tukey")))
##EUR>2NG=CHI=ADU. 2NG>ADU Europeans taller than all other groups.

#do groups differ from sedentees in their relationship between height and age (examine interaction between age at recruitment and height)
height.ageint.40a.res <- lm(z.height~z.log.age*residence19pub, data=age40a.data)
height.ageint.40a.sres <- summary(height.ageint.40a.res)
confint(height.ageint.40a.res)
summary(model.frame(height.ageint.40a.res))
##no interactions effects between height and residence group.

#secular trends in height of men who shared same childhood ecology?
height.bdadu.40a.res <- lm(z.height~z.log.age, data=subset(age40a.data, ukbd.born.adu=="Reached adulthood in Bangladesh"))
height.bdadu.40a.sres <- summary(height.bdadu.40a.res)
confint(height.bdadu.40a.res)
height.ukadu.40a.res <- lm(z.height~z.log.age, data=subset(age40a.data, ukbd.born.adu=="Reached adulthood in UK"))
height.ukadu.40a.sres <- summary(height.ukadu.40a.res)
confint(height.ukadu.40a.res)
##in both groups, no evidence of secular trend in height.

#weight differences between residence groups, split at age 19
weight.40a.res <- lm(Weight~residence19pub, data=age40a.data)
weight.40a.sres <- summary(weight.40a.res)
##all groups are heavier than sedentees.
#post-hoc analysis
weight.40a.res.ph <- summary(glht(weight.40a.res, linfct=mcp(residence19pub="Tukey")))
##No difference between all UK resident groups

#is there a secular trend/age effect on recalled age at puberty?
pub.40a.res <- lm(z.pub.compos~z.log.age, data=age40a.data)
pub.40a.sres <- summary(pub.40a.res)
confint(pub.40a.res)
##overall, older men recall age at puberty later. This may be secular trend, systematic recall bias or developmental effect
pub.bdadu.40a.res <- lm(z.pub.compos~z.log.age, data=subset(age40a.data, ukbd.born.adu=="Reached adulthood in Bangladesh"))
pub.bdadu.40a.sres <- summary(pub.bdadu.40a.res)
confint(pub.bdadu.40a.res)
pub.ukadu.40a.res <- lm(z.pub.compos~z.log.age, data=subset(age40a.data, ukbd.born.adu=="Reached adulthood in UK"))
pub.ukadu.40a.sres <- summary(pub.ukadu.40a.res)
#within both groups sharing similar childhood conditions, no secular trend or a systematic recall bias.

#Hypot 1.1a SalT by residence: sd units (log)age and bmi as covariates under 40 only-------
res.2salt.40a.res <- lapply(z.log.age40a.2salt, y=age40a.data$residence19pub, db=age40a.data, age.bmi.lm)
res.2salt.40a.sres <- lapply(res.2salt.40a.res, summary)
## for Wake and Bed samples CHI and 2NG <40 salT is higher compared to SED
## Contrast with 1.1. Waking ADU salT is non-sig compared to SED, for Bed salT nearlly sig higher p=0.07; BMI nonsig. covariate

#for post-hoc multiple comparison Tukey correction of all-pair multiple comparison
res.salt1.40a.res.ph <- summary(glht(res.2salt.40a.res$z.log.meanS1D1D2, linfct=mcp(y="Tukey")))
## Wake: CHI > ADU; 2NG > ADU; CHI > EUR.
##contrast with 1.1.ph: CHI is higher than EUR, 2NG n.s.
res.salt3.40a.res.ph <- summary(glht(res.2salt.40a.res$z.log.meanS3D1D2, linfct=mcp(y="Tukey")))
#Bed: No other sig ph diffs

#Hypot 1.2a age at migration "z.AgeMigUK" as continuous predictor after adjusting for number of years in uk "nyu" and age at migration regressions in all migs-----------
nyu.amu.2salt.40a.res <- lapply(z.log.age40a.2salt, y=age40a.data$z.AgeMigUK, db=age40a.data, nyu.bmi.lm)
nyu.amu.2salt.40a.sres <- lapply(nyu.amu.2salt.40a.res, summary)
lapply(nyu.amu.2salt.40a.res, confint)
#vif(nyu.amu.2salt.res$z.log.meanS1D1D2)
##Number of years in UK and age of migration significant negative predictors of evening, but not waking salivary T for all migrants <40
##Contrast with 1.2: waking salivary T n.s. affected by age at migration for all migrants <40

#Hypot 4.1.1a <40 age at recruitment is continuous predictor of salivary testosterone across all populations (untransformed for comparison to other published declines)-----
agerec.2salt.ut.40a.res <- lapply(unt.40a.2salt, db=age40a.data, agerec.lm)
agerec.2salt.ut.40a.sres <- lapply(agerec.2salt.ut.40a.res, summary)
agerec.2salt.ut.40a.ci <- lapply(agerec.2salt.ut.40a.res, confint)
agerec.wksalt.40a.n <- length(resid(agerec.2salt.ut.40a.res$MeanS1D1D2))
agerec.bdsalt.40a.n <- length(resid(agerec.2salt.ut.40a.res$MeanS3D1D2))
##sig. negative decline in wake salT of 1.56 pg/mL per year, sig. negative decline in bed salT of 1.61 pg/mL per year. 

#also applying transformed data, <40y at recruitment
agerec.2salt.40a.res <- lapply(z.log.age40a.2salt, db=age40a.data, function(x, db){
  lm(x~z.log.age, data=db)	
})
agerec.2salt.40a.sres <- lapply(agerec.2salt.40a.res, summary)
agerec.2salt.40a.ci <- lapply(agerec.2salt.40a.res, confint)
##note z.log.age at recruitment is not sig. for Waking (p=0.053) with z.trans values when restricting to <40y at recruitment

#Hypot 4.1.2a <40 age at recruitment is continuous predictor of salivary testosterone across all populations, including bmi as a covaraite (untransformed for comparison to other published declines)-----
age.bmi.2salt.ut.40a.res <- lapply(unt.40a.2salt, db=age40a.data, agerec.bmi.lm)
age.bmi.2salt.ut.40a.sres <- lapply(age.bmi.2salt.ut.40a.res, summary)
age.bmi.2salt.ut.40a.ci <- lapply(age.bmi.2salt.ut.40a.res, confint)
age.bmi.wksalt.40a.n <- length(resid(age.bmi.2salt.ut.40a.res$MeanS1D1D2))
age.bmi.bdsalt.40a.n <- length(resid(age.bmi.2salt.ut.40a.res$MeanS3D1D2))
##adjusting for BMI, sig. negative decline in wake salT of 2.27 (-3.87, -0.68) pg/mL per year, sig. negative decline in bed salT of 1.82 (-3.14, -0.513) pg/mL per year.

#applying transformed data with bmi, <40y at recruitment
agerec.bmi.2salt.40a.res <- lapply(z.log.age40a.2salt, db=age40a.data, function(x, db){
  lm(x~z.log.age+z.bmi, data=db)	
})
agerec.bmi.2salt.40a.sres <- lapply(agerec.bmi.2salt.40a.res, summary)
agerec.bmi.2salt.40a.ci <- lapply(agerec.bmi.2salt.40a.res, confint)
##z.log.age and z.bmi sig. for both sample times

#Hypot 4.1.2b >40 age at recruitment is continuous predictor of salivary testosterone across all populations, including bmi as a covaraite (untransformed for comparison to other published declines)-----
age.bmi.2salt.ut.40b.res <- lapply(unt.40b.2salt, db=age40b.data, agerec.bmi.lm)
age.bmi.2salt.ut.40b.sres <- lapply(age.bmi.2salt.ut.40b.res, summary)
age.bmi.2salt.ut.40b.ci <- lapply(age.bmi.2salt.ut.40b.res, confint)
age.bmi.wksalt.40b.n <- length(resid(age.bmi.2salt.ut.40b.res$MeanS1D1D2))
age.bmi.bdsalt.40b.n <- length(resid(age.bmi.2salt.ut.40b.res$MeanS3D1D2))
##adjusting for BMI, sig. negative decline in wake salT of -1.313 (-2.233120  -0.3935973) pg/mL per year, sig. negative decline in bed salT of -1.88 (-2.7985791  -0.9613626) pg/mL per year.

#applying transformed data with bmi, >40y at recruitment
agerec.bmi.2salt.40b.res <- lapply(z.log.age40b.2salt, db=age40b.data, function(x, db){
  lm(x~z.log.age+z.bmi, data=db)	
})
agerec.bmi.2salt.40b.sres <- lapply(agerec.bmi.2salt.40b.res, summary)
agerec.bmi.2salt.40b.ci <- lapply(agerec.bmi.2salt.40b.res, confint)
##z.log.age and z.bmi sig. for both sample times

##The overall population decline within men <40 is closer to that reported elsewhere in longitudinal studies by Harman et al 2001 The rate of decline of serum testosterone in Caucasion men according to Harman et al is 0.110 nmol/L per year.  This is 0.11 x 0.288 = 0.032 ng/ml per year or 32 pg/ml per year.  
##Salivary testosterone according to Wang et al is 1/15 that of the serum concentration across a wide range of values.  Therefore, this gives 32 x 1/15 = 2.133 pg/ml per year. 
##We observe something closer to this within men <40 after adjusting for BMI: -2.27 (-3.87, -0.68) pg/mL per year and a sig. negative decline in bed salT of -1.82 (-3.14, -0.513) pg/mL per year.
##We observe something closer to this within men >40 after adjusting for BMI: -1.313 (-2.23, -0.39) pg/mL per year and a sig. negative decline in bed salT of -1.88 (-2.80, -0.96) pg/mL per year.
##Indicates that the sig age-related declines are most pronounced at older and younger ages in this population. This was supported by confining analysis a 'middle aged' cohort between age 30-50, where there was not an age-related decline observed at all (not shown here) ##adjusting for BMI, non-sig decline in wake salT of men 30-50y -0.7941 (-2.558085   0.969888) pg/mL per year, non-sig. pos trend for bed salT of 0.80 (-0.8594868  2.473803) pg/mL per year.

#Hypot 4.2.1a restricting analysis to men <40 are the slopes significantly different overall or between groups for salT?-------
#method: ANCOVA of salT as dependent variable with age as predictor including all groups
res.agerec.40a.res <- lapply(unt.40a.2salt, db=age40a.data, agerec.aov)
lapply(res.agerec.40a.res, summary)
##indicates a signficant effect of both age and residence for both time points

#also test for interaction
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, as an interaction, then test for differences post-hoc
res.40a.agerecxres <- lapply(unt.40a.2salt, db=age40a.data, agerecxres.aov)
lapply(res.40a.agerecxres, summary)
##within men under 40, n.s. non-sig. interaction effect. Differences between salT of sedentees and all other residence groups.

#post-hoc testing of waking salT only
summary(glht(res.40a.agerecxres$MeanS1D1D2, linfct = mcp(residence19pub = "Tukey")))
##in post hoc testing, no differences in ageing profiles
#sample size: summary(model.frame(res.40a.agerecxres$MeanS1D1D2)$residence19pub)

#linear models 
summary(lm(MeanS1D1D2~BMI+AgeRecruit*residence19pub, data=age40a.data))
##within <40 men, no significant interaction effects, suggesting that a more simple model may apply when restricting analysis to men <40

#also applying transformed data
res.40a.agerecxres <- lapply(z.log.age40a.2salt, db=age40a.data, agerecxres.aov)
lapply(res.40a.agerecxres, summary)
##within <40 men, no significant interaction effects, suggesting that a more simple model may apply when restricting analysis to men <40

#linear model for values of differences in slopes in waking testosterone with interactions, using transformed data and bmi to match other regressions in the paper, reported values in fig. 3
resxagerec.40a.res <- lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=age40a.data)
resxagerec.40a.sres <- summary(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=age40a.data))
resxagerec.40a.n <- summary(model.frame(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=age40a.data))$residence19pub)
resxagerec.40a.ci <- confint(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=age40a.data))
##within <40 men, no significant interaction effects, suggesting that a more simple model may apply when restricting analysis to men <40
##see: res.2salt.40a.res for simplified model (Hypot. 1.1a)

#list of coefficients and paste to add to text
resxageres.coef <- as.list(resxagerec.40a.res$coefficients[c("z.log.age:residence19pubAdult migrants", "z.log.age:residence19pubChild migrants", "z.log.age:residence19pubSecond generation migrants", "z.log.age:residence19pubBritish European")])
resxagerec.40a.ci.ls <- as.list(resxagerec.40a.ci[c("z.log.age:residence19pubAdult migrants", "z.log.age:residence19pubChild migrants", "z.log.age:residence19pubSecond generation migrants", "z.log.age:residence19pubBritish European"),])
paste(sprintf("%.3f", resxageres.coef),", 95%CI=", paste(round(as.numeric(resxagerec.40a.ci.ls[1:4]),3), ", ", round(as.numeric(resxagerec.40a.ci.ls[5:8]),3), sep=""),sep="")

#Hypot 4.3.1a age at recruitment <40 is continuous predictor of salivary testosterone within residence groups--------
#Bengali Sedentees
#Wake salT
sed.s1d1d2.age.40a.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Bangladeshi sedentees"))
sed.s1d1d2.age.40a.sres <- summary(sed.s1d1d2.age.40a.res)
sed.s1d1d2.age.40a.ci <- confint(sed.s1d1d2.age.40a.res)
##n.s. age-related trend in salT (positive .85 pg/mL per year)
#Evening salT
sed.s3d1d2.age.40a.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Bangladeshi sedentees"))
sed.s3d1d2.age.40a.sres <- summary(sed.s3d1d2.age.40a.res)
sed.s3d1d2.age.40a.ci <- confint(sed.s3d1d2.age.40a.res)
##n.s. trend in bed salT positive 0.63 pg/ml per year

#Adult migrants
#AM salT
adu.s1d1d2.age.40a.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Adult migrants"))
adu.s1d1d2.age.40a.sres <- summary(adu.s1d1d2.age.40a.res)
adu.s1d1d2.age.40a.ci <- confint(adu.s1d1d2.age.40a.res)
##no sig. trend in wake salT (n.s. of 0.24 pg/mL per year)
#Evening salT
adu.s3d1d2.age.40a.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Adult migrants"))
adu.s3d1d2.age.40a.sres <- summary(adu.s3d1d2.age.40a.res)
adu.s3d1d2.age.40a.ci <- confint(adu.s3d1d2.age.40a.res)
##no sig. trend in wake salT (n.s. of -1.16 pg/mL per year)

#Child migrants 
#AM salT
chi.s1d1d2.age.40a.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Child migrants"))
chi.s1d1d2.age.40a.sres <- summary(chi.s1d1d2.age.40a.res)
chi.s1d1d2.age.40a.ci <- confint(chi.s1d1d2.age.40a.res)
##no sig. trend in wake salT (n.s. of -0.81 pg/mL per year)
#Evening salT
chi.s3d1d2.age.40a.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Child migrants"))
chi.s3d1d2.age.40a.sres <- summary(chi.s3d1d2.age.40a.res)
chi.s3d1d2.age.40a.ci <- confint(chi.s3d1d2.age.40a.res)
##sig. (p=0.0) negative trend in bed salT (-3.14 pg/mL per year)

#Second Generation Migrants 
#AM salT
sg.s1d1d2.age.40a.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Second generation migrants"))
sg.s1d1d2.age.40a.sres <- summary(sg.s1d1d2.age.40a.res)
sg.s1d1d2.age.40a.ci <- confint(sg.s1d1d2.age.40a.res)
##no sig. trend in wake salT (n.s. of -0.60 pg/mL per year)
#Evening salT
sg.s3d1d2.age.40a.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="Second generation migrants"))
sg.s3d1d2.age.40a.sres <- summary(sg.s3d1d2.age.40a.res)
sg.s3d1d2.age.40a.ci <- confint(sg.s3d1d2.age.40a.res)
##no sig. trend in evening salT (n.s. of -2.15 pg/mL per year)

#British European
#AM salT
eur.s1d1d2.age.40a.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="British European"))
eur.s1d1d2.age.40a.sres <- summary(eur.s1d1d2.age.40a.res)
eur.s1d1d2.age.40a.ci <- confint(eur.s1d1d2.age.40a.res)
##no sig. trend in wake salT (n.s. of -1.58 pg/mL per year)
eur.s3d1d2.age.40a.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(age40a.data, residence19pub=="British European"))
eur.s3d1d2.age.40a.sres <- summary(eur.s3d1d2.age.40a.res)
eur.s3d1d2.age.40a.ci <- confint(eur.s3d1d2.age.40a.res)
##no sig. trend in wake salT (n.s. of 1.80 pg/mL per year)

##4.3.1a conclusions: when confining to men <40, no evidence of a difference between residential groups in age related decline, except in CHI Bed sample. Suggests that age-related variations seen across groups is particular to the whole of the lifecourse, that differences in the slopes of decline are steepest when comparing men at younger or older ages, and not as steep during middle age.

#Hypot 4.4.1a restricting to <40y, are the slopes significantly different between groups for salT, for those showing a decline (UK resident men only)?------
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, set Europeans as reference group
age40a.ukres.data <- subset(age40a.data, residence19pub!="Bangladeshi sedentees")
age40a.ukres.data$residence19pub <- relevel(age40a.ukres.data$residence19pub, ref="British European")
age40a.ukres.data$residence19pub <- droplevels(age40a.ukres.data$residence19pub)
res.agerec.40a.ukres <- lapply(unt.ukres.40a.2salt, db=age40a.ukres.data, agerec.aov)
lapply(res.agerec.40a.ukres, summary)
##indicates a signficant effect of both age and residence for waking salT within UK Res only, effect of age in both Wake and Bed salT
##counts of sample size: summary(subset(age40a.data, residence19pub!="Bangladeshi sedentees")$residence19pub)

#also test for interaction
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, as an interaction, then test for differences post-hoc
res.agerecxukres <- lapply(unt.ukres.40a.2salt, db=age40a.ukres.data, agerecxres.aov)
lapply(res.agerecxukres, summary)
##No indiciation of a significant agexresidence interaction in <40 men, therefore the slopes aren't different between residence groups within the UK

#alternative linear model using transformed data and bmi to match other regressions in the paper
summary(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=age40a.ukres.data))
confint(lm(z.log.meanS1D1D2~z.bmi+z.log.age*residence19pub, data=age40a.ukres.data))
##no indictaion of interaction effect, refer to more simple model:
summary(lm(z.log.meanS1D1D2~z.bmi+z.log.age+residence19pub, data=age40a.ukres.data))
confint(lm(z.log.meanS1D1D2~z.bmi+z.log.age+residence19pub, data=age40a.ukres.data))
summary(glht(lm(z.log.meanS1D1D2~z.bmi+z.log.age+residence19pub, data=age40a.ukres.data), linfct = mcp(residence19pub = "Tukey")))
##when confining analysis to men <40 in the UK only, sig. higher waking salT of CHI and 2NG, compared to EUR and ADU

#alternative linear model using transformed data and imputed mssbmi to match other regressions in the paper
summary(lm(z.log.meanS1D1D2~z.log.age+z.mssbmi+residence19pub+z.log.age+residence19pub, data=age40a.ukres.data))
confint(lm(z.log.meanS1D1D2~z.log.age+z.mssbmi+residence19pub+z.log.age+residence19pub, data=age40a.ukres.data))
summary(glht(lm(z.log.meanS1D1D2~z.mssbmi+z.log.age+residence19pub, data=ukres19.data), linfct = mcp(residence19pub = "Tukey")))
##when confining analysis to men <40 in the UK only, with imputed mssbmi sig. higher waking salT of CHI and 2NG, compared to EUR and ADU

#Hypot 4.5.1a When confnining analysis to men <40 who did not show a sig relationship between age and salT (UK born men) is there a significant age related decline in salT, when not considering residence group?-------
#AM salT
res.age.40a.ukborn.s1 <- lm(MeanS1D1D2~AgeRecruit, data=subset(age40a.data, residence19pub=="British European"| residence19pub=="Second generation migrants"))
summary(res.age.40a.ukborn.s1)
confint(res.age.40a.ukborn.s1)
length(resid(res.age.40a.ukborn.s1))
#n.s. age realted decline of waking salT -2.09 (95%CI: -4.857065, 0.672462) within men born and raised in the UK

#Eve salT
res.age.40a.ukborn.s3 <- lm(MeanS3D1D2~AgeRecruit, data=subset(age40a.data, residence19pub=="British European"| residence19pub=="Second generation migrants"))
summary(res.age.40a.ukborn.s3)
confint(res.age.40a.ukborn.s3)
length(resid(res.age.40a.ukborn.s3))
#non-sig age realted decline of evening salT of -1.8 pg/ml per year (95%CI: -4.452067, 0.8494162) within men born and raised in the UK

#Hypot 4.5.2a <40 age at recruitment Does adjustment of waking salT to decline found in UK born groups eliminate differences between Bengali and European groups?----
#create an adjusted variable for waking salT adding 1.22pg/ml for each year over age 22 men were at recruitment
age40a.data$S1D1D2.uk.adj <- ifelse(age40a.data$AgeRecruit<=22, age40a.data$MeanS1D1D2, ((age40a.data$AgeRecruit-22)*1.22)+age40a.data$MeanS1D1D2)
#inspect to see if this adjusted value needs to be log transformed for normal distriboution
hist(subset(age40a.data, residence19pub=="British European"| residence19pub=="Second generation migrants")[,"S1D1D2.uk.adj"])
##normally distributed, include untransformed value

#in <40 age at recruit, linear model comparing UK-born groups with adjusted salT and z.bmi
summary(lm(S1D1D2.uk.adj~z.log.age+z.bmi+residence19pub, data=subset(age40a.data, residence19pub=="British European"| residence19pub=="Second generation migrants")))
confint(lm(S1D1D2.uk.adj~z.log.age+z.bmi+residence19pub, data=subset(age40a.data, residence19pub=="British European"| residence19pub=="Second generation migrants")))
##after adjusting correction factor, second generation waking salT not sig. diff. in men under age 40 born in UK


###PUBERTY HYPOTHESES-------
#Hypot 5.1.1a <40 age at recruitment, age at puberty determined by residence after adjustment for recall bias and demographic trends by age at recruitment----
res.pub40a.res <- lapply(z.age40a.pub, y=age40a.data$residence19pub, db=age40a.data, age.lm)
res.pub40a.sres <- lapply(res.pub40a.res, summary)
#sample sizes: summary(model.frame(res.pub40a.res$z.pub.compos)$y)
#in men <40 compared to sedentees, 2NG and EUR are both sig. earlier age of puberty in composite and all measures (besides voice for 2NG), ADU recall earlier age at voice breaking

#for post-hoc multiple comparison Tukey correction of all-pair multiple comparison
res.pub40a.res.ph <- summary(glht(res.pub40a.res$z.pub.compos, linfct=mcp(y="Tukey")))
## Composite value: EUR > SED, ADU, CHI; 2NG > SED
## in contrast to 5.1.1., difference from 2NG n.s. for ADU

#Hypot 5.2.2a, in <40 men, age at puberty determined by age at migration of child migrants (age 19 cutoff), after adjustment for recall bias and demographic trends by age at recruitment.------
amu.age.19pub40a.res <- lm(z.pub.compos~z.log.age, data=subset(age40a.data, residence19pub=="Child migrants"))
amu.age.19pub40a.sres <- summary(amu.age.19pub40a.res)
confint(amu.age.19pub40a.res)
#sample size: nrow(model.frame(amu.age.19pub.res$z.pub.compos))
#within child migrants (<19 years) only, non sig. (p=0.07) positive correlation between age at migration and composite age at puberty, sig. for age at shaving and pubic hair. 
#sample sizes: n=19

#Hypot 5.3.1, age at puberty determined by migration cohort 0-8 years child migrants and 9-19 years (age 19 cutoff), compared to <19 after adjustment for recall bias and demographic trends by age at recruitment.----
cat.19pub.res <- lapply(z.pub, y=repro.data$age.8.19.mig, db=repro.data, age.lm)
cat.19pub.sres <- lapply(cat.19pub.res, summary)
cat.19pub.ci <- lapply(cat.19pub.res, confint)
summary(model.frame(cat.19pub.res$z.pub.compos)$y)

cat.19pub.res.ph <- summary(glht(cat.19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
confint(glht(cat.19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
##migration cohort 0-8 years child migrants recall sig. earlier shaving, n.s. pubic hair (p=0.057), compared to 9-19, n.s. composite. Note that if not adjusting for age, migrants 0-9 recall earlier composite than 9-19 (not shown)

#Hypot 5.3.2, age at puberty determined by migration cohort pre-birth-8 years (Second generation and child migrants) and 9-19 years (age 19 cutoff), after adjustment for recall bias and demographic trends by age at recruitment.----
cat.b19pub.res <- lapply(z.pub, y=repro.data$age.b8.19.mig, db=repro.data, age.lm)
cat.b19pub.sres <- lapply(cat.b19pub.res, summary)
cat.b19pub.ci <- lapply(cat.b19pub.res, confint)
summary(model.frame(cat.b19pub.res$z.pub.compos)$y)

cat.b19pub.res.ph <- summary(glht(cat.b19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
confint(glht(cat.b19pub.res$z.pub.compos, linfct=mcp(y="Tukey")))

#Hypotheses 6a: Across all groups <40, without separation by ethnicity or developmental exposure to ecological conditions, adult salivary T is a predictor of recalled age at puberty-------

#Hypot 6.1.1a waking adult salT is a predictor of pub traits across all groups, after adjustment for recall bias and demographic trends by age at recruitment.-------
salt1.pub.40a.res <- lm(z.pub.compos~z.log.age+z.log.meanS1D1D2, data=age40a.data)	
salt1.pub.40a.sres <- summary(salt1.pub.40a.res)
length(resid(salt1.pub.40a.res))
confint(salt1.pub.40a.res)

#Hypot 6.1.2a evening adult salT is a predictor of pub traits across all groups, after adjustment for recall bias and demographic trends by age at recruitment.-----
salt3.pub.40a.res <- lm(z.pub.compos~z.log.age+z.log.meanS3D1D2, data=age40a.data)	
salt3.pub.40a.sres <- summary(salt3.pub.40a.res)
length(resid(salt3.pub.40a.res))
confint(salt3.pub.40a.res)


###HEIGHT HYPOTHESES-------
#Hypotheses 9: Childhood age at migration is a predictor of adult height------------

#Hypot 9.1.1 within CHI <40  only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 19years, no covariates-----------
height.agemig.chi.40a.res <- lm(z.height~z.AgeMigUK, data=subset(age40a.data, residence19pub=="Child migrants"))
height.agemig.chi.40a.sres <- summary(height.agemig.chi.40a.res)
confint(height.agemig.chi.40a.res)
nrow(model.frame(height.agemig.chi.40a.res))
##for Age Mig within CHI only, height is not sig. predicted by age at migration.

#Hypot 9.1.2 within CHI <40 only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 19years, age at recruitment as covariate-----------
height.agemig.agerec.chi.40a.res <- lm(z.height~z.log.age+z.AgeMigUK, data=subset(age40a.data, residence19pub=="Child migrants"))
height.agemig.agerec.chi.40a.sres <- summary(height.agemig.agerec.chi.40a.res)
#when age at recruitment and age at migration are both included, neither shows a significant age at migration effect. 

#Hypotheses 10: Adult age at migration is a predictor of adult height------------

#Hypot 10.1.1 within ADU only, age at migration "z.AgeMigUK" as continuous predictor for <40 year old migrants who arrived after 19years, no covariates-----------
height.agemig.adu.40a.res <- lm(z.height~z.AgeMigUK, data=subset(age40a.data, residence19pub=="Adult migrants"))
height.agemig.adu.40a.sres <- summary(height.agemig.adu.40a.res)
confint(height.agemig.adu.40a.res)
##for Age Mig within ADU only, height is n.s. predicted by age at migration.

#Hypot 10.1.2 within ADU only, age at migration "z.AgeMigUK" as continuous predictor for ,40 year old migrants who arrived after 19years, age at recruitment as covariate-----------
height.agemig.agerec.adu.40a.res <- lm(z.height~z.log.age+z.AgeMigUK, data=subset(age40a.data, residence19pub=="Adult migrants"))
height.agemig.agerec.adu.40a.sres <- summary(height.agemig.agerec.adu.40a.res)
#when age at recruitment and age at migration are both included, neither shows a significant age at migration effect.

#Hypot 10.1.3 within ADU only, age at recruitment as continuous predictor for migrants who arrived after 19years no covariate-----------
height.agerec.adu.40a.res <- lm(z.height~z.log.age, data=subset(age40a.data, residence19pub=="Adult migrants"))
height.agerec.adu.40a.sres <- summary(height.agemig.adu.40a.res)
confint(height.agerec.adu.40a.res)
#age at recruitment alone n.s. on height.


#SUPPLEMENTAL ANALYSES OF intra-group analysis of Child migrants with imputed BMI from package "mi"--------
#Hypot 2.1.2.i within CHI only, age at migration "z.AgeMigUK" as continuous predictor who arrived before 19years "y19" regressions, with imputed BMI from package "mi"-------------
#waking pooled
y19a.s1salt.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS1D1D2~z.log.age+z.bmi.ci+z.AgeMigUK))
y19a.s1salt.mssbmi.ci.sres <- summary(MIcombine(y19a.s1salt.mssbmi.ci.res))

#evening pooled
y19a.s3salt.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS3D1D2~z.log.age+z.bmi.ci+z.AgeMigUK))
y19a.s3salt.mssbmi.ci.sres <- summary(MIcombine(y19a.s3salt.mssbmi.ci.res))

#chain 1
y19a.2salt.mssbmi.ci1.res <- lapply(z.log.19.2salt.i, y=ci1.mig.imp.data.chi$z.AgeMigUK, db=ci1.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19a.2salt.mssbmi.ci1.sres <- lapply(y19a.2salt.mssbmi.ci1.res, summary)
##Age Mig n.s. Waking (p=0.9) or Bed (p=0.1) sample within CHI only. age sig. neg covariate for Wake sample

#chain 2
y19a.2salt.mssbmi.ci2.res <- lapply(z.log.19.2salt.i, y=ci2.mig.imp.data.chi$z.AgeMigUK, db=ci2.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19a.2salt.mssbmi.ci2.sres <- lapply(y19a.2salt.mssbmi.ci2.res, summary)
##Age Mig n.s. Waking (p=0.89) or Bed (p=0.084) sample within CHI only. age sig. neg covariate for Wake sample

#chain 3
y19a.2salt.mssbmi.ci3.res <- lapply(z.log.19.2salt.i, y=ci3.mig.imp.data.chi$z.AgeMigUK, db=ci3.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19a.2salt.mssbmi.ci3.sres <- lapply(y19a.2salt.mssbmi.ci3.res, summary)
##Age Mig n.s. Waking (p=0.86) or Bed (p=0.076) sample within CHI only. age sig. neg covariate for Wake sample

#chain 4
y19a.2salt.mssbmi.ci4.res <- lapply(z.log.19.2salt.i, y=ci4.mig.imp.data.chi$z.AgeMigUK, db=ci4.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19a.2salt.mssbmi.ci4.sres <- lapply(y19a.2salt.mssbmi.ci4.res, summary)
##Age Mig n.s. Waking (p=0.80) or Bed (p=0.068) sample within CHI only. age sig. neg covariate for Wake sample
#Using imputed values, Age Mig is non-sig. predictor of salivary testosterone at waking or evening for any of four chains, Using imputed values from "mi" package does not alter the results (results match setting BMI to population avg mss.bmi)

#Hypot 2.2.2i age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with BMI imputed from package "mi"--------
#waking pooled
nyu.s1salt.amu.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS1D1D2~NumYearsUK+z.bmi.ci+z.AgeMigUK))
nyu.s1salt.amu.mssbmi.ci.sres <- summary(MIcombine(nyu.s1salt.amu.mssbmi.ci.res))

#evening pooled
nyu.s1salt.amu.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS3D1D2~NumYearsUK+z.bmi.ci+z.AgeMigUK))
nyu.s1salt.amu.mssbmi.ci.sres <- summary(MIcombine(nyu.s1salt.amu.mssbmi.ci.res))

#chain 1
nyu.2salt.amu.mssbmi.ci1.res <- lapply(z.log.19.2salt, y=ci1.mig.imp.data.chi$z.AgeMigUK, db=ci1.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
nyu.2salt.amu.mssbmi.ci1.sres <- lapply(nyu.2salt.amu.mssbmi.ci1.res, summary)
##for Age Mig within Child migs only, number of years in the UK and imputed bmi as covariates, waking (p=0.11) n.s. but evening time point (p=0.0036) sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK

#chain 2
nyu.2salt.amu.mssbmi.ci2.res <- lapply(z.log.19.2salt, y=ci2.mig.imp.data.chi$z.AgeMigUK, db=ci2.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
nyu.2salt.amu.mssbmi.ci2.sres <- lapply(nyu.2salt.amu.mssbmi.ci2.res, summary)
##for Age Mig within Child migs only, number of years in the UK and imputed bmi as covariates, waking (p=0.09) and evening time point (p=0.0033) sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK

#chain 3
nyu.2salt.amu.mssbmi.ci3.res <- lapply(z.log.19.2salt, y=ci3.mig.imp.data.chi$z.AgeMigUK, db=ci3.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
nyu.2salt.amu.mssbmi.ci3.sres <- lapply(nyu.2salt.amu.mssbmi.ci3.res, summary)
##for Age Mig within Child migs only, number of years in the UK and imputed bmi as covariates, waking (p=0.068) n.s. but evening time point (p=0.0027) sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK

#chain 4
nyu.2salt.amu.mssbmi.ci4.res <-lapply(z.log.19.2salt, y=ci4.mig.imp.data.chi$z.AgeMigUK, db=ci4.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
nyu.2salt.amu.mssbmi.ci4.sres <- lapply(nyu.2salt.amu.mssbmi.ci4.res, summary)
##for Age Mig within Child migs only, number of years in the UK and imputed bmi as covariates, waking (p=0.065) n.s. but evening time point (p=0.00254) sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK
##Overall, using imputed values from "mi" package does not alter the results (results match setting BMI to population avg mss.bmi), with waking a marginally sig. finding while bed is clearly sig.

#Hypot 2.3.2i SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression with age at recruitment and with imputed BMI from package "mi"-------
#waking pooled
y19b.age.s1salt.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS1D1D2~z.log.age+z.bmi.ci+age.8.19.mig))
y19b.age.s1salt.mssbmi.ci.sres <- summary(MIcombine(y19b.age.s1salt.mssbmi.ci.res))

#evening pooled
y19b.age.s3salt.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS3D1D2~z.log.age+z.bmi.ci+age.8.19.mig))
y19b.age.s3salt.mssbmi.ci.sres <- summary(MIcombine(y19b.age.s3salt.mssbmi.ci.res))

#chain 1
y19b.age.2salt.mssbmi.ci1.res <- lapply(z.log.19.2salt, y=ci1.mig.imp.data.chi$age.8.19.mig, db=ci1.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19b.age.2salt.mssbmi.ci1.sres <- lapply(y19b.age.2salt.mssbmi.ci1.res, summary)
##Age recruit neg. sig. relationship for both sample times. Migration cohort n.s. for both sample times

#chain 2
y19b.age.2salt.mssbmi.ci2.res <- lapply(z.log.19.2salt, y=ci2.mig.imp.data.chi$age.8.19.mig, db=ci2.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19b.age.2salt.mssbmi.ci2.sres <- lapply(y19b.age.2salt.mssbmi.ci2.res, summary)
##for Wake age recruit neg. sig. relationship. For Bed, n.s. (p=0.0501) Migration cohort n.s. for both sample times

#chain 3
y19b.age.2salt.mssbmi.ci3.res <- lapply(z.log.19.2salt, y=ci3.mig.imp.data.chi$age.8.19.mig, db=ci3.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19b.age.2salt.mssbmi.ci3.sres <- lapply(y19b.age.2salt.mssbmi.ci3.res, summary)
##Age recruit neg. sig. relationship for both sample times. Migration cohort n.s. for both sample times

#chain 4
y19b.age.2salt.mssbmi.ci4.res <- lapply(z.log.19.2salt, y=ci4.mig.imp.data.chi$age.8.19.mig, db=ci4.mig.imp.data.chi, function(x, y, db){
  lm(x~z.log.age+z.bmi.ci+y, data=db)	
})
y19b.age.2salt.mssbmi.ci4.sres <- lapply(y19b.age.2salt.mssbmi.ci4.res, summary)
##Age recruit neg. sig. relationship for waking, evening n.s. (p=0.06) sample times. Migration cohort n.s. for both sample times
#Using imputed values, when including age at recruitment into the models, migration before or after age 9 is not a sig. predictor of salivary testosterone at any time point, for any of four chains. Age at recrutiment sig neg. predictor of Waking, marginally of Bed salT (results match setting BMI to population avg mss.bmi)

#Hypot 2.4.2i SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with imputed BMI from package "mi"-----
#waking pooled
y19c.nyu.s1salt.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS1D1D2~NumYearsUK+z.bmi.ci+age.8.19.mig))
y19c.nyu.s1salt.mssbmi.ci.sres <- summary(MIcombine(y19c.nyu.s1salt.mssbmi.ci.res))

#evening pooled
y19c.nyu.s3salt.mssbmi.ci.res <- with(mig.19pub.data.i, lm(z.log.meanS3D1D2~NumYearsUK+z.bmi.ci+age.8.19.mig))
y19c.nyu.s3salt.mssbmi.ci.sres <- summary(MIcombine(y19c.nyu.s3salt.mssbmi.ci.res))

#chain 1
y19c.nyu.2salt.mssbmi.ci1.res <- lapply(z.log.19.2salt, y=ci1.mig.imp.data.chi$age.8.19.mig, db=ci1.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
y19c.nyu.2salt.mssbmi.ci1.sres <- lapply(y19c.nyu.2salt.mssbmi.ci1.res, summary)
##for Wake (p=0.094) n.s. and Bed sig. (p=0.013), Birth to age 9 > 9-19y migs, Num Years neg. sig. relationship. for wake only.

#chain 2
y19c.nyu.2salt.mssbmi.ci2.res <- lapply(z.log.19.2salt, y=ci2.mig.imp.data.chi$age.8.19.mig, db=ci2.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
y19c.nyu.2salt.mssbmi.ci2.sres <- lapply(y19c.nyu.2salt.mssbmi.ci2.res, summary)
##for Wake (p=0.040) n.s. and Bed sig. (p=0.01), Birth to age 9 > 9-19y migs, Num Years neg. sig. relationship. for wake only.

#chain 3
y19c.nyu.2salt.mssbmi.ci3.res <- lapply(z.log.19.2salt, y=ci3.mig.imp.data.chi$age.8.19.mig, db=ci3.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
y19c.nyu.2salt.mssbmi.ci3.sres <- lapply(y19c.nyu.2salt.mssbmi.ci3.res, summary)
##for Wake (p=0.06) n.s. and Bed sig. (p=0.009), Birth to age 9 > 9-19y migs, Num Years neg. sig. relationship. for wake only.

#chain 4
y19c.nyu.2salt.mssbmi.ci4.res <- lapply(z.log.19.2salt, y=ci4.mig.imp.data.chi$age.8.19.mig, db=ci4.mig.imp.data.chi, function(x, y, db){
  lm(x~NumYearsUK+z.bmi.ci+y, data=db)	
})
y19c.nyu.2salt.mssbmi.ci4.sres <- lapply(y19c.nyu.2salt.mssbmi.ci4.res, summary)
##for Wake (p=0.057) n.s. and Bed sig. (p=0.008), Birth to age 9 > 9-19y migs, Num Years neg. sig. relationship. for wake only.
##Overall, using imputed values from "mi" package does not alter the results (results match setting BMI to population avg mss.bmi), waking marginal p=0.07-0.04, bed sig. p=0.01-0.008


#TABLES FOR SUPPLEMENTAL ANALYSIS SECTION 2-----

#Descriptives: residence groups <40 years at recruitment--------
#mean, sd, 95%ci
n.40a.tot<-tapply(X=age40a.data$PartNum, INDEX=age40a.data$residence19pub, FUN=length)
m.40a.age<-round(tapply(X=age40a.data$AgeRecruit, INDEX=age40a.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.40a.age<-round(tapply(X=age40a.data$AgeRecruit, INDEX=age40a.data$residence19pub, FUN=sd, na.rm=TRUE),1)
n.40a.age<-round(tapply(X=age40a.data$AgeRecruit, INDEX=age40a.data$residence19pub, FUN=length),1)
ci.40a.age <- aggregate(age40a.data[,"AgeRecruit"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.height<-round(tapply(X=age40a.data$Height, INDEX=age40a.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.40a.height<-round(tapply(X=age40a.data$Height, INDEX=age40a.data$residence19pub, FUN=sd, na.rm=TRUE),1)
n.40a.height<-round(tapply(X=age40a.data$Height, INDEX=age40a.data$residence19pub, FUN=length),1)
ci.40a.height <- aggregate(age40a.data[,"Height"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.weight<-round(tapply(X=age40a.data$Weight, INDEX=age40a.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.40a.weight<-round(tapply(X=age40a.data$Weight, INDEX=age40a.data$residence19pub, FUN=sd, na.rm=TRUE),1)
n.40a.weight<-round(tapply(X=age40a.data$Weight, INDEX=age40a.data$residence19pub, FUN=length),1)
ci.40a.weight <- aggregate(age40a.data[,"Weight"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.bmi<-round(tapply(X=age40a.data$BMI, INDEX=age40a.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.40a.bmi<-round(tapply(X=age40a.data$BMI, INDEX=age40a.data$residence19pub, FUN=sd, na.rm=TRUE),1)
ci.40a.bmi <- aggregate(age40a.data[,"BMI"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.s1d1d2<-round(tapply(X=age40a.data$MeanS1D1D2, INDEX=age40a.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.40a.s1d1d2<-round(tapply(X=age40a.data$MeanS1D1D2, INDEX=age40a.data$residence19pub, FUN=sd, na.rm=TRUE),1)
ci.40a.s1d1d2 <- aggregate(age40a.data[,"MeanS1D1D2"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.s3d1d2<-round(tapply(X=age40a.data$MeanS3D1D2, INDEX=age40a.data$residence19pub, FUN=mean, na.rm=TRUE),1)
sd.40a.s3d1d2<-round(tapply(X=age40a.data$MeanS3D1D2, INDEX=age40a.data$residence19pub, FUN=sd, na.rm=TRUE),1)
ci.40a.s3d1d2 <- aggregate(age40a.data[,"MeanS3D1D2"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.pub.voice<-round(tapply(X=age40a.data$PubVoice.n, INDEX=age40a.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.40a.pub.voice<-round(tapply(X=age40a.data$PubVoice.n, INDEX=age40a.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.40a.pub.voice <- aggregate(age40a.data[,"PubVoice.n"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.pub.shave<-round(tapply(X=age40a.data$PubShave.n, INDEX=age40a.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.40a.pub.shave<-round(tapply(X=age40a.data$PubShave.n, INDEX=age40a.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.40a.pub.shave <- aggregate(age40a.data[,"PubShave.n"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.pub.pub<-round(tapply(X=age40a.data$PubPub.n, INDEX=age40a.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.40a.pub.pub<-round(tapply(X=age40a.data$PubPub.n, INDEX=age40a.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.40a.pub.pub <- aggregate(age40a.data[,"PubPub.n"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.pub.ne<-round(tapply(X=age40a.data$PubNE.n, INDEX=age40a.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.40a.pub.ne<-round(tapply(X=age40a.data$PubNE.n, INDEX=age40a.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.40a.pub.ne <- aggregate(age40a.data[,"PubNE.n"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])
m.40a.pub.compos<-round(tapply(X=age40a.data$pub.compos, INDEX=age40a.data$residence19pub,FUN=mean, na.rm=TRUE),1)
sd.40a.pub.compos<-round(tapply(X=age40a.data$pub.compos, INDEX=age40a.data$residence19pub,FUN=sd, na.rm=TRUE),1)
ci.40a.pub.compos <- aggregate(age40a.data[,"pub.compos"], list(age40a.data[,"residence19pub"]), FUN = function(x) t.test(x)$conf.int[1:2])

#all group totals/means
n.40a.all <- nrow(age40a.data)
m.40a.age.all <- round(mean(age40a.data$AgeRecruit, na.rm=TRUE),1)
m.40a.height.all <- round(mean(age40a.data$Height, na.rm=TRUE),1)
m.40a.weight.all <- round(mean(age40a.data$Weight, na.rm=TRUE),1)
m.40a.bmi.all <- round(mean(age40a.data$BMI, na.rm=TRUE),1)
m.40a.s1d1d2.all <- round(mean(age40a.data$MeanS1D1D2, na.rm=TRUE),1)
m.40a.s3d1d2.all <- round(mean(age40a.data$MeanS3D1D2, na.rm=TRUE),1)
m.40a.pub.compos.all <- round(mean(age40a.data$pub.compos, na.rm=TRUE),1)

#all group sds
sd.40a.age.all <- round(sd(age40a.data$AgeRecruit, na.rm=TRUE),2)
sd.40a.height.all <- round(sd(age40a.data$Height, na.rm=TRUE),2)
sd.40a.weight.all <- round(sd(age40a.data$Weight, na.rm=TRUE),2)
sd.40a.bmi.all <- round(sd(age40a.data$BMI, na.rm=TRUE),2)
sd.40a.s1d1d2.all <- round(sd(age40a.data$MeanS1D1D2, na.rm=TRUE),2)
sd.40a.s3d1d2.all <- round(sd(age40a.data$MeanS3D1D2, na.rm=TRUE),2)
sd.40a.pub.compos.all <- round(sd(age40a.data$pub.compos, na.rm=TRUE),2)

m.40a.all <- c(m.40a.age.all, m.40a.height.all, m.40a.weight.all, m.40a.bmi.all, m.40a.s1d1d2.all, m.40a.s3d1d2.all, m.40a.pub.compos.all)
sd.40a.all <- c(sd.40a.age.all, sd.40a.height.all, sd.40a.weight.all, sd.40a.bmi.all, sd.40a.s1d1d2.all, sd.40a.s3d1d2.all, sd.40a.pub.compos.all)

#function for pasting mean and SDs together
mean.40a.sd.40a.f <- function(m.40a.var, sd.40a.var){
  paste(sprintf("%.1f", m.40a.var)," (",round(sd.40a.var,1),")",sep="") 
}
msd.40a.all <- c(n.40a.all, mean.40a.sd.40a.f(m.40a.all, sd.40a.all))

#mean and standard deviation
msd.40a.age <- paste(sprintf("%.1f", m.40a.age)," (",round(sd.40a.age,2),")",sep="") 
msd.40a.height <- paste(sprintf("%.1f", m.40a.height)," (",round(sd.40a.height,2),")",sep="") 
msd.40a.weight <- paste(sprintf("%.1f", m.40a.weight)," (",round(sd.40a.weight,2),")",sep="")
msd.40a.bmi <- paste(sprintf("%.1f", m.40a.bmi)," (",round(sd.40a.bmi,2),")",sep="")
msd.40a.s1d1d2 <- paste(sprintf("%.1f", m.40a.s1d1d2)," (",round(sd.40a.s1d1d2,2),")",sep="")
msd.40a.s3d1d2 <- paste(sprintf("%.1f", m.40a.s3d1d2)," (",round(sd.40a.s3d1d2,2),")",sep="")
msd.40a.pub.voice <- paste(sprintf("%.1f", m.40a.pub.voice)," (",round(sd.40a.pub.voice,2),")",sep="")
msd.40a.pub.shave <- paste(sprintf("%.1f", m.40a.pub.shave)," (",round(sd.40a.pub.shave,2),")",sep="")
msd.40a.pub.pub <- paste(sprintf("%.1f", m.40a.pub.pub)," (",round(sd.40a.pub.pub,2),")",sep="")
msd.40a.pub.ne <- paste(sprintf("%.1f", m.40a.pub.ne)," (",round(sd.40a.pub.ne,2),")",sep="")
msd.40a.pub.compos <- paste(sprintf("%.1f", m.40a.pub.compos)," (",round(sd.40a.pub.compos,2),")",sep="")


#mean and ci (for text)
#a function for pasting mean and ci together
mean.40a.ci <- function(m.40a.var, ci.40a.var){
  paste(sprintf("%.1f", m.40a.var),", 95%CI=", paste(round(as.numeric(ci.40a.var$x)[1:5],1), ", ", round(as.numeric(ci.40a.var$x)[6:10],1), sep=""),sep="")
}
m.40a.var.ls <- c("m.40a.age", "m.40a.height", "m.40a.weight", "m.40a.bmi", "m.40a.s1d1d2", "m.40a.s2d1d2", "m.40a.s3d1d2", "m.40a.pub.voice", "m.40a.pub.shave", "m.40a.pub.pub", "m.40a.pub.ne", "m.40a.pub.compos")
ci.40a.var.ls <- c("ci.40a.age", "ci.40a.height", "ci.40a.weight", "ci.40a.bmi", "ci.40a.s1d1d2", "ci.40a.s2d1d2", "ci.40a.s3d1d2", "ci.40a.pub.voice", "ci.40a.pub.shave", "ci.40a.pub.pub", "ci.40a.pub.ne", "ci.40a.pub.compos")
cbind(noquote(m.40a.var.ls[1]), noquote(ci.40a.var.ls))
mean.40a.ci(m.40a.age, ci.40a.age)
mean.40a.ci(m.40a.s1d1d2, ci.40a.s1d1d2)
mean.40a.ci(m.40a.pub.compos, ci.40a.pub.compos)
mean.40a.ci(m.40a.s1d1d2, ci.40a.s1d1d2)
mean.40a.ci(m.40a.s3d1d2, ci.40a.s3d1d2)

#combine into table using "cbind"
mig40a.tab.data<-cbind(n.40a.tot, m.40a.age, sd.40a.age, m.40a.height, sd.40a.height, m.40a.weight, sd.40a.weight, m.40a.bmi, sd.40a.bmi, m.40a.s1d1d2, sd.40a.s1d1d2, m.40a.s3d1d2, sd.40a.s3d1d2, m.40a.pub.compos, sd.40a.pub.compos)

#convert to data frame and rename variables
mig40a.tab.data<-as.data.frame(x=mig40a.tab.data)

#for a reduced table of mean (sd), including composite age of puberty only
rn <- names(m.40a.age)  #rownames
cn <- c(c("N", "Age, y", "Height, cm", "Weight, kg", "BMI", "Waking, pg/mL", "Evening, pg/mL", "Recalled age at puberty, y")) #column names
mig40a.tab.msd <- matrix(c(n.40a.tot, msd.40a.age, msd.40a.height, msd.40a.weight, msd.40a.bmi, msd.40a.s1d1d2, msd.40a.s3d1d2, msd.40a.pub.compos),5,8,dimnames=list(rn,cn))
#reorder and rename of residence groups
res.order <- c("Bangladeshi sedentees", "Adult migrants", "Child migrants", "Second generation migrants", "British European")
mig40a.tab.msd <- mig40a.tab.msd[match(res.order, rn),]
mig40a.tab.msd <- rbind(mig40a.tab.msd, msd.40a.all) 
rownames(mig40a.tab.msd) <- c("Bangladeshi sedentees", "Adult migrants", "Child migrants", "Second generation migrants", "British European", "All groups")

bhai40a.desc.tab.ht <- stargazer(mig40a.tab.msd, digits=1, out="bhai40a.desc.tab.html", summary=FALSE, type="html")


#Table 1.1a SalT by residence: lapply both sampling variables and get summary of <40y
res.2salt.40a.tab.ht <- stargazer(res.2salt.40a.res, 
                                  title="Salivary testosterone by residence group, <40y age at recruitment", 
                                  dep.var.caption  = "<em> sample time </em>", 
                                  out="res.2salt.40a.tab.html", 
                                  column.labels=c("Waking", "Evening"), 
                                  covariate.labels=c("Constant", "Age(log)", "BMI", "Adult migrants", "Child migrants", "Second generation migrants", "British European"), 
                                  notes ="all values are z-transformed SD units, age and testosterone also log transformed. Reference category: Bangladeshi sedentees", 
                                  type="html", 
                                  dep.var.labels="", 
                                  star.cutoffs = c(0.05, 0.01, 0.001), 
                                  report=('vc*ps'), 
                                  ci= T, 
                                  intercept.bottom = FALSE, 
                                  single.row = TRUE, 
                                  model.numbers=F
)
## for Wake and Bed samples YOM and 2NG salT is higher compared to SED, BMI also sig. covariate

#for n by residence group
summary(model.frame(res.2salt.40a.res$z.log.meanS1D1D2)$y)
summary(model.frame(res.2salt.40a.res$z.log.meanS3D1D2)$y)

#Table 1.2a Puberty by residence group:
res.pub.40a.tab.ht <- stargazer(res.pub40a.res$z.pub.compos, 
                                title="Recalled age at puberty by residence group, <40y age at recruitment", 
                                out="res.pub.40a.tab.html",
                                column.labels=c("Composite age at puberty"), 
                                covariate.labels=c("Constant", "Age(log)", "Adult Migrants", "Child Migrants", "Second Generation Migrants", "British-born European"), 
                                notes ="all values are z-transformed SD units, age also log transformed. Reference category: Bangladeshi Sedentees", 
                                type="html", 
                                dep.var.caption  = "<em> measure </em>", dep.var.labels="", 
                                star.cutoffs = c(0.05, 0.01, 0.001), 
                                report=('vc*ps'),
                                ci= T, 
                                intercept.bottom = FALSE, 
                                model.numbers=F,  
                                single.row = TRUE
)
#compared to sedentees, 2NG and EUR are both sig. earlier age of puberty in composite and all measures (besides voice for 2NG)

#Table S1a. Post-hoc table <40y only
# Table for export results of multiple comparison (post hoc Tukey)
# Source: Modified from https://gist.github.com/cheuerde/3acc1879dc397a1adfb0 
# x is a ghlt object

table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), 
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)
  
}

#save Wake and Bed contrasts as a new object, then save an html version of new contrasts for both time points (rows 5:10) with stargazer
res.salt1.40a.res.ph.tab <- table_glht(res.salt1.40a.res.ph)
res.salt1.40a.res.ph.tab[5:10,]

res.salt3.40a.res.ph.tab <- table_glht(res.salt3.40a.res.ph)
res.salt3.40a.res.ph.tab[5:10,]

res.salt.40a.ph.tab <- cbind(res.salt1.40a.res.ph.tab[5:10,], res.salt3.40a.res.ph.tab[5:10,])

res.salt.40a.ph.tab.ht <- stargazer(res.salt.40a.ph.tab, digits=2, out="bhai.ph.40a.tab.html", summary=FALSE, type="html")


nyu.amu.2salt.40a.res

#tables of summary results of multiple imputations-----
#Table Hypot 2.1.2.i within CHI only, age at migration "z.AgeMigUK" as continuous predictor who arrived before 19years "y19" regressions, with imputed BMI from package "mi"-------------
#Waking
#lowest and highest p values calcuated by four chains
#minimum
y19a.s1salt.mssbmi.ci.pmin <- apply(as.data.frame(cbind(y19a.2salt.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], y19a.2salt.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], y19a.2salt.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], y19a.2salt.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=min)
#maximum
y19a.s1salt.mssbmi.ci.pmax <- apply(as.data.frame(cbind(y19a.2salt.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], y19a.2salt.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], y19a.2salt.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], y19a.2salt.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
y19a.s1salt.mssbmi.tab.f <- as.data.frame(cbind(y19a.s1salt.mssbmi.ci.sres[,c(1,3:5)], y19a.s1salt.mssbmi.ci.pmin, y19a.s1salt.mssbmi.ci.pmax, y19a.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], confint(y19a.2salt.mssbmi.res$z.log.meanS1D1D2), y19a.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], y19a.2salt.sres$z.log.meanS1D1D2$coefficients[,1], confint(y19a.2salt.res$z.log.meanS1D1D2), y19a.2salt.sres$z.log.meanS1D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
y19a.s1salt.mssbmi.ci <- apply(round(y19a.s1salt.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
y19a.s1salt.mssbmi.ci.prg <- apply(round(y19a.s1salt.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
y19a.s1salt.mssbmi.95ci <- apply(round(y19a.s1salt.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
y19a.s1salt.95ci <- apply(round(y19a.s1salt.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

y19a.s1salt.mssbmi.tab <- as.data.frame(
  cbind(paste(round(y19a.s1salt.mssbmi.ci.sres[,1], 3), paste0("(", y19a.s1salt.mssbmi.ci,")")), 
        y19a.s1salt.mssbmi.ci.prg, 
        paste(round(y19a.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", y19a.s1salt.mssbmi.95ci,")")),
        round(y19a.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], 2),
        paste(round(y19a.2salt.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", y19a.s1salt.95ci,")")),
        round(y19a.2salt.sres$z.log.meanS1D1D2$coefficients[,4],2)))

#rename variables
names(y19a.s1salt.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
y19a.s1salt.mssbmi.tab.ht <- stargazer(y19a.s1salt.mssbmi.tab, digits=2, out="y19a.s1salt.mssbmi.tab.html", summary=FALSE, type="html")

#Evening
#lowest and highest p values calcuated by four chains
#minimum
y19a.s3salt.mssbmi.ci.pmin <- apply(as.data.frame(cbind(y19a.2salt.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], y19a.2salt.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], y19a.2salt.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], y19a.2salt.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=min)
#maximum
y19a.s3salt.mssbmi.ci.pmax <- apply(as.data.frame(cbind(y19a.2salt.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], y19a.2salt.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], y19a.2salt.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], y19a.2salt.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
y19a.s3salt.mssbmi.tab.f <- as.data.frame(cbind(y19a.s3salt.mssbmi.ci.sres[,c(1,3:5)], y19a.s3salt.mssbmi.ci.pmin, y19a.s3salt.mssbmi.ci.pmax, y19a.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], confint(y19a.2salt.mssbmi.res$z.log.meanS3D1D2), y19a.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], y19a.2salt.sres$z.log.meanS3D1D2$coefficients[,1], confint(y19a.2salt.res$z.log.meanS3D1D2), y19a.2salt.sres$z.log.meanS3D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
y19a.s3salt.mssbmi.ci <- apply(round(y19a.s3salt.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
y19a.s3salt.mssbmi.ci.prg <- apply(round(y19a.s3salt.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
y19a.s3salt.mssbmi.95ci <- apply(round(y19a.s3salt.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
y19a.s3salt.95ci <- apply(round(y19a.s3salt.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

y19a.s3salt.mssbmi.tab <- as.data.frame(
  cbind(paste(round(y19a.s3salt.mssbmi.ci.sres[,1], 3), paste0("(", y19a.s3salt.mssbmi.ci,")")), 
        y19a.s3salt.mssbmi.ci.prg, 
        paste(round(y19a.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", y19a.s3salt.mssbmi.95ci,")")),
        round(y19a.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], 2),
        paste(round(y19a.2salt.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", y19a.s3salt.95ci,")")),
        round(y19a.2salt.sres$z.log.meanS3D1D2$coefficients[,4],2)))

#rename variables
names(y19a.s3salt.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
y19a.s3salt.mssbmi.tab.ht <- stargazer(y19a.s3salt.mssbmi.tab, digits=2, out="y19a.s3salt.mssbmi.tab.html", summary=FALSE, type="html")

#Table Hypot 2.2.2i age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with BMI imputed from package "mi"--------
#Waking
#lowest and highest p values calcuated by four chains
#minimum
nyu.s1salt.amu.mssbmi.ci.pmin <- apply(as.data.frame(cbind(nyu.2salt.amu.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=min)
#maximum
nyu.s1salt.amu.mssbmi.ci.pmax <- apply(as.data.frame(cbind(nyu.2salt.amu.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
nyu.s1salt.amu.mssbmi.tab.f <- as.data.frame(cbind(nyu.s1salt.amu.mssbmi.ci.sres[,c(1,3:5)], nyu.2salt.amu.mssbmi.ci.pmin, nyu.2salt.amu.mssbmi.ci.pmax, nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], confint(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS1D1D2), nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], nyu.yom.amu.2salt.sres$z.log.meanS1D1D2$coefficients[,1], confint(nyu.yom.amu.2salt.res$z.log.meanS1D1D2), nyu.yom.amu.2salt.sres$z.log.meanS1D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
nyu.s1salt.amu.mssbmi.ci <- apply(round(nyu.s1salt.amu.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
nyu.s1salt.amu.mssbmi.ci.prg <- apply(round(nyu.s1salt.amu.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
nyu.s1salt.amu.mssbmi.95ci <- apply(round(nyu.s1salt.amu.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
nyu.s1salt.amu.95ci <- apply(round(nyu.s1salt.amu.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

nyu.s1salt.amu.mssbmi.tab <- as.data.frame(
  cbind(paste(round(nyu.s1salt.amu.mssbmi.ci.sres[,1], 3), paste0("(", nyu.s1salt.amu.mssbmi.ci,")")), 
        nyu.s1salt.amu.mssbmi.ci.prg, 
        paste(round(nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", nyu.s1salt.amu.mssbmi.95ci,")")),
        round(nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], 2),
        paste(round(nyu.yom.amu.2salt.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", nyu.s1salt.amu.95ci,")")),
        round(nyu.yom.amu.2salt.sres$z.log.meanS1D1D2$coefficients[,4],2)))

#rename variables
names(nyu.s1salt.amu.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
nyu.s1salt.amu.mssbmi.tab.ht <- stargazer(nyu.s1salt.amu.mssbmi.tab, digits=2, out="nyu.s1salt.amu.mssbmi.tab.html", summary=FALSE, type="html")

#Evening
#lowest and highest p values calcuated by four chains
#minimum
nyu.s3salt.amu.mssbmi.ci.pmin <- apply(as.data.frame(cbind(nyu.2salt.amu.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=min)
#maximum
nyu.s3salt.amu.mssbmi.ci.pmax <- apply(as.data.frame(cbind(nyu.2salt.amu.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], nyu.2salt.amu.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
nyu.s3salt.amu.mssbmi.tab.f <- as.data.frame(cbind(nyu.s3salt.amu.mssbmi.ci.sres[,c(1,3:5)], nyu.s3salt.amu.mssbmi.ci.pmin, nyu.s3salt.amu.mssbmi.ci.pmax, nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], confint(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS3D1D2), nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], nyu.yom.amu.2salt.sres$z.log.meanS3D1D2$coefficients[,1], confint(nyu.yom.amu.2salt.res$z.log.meanS3D1D2), nyu.yom.amu.2salt.sres$z.log.meanS3D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
nyu.s3salt.amu.mssbmi.ci <- apply(round(nyu.s3salt.amu.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
nyu.s3salt.amu.mssbmi.ci.prg <- apply(round(nyu.s3salt.amu.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
nyu.s3salt.amu.mssbmi.95ci <- apply(round(nyu.s3salt.amu.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
nyu.s3salt.amu.95ci <- apply(round(nyu.s3salt.amu.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

nyu.s3salt.amu.mssbmi.tab <- as.data.frame(
  cbind(paste(round(nyu.s3salt.amu.mssbmi.ci.sres[,1], 3), paste0("(", nyu.s3salt.amu.mssbmi.ci,")")), 
        nyu.s3salt.amu.mssbmi.ci.prg, 
        paste(round(nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", nyu.s3salt.amu.mssbmi.95ci,")")),
        round(nyu.yom.amu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], 2),
        paste(round(nyu.yom.amu.2salt.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", nyu.s3salt.amu.95ci,")")),
        round(nyu.yom.amu.2salt.sres$z.log.meanS3D1D2$coefficients[,4],2)))

#rename variables
names(nyu.s3salt.amu.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
nyu.s3salt.amu.mssbmi.tab.ht <- stargazer(nyu.s3salt.amu.mssbmi.tab, digits=2, out="nyu.s3salt.amu.mssbmi.tab.html", summary=FALSE, type="html")

#Table Hypot 2.3.2i SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression with age at recruitment and with imputed BMI from package "mi"-------
#Waking
#lowest and highest p values calcuated by four chains
#minimum
y19b.age.s1salt.mssbmi.ci.pmin <- apply(as.data.frame(cbind(y19b.age.2salt.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=min)
#maximum
y19b.age.s1salt.mssbmi.ci.pmax <- apply(as.data.frame(cbind(y19b.age.2salt.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
y19b.age.s1salt.mssbmi.tab.f <- as.data.frame(cbind(y19b.age.s1salt.mssbmi.ci.sres[,c(1,3:5)], y19b.age.s1salt.mssbmi.ci.pmin, y19b.age.s1salt.mssbmi.ci.pmax, y19b.age.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], confint(y19b.age.2salt.mssbmi.res$z.log.meanS1D1D2), y19b.age.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], y19b.age.2salt.sres$z.log.meanS1D1D2$coefficients[,1], confint(y19b.age.2salt.res$z.log.meanS1D1D2), y19b.age.2salt.sres$z.log.meanS1D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
y19b.age.s1salt.mssbmi.ci <- apply(round(y19b.age.s1salt.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
y19b.age.s1salt.mssbmi.ci.prg <- apply(round(y19b.age.s1salt.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
y19b.age.s1salt.mssbmi.95ci <- apply(round(y19b.age.s1salt.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
y19b.age.s1salt.95ci <- apply(round(y19b.age.s1salt.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

y19b.age.s1salt.mssbmi.tab <- as.data.frame(
  cbind(paste(round(y19b.age.s1salt.mssbmi.ci.sres[,1], 3), paste0("(", y19b.age.s1salt.mssbmi.ci,")")), 
        y19b.age.s1salt.mssbmi.ci.prg, 
        paste(round(y19b.age.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", y19b.age.s1salt.mssbmi.95ci,")")),
        round(y19b.age.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], 2),
        paste(round(y19b.age.2salt.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", y19b.age.s1salt.95ci,")")),
        round(y19b.age.2salt.sres$z.log.meanS1D1D2$coefficients[,4],2)))

#rename variables
names(y19b.age.s1salt.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
y19b.age.s1salt.mssbmi.tab.ht <- stargazer(y19b.age.s1salt.mssbmi.tab, digits=2, out="y19b.age.s1salt.mssbmi.tab.html", summary=FALSE, type="html")

#Evening
#lowest and highest p values calcuated by four chains
#minimum
y19b.age.s3salt.mssbmi.ci.pmin <- apply(as.data.frame(cbind(y19b.age.2salt.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=min)
#maximum
y19b.age.s3salt.mssbmi.ci.pmax <- apply(as.data.frame(cbind(y19b.age.2salt.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], y19b.age.2salt.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
y19b.age.s3salt.mssbmi.tab.f <- as.data.frame(cbind(y19b.age.s3salt.mssbmi.ci.sres[,c(1,3:5)], y19b.age.s3salt.mssbmi.ci.pmin, y19b.age.s3salt.mssbmi.ci.pmax, y19b.age.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], confint(y19b.age.2salt.mssbmi.res$z.log.meanS3D1D2), y19b.age.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], y19b.age.2salt.sres$z.log.meanS3D1D2$coefficients[,1], confint(y19b.age.2salt.res$z.log.meanS3D1D2), y19b.age.2salt.sres$z.log.meanS3D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
y19b.age.s3salt.mssbmi.ci <- apply(round(y19b.age.s3salt.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
y19b.age.s3salt.mssbmi.ci.prg <- apply(round(y19b.age.s3salt.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
y19b.age.s3salt.mssbmi.95ci <- apply(round(y19b.age.s3salt.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
y19b.age.s3salt.95ci <- apply(round(y19b.age.s3salt.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

y19b.age.s3salt.mssbmi.tab <- as.data.frame(
  cbind(paste(round(y19b.age.s3salt.mssbmi.ci.sres[,1], 3), paste0("(", y19b.age.s3salt.mssbmi.ci,")")), 
        y19b.age.s3salt.mssbmi.ci.prg, 
        paste(round(y19b.age.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", y19b.age.s3salt.mssbmi.95ci,")")),
        round(y19b.age.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], 2),
        paste(round(y19b.age.2salt.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", y19b.age.s3salt.95ci,")")),
        round(y19b.age.2salt.sres$z.log.meanS3D1D2$coefficients[,4],2)))

#rename variables
names(y19b.age.s3salt.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
y19b.age.s3salt.mssbmi.tab.ht <- stargazer(y19b.age.s3salt.mssbmi.tab, digits=2, out="y19b.age.s3salt.mssbmi.tab.html", summary=FALSE, type="html")


#Table Hypot 2.4.2i SalT within CHI migs, migration before age 9 as compared to migration age 9-19 "age.8.19.mig" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 19yo migs only with imputed BMI-----
#Waking
#lowest and highest p values calcuated by four chains
#minimum
y19c.nyu.s1salt.mssbmi.ci.pmin <- apply(as.data.frame(cbind(y19c.nyu.2salt.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=min)
#maximum
y19c.nyu.s1salt.mssbmi.ci.pmax <- apply(as.data.frame(cbind(y19c.nyu.2salt.mssbmi.ci1.sres$z.log.meanS1D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci2.sres$z.log.meanS1D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci3.sres$z.log.meanS1D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci4.sres$z.log.meanS1D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
y19c.nyu.s1salt.mssbmi.tab.f <- as.data.frame(cbind(y19c.nyu.s1salt.mssbmi.ci.sres[,c(1,3:5)], y19c.nyu.s1salt.mssbmi.ci.pmin, y19c.nyu.s1salt.mssbmi.ci.pmax, y19c.nyu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], confint(y19c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2), y19c.nyu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], y19c.nyu.2salt.sres$z.log.meanS1D1D2$coefficients[,1], confint(y19c.nyu.2salt.res$z.log.meanS1D1D2), y19c.nyu.2salt.sres$z.log.meanS1D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
y19c.nyu.s1salt.mssbmi.ci <- apply(round(y19c.nyu.s1salt.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
y19c.nyu.s1salt.mssbmi.ci.prg <- apply(round(y19c.nyu.s1salt.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
y19c.nyu.s1salt.mssbmi.95ci <- apply(round(y19c.nyu.s1salt.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
y19c.nyu.s1salt.95ci <- apply(round(y19c.nyu.s1salt.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

y19c.nyu.s1salt.mssbmi.tab <- as.data.frame(
  cbind(paste(round(y19c.nyu.s1salt.mssbmi.ci.sres[,1], 3), paste0("(", y19c.nyu.s1salt.mssbmi.ci,")")), 
        y19c.nyu.s1salt.mssbmi.ci.prg, 
        paste(round(y19c.nyu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", y19c.nyu.s1salt.mssbmi.95ci,")")),
        round(y19c.nyu.2salt.mssbmi.sres$z.log.meanS1D1D2$coefficients[,4], 2),
        paste(round(y19c.nyu.2salt.sres$z.log.meanS1D1D2$coefficients[,1], 3), paste0("(", y19c.nyu.s1salt.95ci,")")),
        round(y19c.nyu.2salt.sres$z.log.meanS1D1D2$coefficients[,4],2)))

#rename variables
names(y19c.nyu.s1salt.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
y19c.nyu.s1salt.mssbmi.tab.ht <- stargazer(y19c.nyu.s1salt.mssbmi.tab, digits=2, out="y19c.nyu.s1salt.mssbmi.tab.html", summary=FALSE, type="html")

#Evening
#lowest and highest p values calcuated by four chains
#minimum
y19c.nyu.s3salt.mssbmi.ci.pmin <- apply(as.data.frame(cbind(y19c.nyu.2salt.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=min)
#maximum
y19c.nyu.s3salt.mssbmi.ci.pmax <- apply(as.data.frame(cbind(y19c.nyu.2salt.mssbmi.ci1.sres$z.log.meanS3D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci2.sres$z.log.meanS3D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci3.sres$z.log.meanS3D1D2$coefficients[,4], y19c.nyu.2salt.mssbmi.ci4.sres$z.log.meanS3D1D2$coefficients[,4])), 1, FUN=max)

#create full data frame with comparative coefficients
y19c.nyu.s3salt.mssbmi.tab.f <- as.data.frame(cbind(y19c.nyu.s3salt.mssbmi.ci.sres[,c(1,3:5)], y19c.nyu.s3salt.mssbmi.ci.pmin, y19c.nyu.s3salt.mssbmi.ci.pmax, y19c.nyu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], confint(y19c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2), y19c.nyu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], y19c.nyu.2salt.sres$z.log.meanS3D1D2$coefficients[,1], confint(y19c.nyu.2salt.res$z.log.meanS3D1D2), y19c.nyu.2salt.sres$z.log.meanS3D1D2$coefficients[,4]))

#collapse range values into single columns
# create a new column of 95% CI for single collapsed column, then paste to coefficent
y19c.nyu.s3salt.mssbmi.ci <- apply(round(y19c.nyu.s3salt.mssbmi.tab.f[ , 2:3], 3) , 1 , paste , collapse = ", " )
y19c.nyu.s3salt.mssbmi.ci.prg <- apply(round(y19c.nyu.s3salt.mssbmi.tab.f[ , 5:6], 2) , 1 , paste , collapse = "-" )
y19c.nyu.s3salt.mssbmi.95ci <- apply(round(y19c.nyu.s3salt.mssbmi.tab.f[ , 8:9], 3) , 1 , paste , collapse = ", " )
y19c.nyu.s3salt.95ci <- apply(round(y19c.nyu.s3salt.mssbmi.tab.f[ , 12:13], 3) , 1 , paste , collapse = ", " )

y19c.nyu.s3salt.mssbmi.tab <- as.data.frame(
  cbind(paste(round(y19c.nyu.s3salt.mssbmi.ci.sres[,1], 3), paste0("(", y19c.nyu.s3salt.mssbmi.ci,")")), 
        y19c.nyu.s3salt.mssbmi.ci.prg, 
        paste(round(y19c.nyu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", y19c.nyu.s3salt.mssbmi.95ci,")")),
        round(y19c.nyu.2salt.mssbmi.sres$z.log.meanS3D1D2$coefficients[,4], 2),
        paste(round(y19c.nyu.2salt.sres$z.log.meanS3D1D2$coefficients[,1], 3), paste0("(", y19c.nyu.s3salt.95ci,")")),
        round(y19c.nyu.2salt.sres$z.log.meanS3D1D2$coefficients[,4],2)))

#rename variables
names(y19c.nyu.s3salt.mssbmi.tab) <- c("Pooled MI estimate (95% CI)",  "p range", "Population mean imputed BMI estimate (95% CI)", "p", "Complete cases BMI estimate (95% CI)", "p")
y19c.nyu.s3salt.mssbmi.tab.ht <- stargazer(y19c.nyu.s3salt.mssbmi.tab, digits=2, out="y19c.nyu.s3salt.mssbmi.tab.html", summary=FALSE, type="html")
