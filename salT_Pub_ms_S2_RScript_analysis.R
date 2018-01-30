#S2
#R script for Manuscript: "Childhood ecology influences salivary testosterone and pubertal age of Bangladeshi UK migrant men"

#Packages used----------------------------------------------------------------------------------------------------------------------------
library(multcomp)
library(foreign, pos=4)
library(reshape)
library(car)
library(RCurl)

#Run following script to load:
#download from source file
repro.data <- read.csv(text = getURL("https://raw.githubusercontent.com/kessonovitch/BHAI_Data/master/reproData.csv"))
#inspect
# str(repro.data)
#relevel factor variables to make sedentees or oldest migrant groups the reference levels (if necessary)
repro.data$residence16 <- factor(repro.data$residence16, levels=c("Bangladeshi sedentees", "Adult migrants", "Child migrants", "Second generation migrants", "British European"))
repro.data$residence18 <- factor(repro.data$residence18, levels=c("Bangladeshi sedentees", "Adult migrants", "Child migrants", "Second generation migrants", "British European"))
#for list of variables write.csv(names(repro.data), file = "varList.csv", row.names = FALSE)
#SUBSETS---------------------------------------------------------------------------------------------------------------------------------------

#subset databases of only those who migrated as children 
#subset of data for those who migrated before age 16
mig.16.data <- subset(repro.data, repro.data$residence16=="Child migrants")
#subset of data for those who migrated before age 18
mig.18.data <- subset(repro.data, repro.data$residence18=="Child migrants")
mig.18.data$age.8.18.mig <- droplevels(mig.18.data)$age.8.18.mig
#subset of data of child migrants who migrated before age 9
mig.8.data <- subset(repro.data, age.8.16.mig=="Birth-9y")

#create a database of only those who migrated as adults 
mig.adu.data <- subset(repro.data, repro.data$residence16=="Adult migrants")
#create a database of only those who migrated as adults and age recruit < 40 
mig.adu40a.data <- subset(repro.data, repro.data$residence16=="Adult migrants"& AgeRecruit <= 40)
#create a database of only those who migrated as adults and age recruit > 40
mig.adu40b.data <- subset(repro.data, repro.data$residence16=="Adult migrants"& AgeRecruit > 40)
#create a database of only those who migrated as adults 18yo cutoff 
mig.adu18.data <- subset(repro.data, repro.data$residence18=="Adult migrants")

#create database of only UK resident groups (divided at ≤16)
ukres16.data <- subset(repro.data, residence16=="Adult migrants" | residence16=="Child migrants" | residence16=="Second generation migrants" | residence16== "British European")
#create database of only ethnic Bengali UK resident groups
ukresbds16.data <- subset(repro.data, residence16=="Adult migrants" | residence16=="Child migrants" | residence16=="Second generation migrants")

#LISTS----------------------------------------------------------------------------------------------------------------------------

#list of 2 (WAKE and BED) logged z. trans. salT varaibles
z.log.2salt <- repro.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
unt.2salt <- repro.data[,c("MeanS1D1D2", "MeanS3D1D2")]
unt.ukres.2salt <- ukres16.data[,c("MeanS1D1D2", "MeanS3D1D2")]
z.log.8.2salt <- mig.8.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.18.2salt <- mig.18.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.16.2salt <- mig.16.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.adu.2salt <- mig.adu.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.adu40a.2salt <- mig.adu40a.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]
z.log.adu40b.2salt <- mig.adu40b.data[, c("z.log.meanS1D1D2", "z.log.meanS3D1D2")]

#list of z. trans. puberty varaibles
z.pub <- repro.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.8.pub <- mig.8.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.18.pub <- mig.18.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.16.pub <- mig.16.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.adu18.pub <- mig.adu18.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]
z.ukbd.pub <- ukresbds16.data[, c("z.pub.voice", "z.pub.shave", "z.pub.pub", "z.pub.ne", "z.pub.compos")]

#DESCRIPTIVES--------------------------------------------------------------------------------------------------------------------
#age differences between residence groups, split at age 16
age.res <- lm(AgeRecruit~residence16, data=repro.data)
age.sres <- summary(age.res)
confint(repro.data$AgeRecruit)
##All migrant groups significantly different from sedentees (ADU older, CHI and 2NG younger). EUR not different from SED
#post-hoc analysis
age.res.ph <- summary(glht(age.res, linfct=mcp(residence16="Tukey")))
## Age: ADU > CHI, 2NG, EUR; EUR > CHI, 2NG; CHI > 2NG

#within child migrants, note a difference in age at recruitment by two sample t-test
t.test(mig.16.data$AgeRecruit~mig.16.data$age.8.16.mig)
#therefore the mean age at recruitment mean in group 9-16y (36.36) mean in group Birth-9y (27.95) is different betweent these migration cohorts t = 3.132, df = 28.362, p-value = 0.004006. (those who migrated at earlier ages are sig. younger, likely reflective of the historical patterns of migration: peak migration of child migrants occured around the same time).

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

#Model 4, for those who migrated: number of years "nyu" in the UK lm(phys ~z.NumYearsUK, data=mig.16.data)
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
  aov(x ~AgeRecruit+residence16, data=db)	
}

#Model 5.2, age at recruitment ANCOVA
agerecxres.aov <- function(x, y, db){
  aov(x ~AgeRecruit*residence16, data=db)	
}


#Salivary Testosterone Hypotheses-------------------------------------------------------------------------------------------------
#Hypothesis 1: Cohorts separated by ethnicity and developmental exposure to ecological conditions will differ in salivary T

#Hypot 1.1 SalT by residence: sd units (log)age and bmi as covariates
res.2salt.res <- lapply(z.log.2salt, y=repro.data$residence16, db=repro.data, age.bmi.lm)
res.2salt.sres <- lapply(res.2salt.res, summary)
## for Wake and Bed samples CHI and 2NG salT is higher compared to SED, BMI also sig. covariate

#for post-hoc multiple comparison Tukey correction of all-pair multiple comparison
res.salt1.res.ph <- summary(glht(res.2salt.res$z.log.meanS1D1D2, linfct=mcp(y="Tukey")))
## Wake: CHI > ADU; 2NG > ADU; 2NG > EUR.
res.salt3.res.ph <- summary(glht(res.2salt.res$z.log.meanS3D1D2, linfct=mcp(y="Tukey")))
#Bed: 2NG > ADU 

#Hypot 1.2 age at migration "z.AgeMigUK" as continuous predictor after adjusting for number of years in uk "nyu" and age at migration regressions in all migs
nyu.amu.2salt.res <- lapply(z.log.2salt, y=repro.data$z.AgeMigUK, db=repro.data, nyu.bmi.lm)
nyu.amu.2salt.sres <- lapply(nyu.amu.2salt.res, summary)
#vif(nyu.amu.2salt.res$z.log.meanS1D1D2)
##Number of years in UK and age of migration significant negative predictors of salivary T for all migrants

#Hypotheses 2: Childhood age at migration is a predictor of adult salivary T

#Hypot 2.1.1 within CHI only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 16years "y16" regressions
y16a.2salt.res <- lapply(z.log.16.2salt, y=mig.16.data$z.AgeMigUK, db=mig.16.data, age.bmi.lm)
y16a.2salt.sres <- lapply(y16a.2salt.res, summary)
##for Age Mig within CHI only, waking and bed time points, no sig covariates

#Hypot 2.1.2 within CHI only, age at migration "z.AgeMigUK" as continuous predictor who arrived before 16years "y16" regressions, with imputed BMI
y16a.2salt.mssbmi.res <- lapply(z.log.16.2salt, y=mig.16.data$z.AgeMigUK, db=mig.16.data, age.mss.bmi.lm)
y16a.2salt.mssbmi.sres <- lapply(y16a.2salt.mssbmi.res, summary)
##Age Mig n.s. within CHI only, Waking and Bed sample. Age sig. covariate for Wake sample

#Hypot 2.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 16yo migs only
nyu.yom.amu.2salt.res <- lapply(z.log.16.2salt, y=mig.16.data$z.AgeMigUK, db=mig.16.data, nyu.bmi.lm)
nyu.yom.amu.2salt.sres <- lapply(nyu.yom.amu.2salt.res, summary)
##for Age Mig within Child migs only, number of years in the UK and bmi as covariates, waking and evening time points both nearly (p=0.053; p=0.052) sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK

#Hypot 2.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 16yo migs only with imputed BMI
nyu.yom.amu.2salt.mssbmi.res <- lapply(z.log.16.2salt, y=mig.16.data$z.AgeMigUK, db=mig.16.data, nyu.mssbmi.lm)
nyu.yom.amu.2salt.mssbmi.sres <- lapply(nyu.yom.amu.2salt.mssbmi.res, summary)
##for Age Mig within Child migs only, number of years in the UK and imputed bmi as covariates, waking and evening time points both sig. neg predicted by age at migration, waking also neg. predicted by number of years in the UK

#Hypot 2.3.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 regression with age at recruitment 
y16b.age.2salt.res <- lapply(z.log.16.2salt, y=mig.16.data$age.8.16.mig, db=mig.16.data, age.bmi.lm)
y16b.age.2salt.sres <- lapply(y16b.age.2salt.res, summary)
##for Wake, age recruit neg. sig. relationship. For Bed, n.s. for all covariates 
#vif(y16c.nyu.2salt.res$z.log.meanS1D1D2)

#Hypot 2.3.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 "age.8.16.mig" regression with age at recruitment and imputed BMI
y16b.age.2salt.mssbmi.res <- lapply(z.log.16.2salt, y=mig.16.data$age.8.16.mig, db=mig.16.data, age.mss.bmi.lm)
y16b.age.2salt.mssbmi.sres <- lapply(y16b.age.2salt.mssbmi.res, summary)
##for Wake age recruit neg. sig. relationship. For Bed, n.s. for all covariates 

#Hypot 2.4.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 "age.8.16.mig" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 16yo migs only
y16c.nyu.2salt.res <- lapply(z.log.16.2salt, y=mig.16.data$age.8.16.mig, db=mig.16.data, nyu.bmi.lm)
y16c.nyu.2salt.sres <- lapply(y16c.nyu.2salt.res, summary)
##for Wake, Birth to age 9 > 9-16y migs, Num Years neg. sig. relationship. For Bed, n.s. for all covariates but note small sample size (n=24) for both sample times
#vif(y16c.nyu.2salt.res$z.log.meanS1D1D2)

#Hypot 2.4.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 "age.8.16.mig" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 16yo migs only with imputed BMI
y16c.nyu.2salt.mssbmi.res <- lapply(z.log.16.2salt, y=mig.16.data$age.8.16.mig, db=mig.16.data, nyu.mssbmi.lm)
y16c.nyu.2salt.mssbmi.sres <- lapply(y16c.nyu.2salt.mssbmi.res, summary)
##for Wake and Bed samples, Birth to age 9 > 9-16y migs, Num Years neg. sig. relationship. for wake only.

#Hypot 3 Adult conditions influence salivary testosterone

#Hypot 3.1.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi amu regressions in ADU over 16yo migs only
nyu.adu.amu.2salt.res <- lapply(z.log.adu.2salt, y=mig.adu.data$z.AgeMigUK, db=mig.adu.data, nyu.bmi.lm)
nyu.adu.amu.2salt.sres <- lapply(nyu.adu.amu.2salt.res, summary)
##Within all Adult migs, number of years in the UK and bmi as covariates, both waking and evening time points were sig. predicted by number of years in the UK, while age at migration was n.s. this could be an age effect, an ecological effect or a combination of both.

#Hypot 3.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 16yo migs only, <40yo age at recruitment cohort
nyu.adu40a.amu.2salt.res <- lapply(z.log.adu40a.2salt, y=mig.adu40a.data$z.AgeMigUK, db=mig.adu40a.data, nyu.bmi.lm)
nyu.adu40a.amu.2salt.sres <- lapply(nyu.adu40a.amu.2salt.res, summary)
##for Age Mig within younger Adult migs <40 only, number of years in the UK and bmi as covariates, neither waking nor evening time points were sig. predicted by any of the covariates

#Hypot 3.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 16yo migs only, >40yo age at recruitment cohort
nyu.adu40b.amu.2salt.res <- lapply(z.log.adu40b.2salt, y=mig.adu40b.data$z.AgeMigUK, db=mig.adu40b.data, nyu.bmi.lm)
nyu.adu40b.amu.2salt.sres <- lapply(nyu.adu40b.amu.2salt.res, summary)
##for Age Mig within older Adult migs >40 only, number of years in the UK sig. neg correlation with evening time point only. No other sig. predicted by any of the covariates

#Age at recruitment hypotheses-----------------------------------------------------------------

#Hypot 4.1.1 age at recruitment is continuous predictor of salivary testosterone across all populations
agerec.2salt.res <- lapply(unt.2salt, db=repro.data, agerec.lm)
agerec.2salt.sres <- lapply(agerec.2salt.res, summary)
agerec.2salt.ci <- lapply(agerec.2salt.res, confint)
agerec.wksalt.n <- length(resid(agerec.2salt.res$MeanS1D1D2))
agerec.bdsalt.n <- length(resid(agerec.2salt.res$MeanS3D1D2))
##sig. negative decline in wake salT of .67 pg/mL per year, sig. negative decline in bed salT of .47 pg/mL per year

#Hypot 4.2.1 are the slopes significantly different between groups for salT?
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
summary(glht(res.agerecxres$MeanS1D1D2, linfct = mcp(residence16 = "Tukey")))
##in post hoc testing, only the child migrants differ significantly in slope the EUR and ADU, however caution should be taken as there is also an interaction effect present
#sample size: summary(model.frame(res.agerecxres$MeanS1D1D2)$residence16)

#Hypot 4.3.1 age at recruitment is continuous predictor of salivary testosterone within residence groups
#Bengali Sedentees
#Wake salT
sed.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Bangladeshi sedentees"))
sed.s1d1d2.age.sres <- summary(sed.s1d1d2.age.res)
sed.s1d1d2.age.ci <- confint(sed.s1d1d2.age.res)
##positive trend in wake salT of 1.09 pg/mL per year
#Evening salT
sed.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Bangladeshi sedentees"))
sed.s3d1d2.age.sres <- summary(sed.s3d1d2.age.res)
sed.s3d1d2.age.ci <- confint(sed.s3d1d2.age.res)
##positive trend in bed salT of 1.04 pg/mL per year

#Adult migrants
#AM salT
adu.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Adult migrants"))
adu.s1d1d2.age.sres <- summary(adu.s1d1d2.age.res)
adu.s1d1d2.age.ci <- confint(adu.s1d1d2.age.res)
##negative trend in wake salT of .76 pg/mL per year
#Evening salT
adu.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Adult migrants"))
adu.s3d1d2.age.sres <- summary(adu.s3d1d2.age.res)
adu.s3d1d2.age.ci <- confint(adu.s3d1d2.age.res)
##negative trend in bed salT of .89 pg/mL per year

#Child migrants 
#AM salT
chi.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Child migrants"))
chi.s1d1d2.age.sres <- summary(chi.s1d1d2.age.res)
chi.s1d1d2.age.ci <- confint(chi.s1d1d2.age.res)
##neg trend in wake salT of 3.67 pg/mL per year
#Evening salT
chi.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Child migrants"))
chi.s3d1d2.age.sres <- summary(chi.s3d1d2.age.res)
chi.s3d1d2.age.ci <- confint(chi.s3d1d2.age.res)
##neg trend in bed salT of 2.15 pg/mL per year

#Second Generation Migrants 
#AM salT
sg.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Second generation migrants"))
sg.s1d1d2.age.sres <- summary(sg.s1d1d2.age.res)
sg.s1d1d2.age.ci <- confint(sg.s1d1d2.age.res)
##no sig. trend in wake salT (n.s. of -0.60 pg/mL per year)
#Evening salT
sg.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence16=="Second generation migrants"))
sg.s3d1d2.age.sres <- summary(sg.s3d1d2.age.res)
sg.s3d1d2.age.ci <- confint(sg.s3d1d2.age.res)
##no sig. trend in evening salT (n.s. of -2.63 pg/mL per year)

#British European
#AM salT
eur.s1d1d2.age.res <- lm(MeanS1D1D2 ~ AgeRecruit, subset(repro.data, residence16=="British European"))
eur.s1d1d2.age.sres <- summary(eur.s1d1d2.age.res)
eur.s1d1d2.age.ci <- confint(eur.s1d1d2.age.res)
##no sig. trend in wake salT (n.s. of -.70 pg/mL per year)
eur.s3d1d2.age.res <- lm(MeanS3D1D2 ~ AgeRecruit, subset(repro.data, residence16=="British European"))
eur.s3d1d2.age.sres <- summary(eur.s3d1d2.age.res)
eur.s3d1d2.age.ci <- confint(eur.s3d1d2.age.res)
##no sig. trend in wake salT (n.s. of -.35 pg/mL per year)

##4.3.1 conclusions: relationship between salT and age varies widely between groups. 
##The overall population decline is less steep than that reported elsewhere in longitudinal studies by Harman et al 2001 The rate of decline of serum testosterone in Caucasion men according to Harman et al is 0.110 nmol/L per year.  This is 0.11 x 0.288 = 0.032 ng/ml per year or 32 pg/ml per year.  
##Salivary testosterone according to Wang et al is 1/15 that of the serum concentration across a wide range of values.  Therefore, this gives 32 x 1/15 = 2.133 pg/ml per year times 17.2 years is 36.69 pg/ml. 

#Hypot 4.4.1 are the slopes significantly different between groups for salT, for those showing a decline (UK resident men only)?
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, set Europeans as reference group
ukres16.data$residence16 <- relevel(ukres16.data$residence16, ref="British European")
res.agerec.ukres <- lapply(unt.ukres.2salt, db=ukres16.data, agerec.aov)
lapply(res.agerec.ukres, summary)
##indicates a signficant effect of both age and residence for waking salT only

#also test for interaction
#method: ANCOVA of salT as dependent variable with age as predictor including all groups, as an interaction, then test for differences post-hoc
res.agerecxukres <- lapply(unt.ukres.2salt, db=ukres16.data, agerecxres.aov)
lapply(res.agerecxukres, summary)
##indicates a signficant effect of age*residence interaction for waking only, therefore suggesting there are differences between slopes by residence group in waking salT.

#post-hoc testing of waking salT only
summary(glht(res.agerecxukres$MeanS1D1D2, linfct = mcp(residence16 = "Tukey")))
##in post hoc testing, only the child migrants differ significantly in slope the EUR and ADU, however caution should be taken as there is also an interaction effect present

#alternative linear model using transformed data and bmi to match other regressions in the paper
summary(lm(z.log.meanS1D1D2~z.log.age+z.bmi+residence16+z.log.age*residence16, data=ukres16.data))
confint(lm(z.log.meanS1D1D2~z.log.age+z.bmi+residence16+z.log.age*residence16, data=ukres16.data))
summary(model.frame(lm(z.log.meanS1D1D2~z.log.age+z.bmi+residence16+z.log.age*residence16, data=ukres16.data))$residence16)
##compared to EUR, CHI migrants are -0.56 SD units 95% CI: -1.315, -0.177 sig. steeper slope

#Hypot 4.5.1 When confnining analysis to men who did not show a sig relationship between age and salT (UK born men) is there a significant age related decline in salT, when not considering residence group?
#AM salT
res.age.ukborn.s1 <- lm(MeanS1D1D2~AgeRecruit, data=subset(repro.data, residence16=="British European"| residence16=="Second generation migrants"))
summary(res.age.ukborn.s1)
confint(res.age.ukborn.s1)
length(resid(res.age.ukborn.s1))
#significant age realted decline of waking salT -1.22 (95%CI: -2.011599, -0.4347596) within men born and raised in the UK

#Eve salT
res.age.ukborn.s3 <- lm(MeanS3D1D2~AgeRecruit, data=subset(repro.data, residence16=="British European"| residence16=="Second generation migrants"))
summary(res.age.ukborn.s3)
confint(res.age.ukborn.s3)
length(resid(res.age.ukborn.s3))
#non-sig age realted decline of evening salT of -0.90 pg/ml per year (95%CI: -1.839, 0.0208) within men born and raised in the UK

#Hypot 4.5.2 Does adjustment of waking salT to decline found in UK born groups eliminate differences between Bengali and European groups?
#create an adjusted variable for waking salT adding 1.22pg/ml for each year over age 22 men were at recruitment
ukres16.data$S1D1D2.uk.adj <- ifelse(ukres16.data$AgeRecruit<=22, ukres16.data$MeanS1D1D2, ((ukres16.data$AgeRecruit-22)*1.22)+ukres16.data$MeanS1D1D2)

#linear model comparing UK-born groups with adjusted salT and z.bmi
summary(lm(S1D1D2.uk.adj~AgeRecruit+z.bmi+residence16, data=subset(ukres16.data, residence16=="British European"| residence16=="Second generation migrants")))
confint(lm(S1D1D2.uk.adj~AgeRecruit+z.bmi+residence16, data=subset(ukres16.data, residence16=="British European"| residence16=="Second generation migrants")))
##after adjusting correction factor, second generation waking salT remain 33.7 pg/ml higher (p=.04) than British Eur

##Puberty Hypotheses-----------------------------------------------------

#Hypothesis 5: Cohorts separated by ethnicity and developmental exposure to ecological conditions will differ in recalled markers of age at puberty.

#Hypot 5.1.1, age at puberty determined by residence after adjustment for recall bias and demographic trends by age at recruitment.
res.pub.res <- lapply(z.pub, y=repro.data$residence18, db=repro.data, age.lm)
res.pub.sres <- lapply(res.pub.res, summary)
#sample sizes: summary(model.frame(res.pub.res$z.pub.compos)$y)
#compared to sedentees, 2NG and EUR are both sig. earlier age of puberty in composite and all measures (besides voice for 2NG), CHI recall earlier age at shaving

#for post-hoc multiple comparison Tukey correction of all-pair multiple comparison
res.pub.res.ph <- summary(glht(res.pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
## Composite value: EUR > SED, ADU, CHI; 2NG > SED, ADU

#Hypot 5.2.1, age at puberty determined by age at migration of child migrants (age 18 cutoff)
amu.18pub.res <- lapply(z.18.pub, y=mig.18.data$z.AgeMigUK, db=mig.18.data, function(x, y, db){
  lm(x~y, data=db)	
})
amu.18pub.sres <- lapply(amu.18pub.res, summary)
#within child migrants (<18 years) only, sig. positive correlation between age at migration and composite age at puberty, sig. for age at shaving and pubic hair. Negative relationship between age at recruitment and age at shaving (older men recall shaving earlier)
#sample sizes: n=27

#Hypot 5.2.2, age at puberty determined by age at migration of child migrants (age 18 cutoff), after adjustment for recall bias and demographic trends by age at recruitment.
amu.age.18pub.res <- lapply(z.18.pub, y=mig.18.data$z.AgeMigUK, db=mig.18.data, age.lm)
amu.age.18pub.sres <- lapply(amu.age.18pub.res, summary)
amu.age.18pub.ci <- lapply(amu.age.18pub.res, confint)
#sample size: nrow(model.frame(amu.age.18pub.res$z.pub.compos))
#within child migrants (<18 years) only, non sig. positive correlation between age at migration and composite age at puberty, sig. for age at shaving and pubic hair. Negative relationship between age at recruitment and age at shaving (older men recall shaving earlier)
#sample sizes: n=27

#Hypot 5.3.1, age at puberty determined by migration cohort 0-8 years child migrants and 9-18 years (age 18 cutoff), after adjustment for recall bias and demographic trends by age at recruitment.
cat.18pub.res <- lapply(z.pub, y=repro.data$age.8.18.mig, db=repro.data, age.lm)
cat.18pub.sres <- lapply(cat.18pub.res, summary)
cat.18pub.ci <- lapply(cat.18pub.res, confint)
summary(model.frame(cat.18pub.res$z.pub.compos)$y)

cat.18pub.res.ph <- summary(glht(cat.18pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
confint(glht(cat.18pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
##n.s. difference between composite age at puberty in 0-9, 9-18 groups (n=26)

#Hypot 5.3.2, age at puberty determined by migration cohort pre-birth-8 years (Second generation and child migrants) and 9-18 years (age 18 cutoff), after adjustment for recall bias and demographic trends by age at recruitment.
cat.b18pub.res <- lapply(z.pub, y=repro.data$age.b8.18.mig, db=repro.data, age.lm)
cat.b18pub.sres <- lapply(cat.b18pub.res, summary)
cat.b18pub.ci <- lapply(cat.b18pub.res, confint)
summary(model.frame(cat.b18pub.res$z.pub.compos)$y)

cat.b18pub.res.ph <- summary(glht(cat.b18pub.res$z.pub.compos, linfct=mcp(y="Tukey")))
confint(glht(cat.b18pub.res$z.pub.compos, linfct=mcp(y="Tukey")))

#Hypot 5.4.1, age at puberty determined by age at migration of adult migrants, after adjustment for recall bias and demographic trends by age at recruitment.
amu.adu18pub.res <- lapply(z.adu18.pub, y=mig.adu18.data$z.AgeMigUK, db=mig.adu18.data, age.lm)
amu.adu18pub.sres <- lapply(amu.adu18pub.res, summary)
##within adult migrants only, no indication of an age at migration effect on recalled puberty

#Hypotheses 6: Across all groups, without separation by ethnicity or developmental exposure to ecological conditions, adult salivary T is a predictor of recalled age at puberty

#Hypot 6.1.1 waking adult salT is a predictor of pub traits across all groups, after adjustment for recall bias and demographic trends by age at recruitment.
salt1.pub.res <- lapply(z.pub, y=repro.data$z.log.meanS1D1D2, db=repro.data, age.lm)
salt1.pub.sres <- lapply(salt1.pub.res, summary)
length(resid(salt1.pub.res$z.pub.compos))

#Hypot 6.1.2 evening adult salT is a predictor of pub traits across all groups, after adjustment for recall bias and demographic trends by age at recruitment.
salt3.pub.res <- lapply(z.pub, y=repro.data$z.log.meanS3D1D2, db=repro.data, age.lm)
salt3.pub.sres <- lapply(salt3.pub.res, summary)
length(resid(salt3.pub.res$z.pub.compos))

#Hypotheses 7: Within UK Bangladeshis only, adult salivary T is a predictor of recalled age at puberty, after adjustment for recall bias and demographic trends by age at recruitment.

#Hypot 7.1.1 waking adult salT as a predictor of pub traits within UK BDs, after adjustment for recall bias and demographic trends by age at recruitment.
salt1.ukbd.pub.res <- lapply(z.ukbd.pub, y=ukresbds16.data$z.log.meanS1D1D2, db=ukresbds16.data, age.lm)
salt1.ukbd.pub.sres <- lapply(salt1.ukbd.pub.res, summary)
##within UK BDs wake salT n.s. predictor of composite age at puberty, only nocturnal emmisions recall is sig.

#Hypot 7.1.2 evening adult salT is a predictor of pub traits within UK BDs, after adjustment for recall bias and demographic trends by age at recruitment.
salt3.ukbd.pub.res <- lapply(z.ukbd.pub, y=ukresbds16.data$z.log.meanS3D1D2, db=ukresbds16.data, age.lm)
salt3.ukbd.pub.sres <- lapply(salt3.ukbd.pub.res, summary)
#within all UK BD groups, Bed salT n.s. (p=0.06) predictor of composite recalled age of puberty, positive relationships between age and voice breaking as well as shaving. Younger men recall both at earlier ages.

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
leveneTest(z.log.meanS1D1D2 ~ residence16, center = mean, data = repro.data)
##levene test n.s.
leveneTest(z.log.meanS3D1D2 ~ residence16, center = mean, data = repro.data)
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
leveneTest(z.log.meanS1D1D2 ~ residence16, center = mean, data = ukresbds16.data)
##levene test n.s.
leveneTest(z.log.meanS3D1D2 ~ residence16, center = mean, data = ukresbds16.data)
##levene test n.s.

#Assumptions 2: Childhood age at migration is a predictor of adult salivary T

#Assumptions 2.1.1 within CHI only, age at migration "z.AgeMigUK" as continuous predictor for migrants who arrived before 16years "y16" regressions
#normality of distribution of residuals
hist(residuals(y16a.2salt.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic
hist(residuals(y16a.2salt.res$z.log.meanS3D1D2), col="darkgray")
##sample size limits on interpretation
#check for bias or heteroscedasticy
plot(fitted(y16a.2salt.res$z.log.meanS1D1D2), residuals(y16a.2salt.res$z.log.meanS1D1D2))
plot(fitted(y16a.2salt.res$z.log.meanS3D1D2), residuals(y16a.2salt.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#check for homogeneity of variance
leveneTest(z.log.meanS1D1D2 ~ age.8.mig.b, center = mean, data = mig.16.data)
##levene test sig. but visual inspection of residuals suggests limitations of sample size as opposed to systematic bias. Interpretation with caveats of sample size
leveneTest(z.log.meanS3D1D2 ~ age.8.mig.b, center = mean, data = repro.data)
##levene test just sig. (p=0.05) but proceed with inference with caveats of sample size

#Assumptions 2.1.2 within CHI only, age at migration "z.AgeMigUK" as continuous predictor who arrived before 16years "y16" regressions, with imputed BMI
#normality of distribution of residuals
hist(residuals(y16b.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic. Better normal distribution with additional points added by imputation.
hist(residuals(y16b.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##better normal distribution with additional points added by imputation.
#check for bias or heteroscedasticy
plot(fitted(y16b.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(y16b.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(y16b.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(y16b.2salt.mssbmi.res$z.log.meanS3D1D2))
##sample size limits on interpretation, possibly some narrowing of distribution at higher fitted values
#checks for homogeneity of variance same as above. Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 2.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" regressions in CHI under 16yo migs only
#nyu.yom.amu.2salt.sres
#normality of distribution of residuals
hist(residuals(nyu.yom.amu.2salt.res$z.log.meanS1D1D2), col="darkgray")
hist(residuals(nyu.yom.amu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(nyu.yom.amu.2salt.res$z.log.meanS1D1D2), residuals(nyu.yom.amu.2salt.res$z.log.meanS1D1D2))
plot(fitted(nyu.yom.amu.2salt.res$z.log.meanS3D1D2), residuals(nyu.yom.amu.2salt.res$z.log.meanS3D1D2))
##no systematic bias, some interpretation limited by sample size

#Assumptions 2.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk including ecological exposure: number of years in the UK "nyu" regressions in CHI under 16yo migs only with imputed BMI
#nyu.yom.amu.2salt.mssbmi.res
#normality of distribution of residuals
hist(residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
hist(residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##looks normally distributed
#check for bias or heteroscedasticy
plot(fitted(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(nyu.yom.amu.2salt.mssbmi.res$z.log.meanS3D1D2))
##no systematic bias, Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 2.3.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 regression with age at recruitment
#y16b.age.2salt.res 
#normality of distribution of residuals
hist(residuals(y16b.age.2salt.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic
hist(residuals(y16b.age.2salt.res$z.log.meanS3D1D2), col="darkgray")
##sample size limits on interpretation
#check for bias or heteroscedasticy
plot(fitted(y16b.age.2salt.res$z.log.meanS1D1D2), residuals(y16b.age.2salt.res$z.log.meanS1D1D2))
plot(fitted(y16b.age.2salt.res$z.log.meanS3D1D2), residuals(y16b.age.2salt.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#check for homogeneity of variance same
leveneTest(z.log.meanS1D1D2 ~ age.8.mig, center = mean, data = mig.16.data)
##levene test sig. but visual inspection of residuals suggests limitations of sample size as opposed to systematic bias. Interpretation with caveats of sample size
leveneTest(z.log.meanS3D1D2 ~ age.8.mig.b, center = mean, data = repro.data)
##levene test just sig. (p=0.05) but proceed with interpretation with caveats of sample size

#Assumptions 2.3.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 "age.16.mig.b" regression with age at recruitment and imputed BMI
#y16b.age.2salt.mssbmi.res
#normality of distribution of residuals
hist(residuals(y16b.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic. Better normal distribution with additional points added by imputation.
hist(residuals(y16b.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##better normal distribution with additional points added by imputation.
#check for bias or heteroscedasticy
plot(fitted(y16b.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(y16b.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(y16b.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(y16b.2salt.mssbmi.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#checks for homogeneity of variance same as above. Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 2.4.1 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 "age.16.mig.b" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 16yo migs only
#y16c.nyu.2salt.res
#normality of distribution of residuals
hist(residuals(y16c.nyu.2salt.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic
hist(residuals(y16c.nyu.2salt.res$z.log.meanS3D1D2), col="darkgray")
##sample size limits on interpretation
#check for bias or heteroscedasticy
plot(fitted(y16c.nyu.2salt.res$z.log.meanS1D1D2), residuals(y16c.nyu.2salt.res$z.log.meanS1D1D2))
plot(fitted(y16c.nyu.2salt.res$z.log.meanS3D1D2), residuals(y16c.nyu.2salt.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#check for homogeneity of variance same
leveneTest(z.log.meanS1D1D2 ~ age.8.mig, center = mean, data = mig.16.data)
##levene test sig. but visual inspection of residuals suggests limitations of sample size as opposed to systematic bias. Interpretation with caveats of sample size
leveneTest(z.log.meanS3D1D2 ~ age.8.mig.b, center = mean, data = repro.data)
##levene test just sig. (p=0.05) but proceed with interpretation with caveats of sample size

#Assumptions 2.4.2 SalT within CHI migs, migration before age 9 as compared to migration age 9-16 "age.16.mig.b" regression including ecological exposure: number of years in the UK "nyu" regressions in CHI under 16yo migs only with imputed BMI
#y16c.nyu.2salt.mssbmi.res
#normality of distribution of residuals
hist(residuals(y16c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2), col="darkgray")
##some leftward skew, but not dramatic. Better normal distribution with additional points added by imputation.
hist(residuals(y16c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2), col="darkgray")
##better normal distribution with additional points added by imputation.
#check for bias or heteroscedasticy
plot(fitted(y16c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2), residuals(y16c.nyu.2salt.mssbmi.res$z.log.meanS1D1D2))
plot(fitted(y16c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2), residuals(y16c.nyu.2salt.mssbmi.res$z.log.meanS3D1D2))
##sample size limits on interpretation
#checks for homogeneity of variance same as above. Increased n of imputed BMI improves inference, but still limited by sample size.

#Assumptions 3 Adult conditions influence salivary testosterone

#Assumptions 3.1.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi amu regressions in ADU over 16yo migs only
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

#Assumptions 3.2.1 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 16yo migs only, <40yo age at recruitment cohort
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

#Assumptions 3.2.2 age at migration "z.AgeMigUK" as continuous predictor with number of years in uk "nyu" and bmi. amu regressions in ADU over 16yo migs only, >40yo age at recruitment cohort
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
hist(residuals(agerec.2salt.res$MeanS1D1D2), col="darkgray")
##normal dist some left skew
hist(residuals(agerec.2salt.res$MeanS3D1D2), col="darkgray")
##normal dist
#check for bias or heteroscedasticy
plot(fitted(agerec.2salt.res$MeanS1D1D2), residuals(agerec.2salt.res$MeanS1D1D2))
plot(fitted(agerec.2salt.res$MeanS3D1D2), residuals(agerec.2salt.res$MeanS3D1D2))
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
leveneTest(MeanS1D1D2 ~ residence16, center = mean, data = mig.data)
##levene test n.s.
leveneTest(MeanS3D1D2 ~ residence16, center = mean, data = mig.data)
##levene test sig. Run with z.log mean salT for evening samples to verify any inferences based on slope (see salT decline by year regressions for details of these tests)

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
leveneTest(MeanS1D1D2 ~ residence16, center = mean, data = ukres16.data)
##levene test n.s.
leveneTest(MeanS3D1D2 ~ residence16, center = mean, data = ukres16.data)
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
leveneTest(z.pub.compos ~ residence18, center = mean, data = repro.data)
##levene test n.s.

