# This R script presents solutions to ECON 121 Problem Set 2.

# Note: You may have noticed that the dataset includes a
#       primary sampling unit identifier and a sampling 
#       weight. It would be reasonable to cluster standard
#       errors at the PSU level and weight using sampling
#       weights, but these details were not the focus of the
#       problem set and are therefore not required.

 # # # # # #
# Problem 2 # 
 # # # # # #
  
# Summary statistics appear below. 16 percent of the sample 
# reports being in fair or poor health, and 13 percent died 
# died before 2019. The sample has a median age of 49. (The mean 
# age is less meaningful because age was top-coded at 85. You
# did not need to notice this.) 56 percent of the sample
# is female, perhaps surprisingly. This gender imbalance
# has two sources. First, men and women responded to the 
# survey at different rates, so the gender imbalance shrinks
# when we use the sampling weights. Second, men die at 
# higher rates than women, so the gender imbalance grows
# with age.

library(haven)
nhis <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nhis2010.dta")

prop.table(table(nhis$health)) # prop.table converts frequencies into proportions
nhis$fpoor <- ifelse(nhis$health > 3, 1, 0)

summary(nhis)

prop.table(table(nhis$age)) 

prop.table(table(nhis$sex)) # 56% female
mean(nhis$sex) # 56% female
weighted.mean(nhis$sex,w = nhis$sampweight) # 52% female once we use weights

 # # # # # #
# Problem 3 # 
 # # # # # #

# 5-year mortality is higher for people with fair/poor health
# than for people with good/very good/excellent health. Thus,
# self-reported health status is predictive of mortality. In
# both groups, 5-year mortality rises non-linearly with age.

library(dplyr) # for group_by()
mort_fpoor_age <- nhis %>% 
                    filter(!is.na(fpoor) & !is.na(mort)) %>% # drop missing values
                    group_by(age, fpoor) %>% # group by age and fpoor
                    summarise(mort = mean(mort)) # mean of mort
mort_fpoor_age$fpoor.f <- factor(mort_fpoor_age$fpoor, # create factor variable for plot
                                 levels = c(0,1),
                                 labels = c("good/v. good/excellent","fair/poor")) 

library(ggplot2)
ggplot(data = mort_fpoor_age, aes(x = age, y = mort,group = fpoor.f,color = fpoor.f)) +
  geom_line() +
  labs(x="age", y="mortality rate", color = "")

 # # # # # #
# Problem 4 # 
 # # # # # #

# Rates of mortality and fair/poor health decline with
# family income. The same general pattern holds
# for education as well, although individuals with 
# post-graduate education do not appear to be in worse 
# health than college graduates.

# Generate factor variable for incfam
nhis$incfam.f <- factor(nhis$incfam, # create factor variable for plot
                        levels = c(11,12,22,23,24),
                        labels = c("<35k","35-49k","50-74k","75-99k",">100k")) 

# Mortality by income
ggplot(data=subset(nhis,!is.na(incfam.f)), mapping=aes(x=incfam.f, y=mort)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Health by income
ggplot(data=subset(nhis,!is.na(incfam.f)), mapping=aes(x=incfam.f, y=fpoor)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Generate education level -- the numeric values are clear enough here, 
nhis$edlev <- case_when((nhis$edyrs<12)                   ~ 1, # numberic variable
                         (nhis$edyrs==12)                 ~ 2,
                         (nhis$edyrs>=13 & nhis$edyrs<15) ~ 3,
                         (nhis$edyrs==16)                 ~ 4,
                         (nhis$edyrs>=17 & nhis$edyrs<20) ~ 5)
nhis$edlev.f <- factor(nhis$edlev, # convert to factor variable with value labels
                       levels = c(1,2,3,4,5),
                       labels = c("<12","12","13-15","16",">16")) 
prop.table(table(nhis$edlev.f))

# Mortality by education
ggplot(data=subset(nhis,!is.na(edlev.f)), mapping=aes(x=edlev.f, y=mort)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

# Health by education
ggplot(data=subset(nhis,!is.na(edlev.f)), mapping=aes(x=edlev.f, y=fpoor)) +
  stat_summary(fun.data=mean_sdl, geom="bar")

 # # # # # #
# Problem 5 # 
 # # # # # #

# Because age and education have non-linear relation-
# ships with health, I include a series of dummy 
# variables for categories. I use the education cate-
# gories from above, and 10-year age intervals.

# For both outcomes and for all three models, the
# results show that mortality and fair/poor health 
# decline with income, decline with education, and 
# rise with age. One surprising result is that 
# conditional on the socioeconomic variables, racial
# gaps in mortality are small and insignificant.
# There are larger racial gaps in fair/poor health.
# Another surprising result is that Hispanics have
# low mortality risk (conditional on the other
# covariates).

# The linear probability results are similar to the
# probit and logit average marginal effects, although 
# the similarity is much stronger for fair/poor health
# than for mortality. You did not need to comment
# on the reason in your response, but the larger 
# difference in the case of mortality is probably
# due to the fact that mortality risk is exceptionally
# low across much of the age distribution, so that
# the marginal effect is calculated in the flatter part 
# of the CDF.

# Generate a factor variables for age (in decades)
# With a factor variable, R automatically converts it into a series of dummies
nhis$agecat.f <- factor(floor(nhis$age/10)*10)

# Mortality analyses
library(estimatr)
lm_robust(mort ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis)
probit_model <- glm(mort ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, family=binomial(link="probit"))
summary(probit_model)
logit_model <- glm(mort ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, family=binomial(link="logit"))
summary(logit_model)
# Marginal effects for the probit and logit models
library(mfx)
probitmfx(mort ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, atmean = TRUE, robust = TRUE)
logitmfx(mort ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, atmean = TRUE, robust = TRUE)

# Fair/poor health analyses
lm_robust(fpoor ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis)
probit_model <- glm(fpoor ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, family=binomial(link="probit"))
summary(probit_model)
logit_model <- glm(fpoor ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, family=binomial(link="logit"))
summary(logit_model)
# Marginal effects for the probit and logit models
library(mfx)
probitmfx(fpoor ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, atmean = TRUE, robust = TRUE)
logitmfx(fpoor ~ incfam.f + edlev.f + agecat.f + black + hisp + asian + other, data = nhis, atmean = TRUE, robust = TRUE)

 # # # # # #
# Problem 6 # 
 # # # # # #

# I used the logit model for this test.

# It is possible to use multcomp::glht() for coefficients that  
# were estimated for the categories of factor variables, 
# but it would be easier to work with dummy variables. For 
# pedagogical purposes, I will generate income and education
# category dummies, and then I will re-run the model.

prop.table(table(nhis$incfam)) 
nhis$inc_35_50 <- ifelse(nhis$incfam==12,1,0)
nhis$inc_50_75 <- ifelse(nhis$incfam==22,1,0)
nhis$inc_75_100 <- ifelse(nhis$incfam==23,1,0)
nhis$inc_gt_100 <- ifelse(nhis$incfam==24,1,0)

prop.table(table(nhis$edyrs)) 
nhis$ed_12 <- ifelse(nhis$edyrs==12,1,0)
nhis$ed_13_15 <- ifelse(nhis$edyrs>12 & nhis$edyrs<16,1,0)
nhis$ed_16 <- ifelse(nhis$edyrs==16,1,0)
nhis$ed_gt16 <- ifelse(nhis$edyrs>16,1,0)

logit_model <- glm(mort ~ inc_35_50 + inc_50_75 + inc_75_100 + inc_gt_100 +
                           ed_12 + ed_13_15 + ed_16 + ed_gt16 +
                           agecat.f + black + hisp + asian + other, data = nhis, family=binomial(link="logit"))

# The difference in log odds between Groups A and B
# is given by:

summary(logit_model)
library(multcomp)
summary(glht(logit_model, linfct = c("asian + ed_12 - black - ed_16 - inc_gt_100 = 0")))

# Since this is positive, we conclude that poorer, less-
# educated Asian group have higher mortality risk than
# richer, more-educated Black group. If we exponentiate
# this difference, we get:

exp(.7051)

# which implies that the odds of dying are twice as high
# for the poorer, less-educated, Asian group. You did not
# need to state this quantity in your answer.

# It's likely that this model is not the best for 
# testing differences between these groups. It would
# be better to include interactions of race and income.

 # # # # # #
# Problem 7 # 
 # # # # # #

# We probably should not interpret these results as causal.
# One problem is that there are many confounding variables
# that we do not observe but may jointly determine health
# and income, for instance place of birth. Another problem
# is that there may be reverse causality, i.e. health may
# affect income.

 # # # # # #
# Problem 8 # 
 # # # # # #

# I used the logit model again, and I exponentiated the 
# coefficients for interpretability. I control for
# insurance status, smoking status, exercise, bacon
# consumption, and obesity. To keep the samples the same
# in the regressions with and without the additional
# control variables, I run the long regression
# and the short regression on the same sample, which
# required subsetting the data first. This was not required.

# Mortality results:
# Insurance status, was not significantly associated with mortality.
# Smoking, exercise, drinking, and obesity were highly associated with 
# mortality: ever smoking raised the odds of death by 73%, weekly 
# exercise reduced the odds by 39%, ever binge drinking raised
# the odds by 23%, and obesity raised the odds by 18%. These patterns 
# explain part of the socioeconomic gradient in health. After 
# controlling for these variables, the odds ratio on the highest 
# income category rose from 0.44 to 0.51 and that on the >16 years of 
# education dummy rose from 0.65 to 0.84. Thus, health behavior
# explains a larger share of the education-mortality relationship
# than the income-mortality relationship.

# Fair/poor health results:
# Uninsurance, smoking, and obesity were positively associated
# with fair/poor health. Exercise was negatively associated with
# fair/poor health. Binge drinking had no association. These 
# patterns again explain part of the socioeconomic gradient 
# in health. The odds ratio on the highest income category
# rises from 0.21 to 0.25, while that on >16 years of education
# rises from 0.26 to 0.36.

# Recode behavior variables as 0/1 dummies
table(nhis$uninsured)
nhis$uninsured <- 2-nhis$uninsured
table(nhis$smokev)
nhis$smokev <- ifelse(nhis$smokev>2, NA, nhis$smokev) # set missing codes to NA
nhis$smokev <- nhis$smokev-1 # turn into 0/1 dummy
table(nhis$vig10fwk)
nhis$vig10fwk <- ifelse(nhis$vig10fwk > 0, 1, 0)
table(nhis$alc5upyr)
nhis$alc5upyr <- ifelse(nhis$alc5upyr > 0, 1, 0)
nhis$obese <- ifelse(nhis$bmi >= 30, 1, 0)

# Mortality analyses: start by subsetting data to non-missing obs, then run regressions
nhis_subset <- na.omit(nhis[c("mort","incfam.f","edlev.f","agecat.f",
                              "white","black","hisp",
                              "uninsured","smokev","vig10fwk","alc5upyr","obese")])
logitor(mort ~ incfam.f + edlev.f + agecat.f + white + black + hisp, 
        data = nhis_subset, robust = TRUE)
logitor(mort ~ incfam.f + edlev.f + agecat.f + white + black + hisp +
                uninsured + smokev + vig10fwk + alc5upyr + obese, 
        data = nhis_subset, robust = TRUE)

# Fair/poor analyses
nhis_subset <- na.omit(nhis[c("fpoor","incfam.f","edlev.f","agecat.f",
                              "white","black","hisp",
                              "uninsured","smokev","vig10fwk","alc5upyr","obese")])
logitor(fpoor ~ incfam.f + edlev.f + agecat.f + white + black + hisp, 
        data = nhis_subset, robust = TRUE)
logitor(fpoor ~ incfam.f + edlev.f + agecat.f + white + black + hisp +
                uninsured + smokev + vig10fwk + alc5upyr + obese, 
        data = nhis_subset, robust = TRUE)

