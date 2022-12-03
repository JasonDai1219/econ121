library(haven)
library(estimatr)
library(mfx)
library(car)
library(KernSmooth)
library(ggplot2)
library(dplyr)
library(AER)

## Question 1 ##
## Load the dataset
star <- read_dta("https://github.com/tvogl/econ121/raw/main/data/project_star.dta")
## Standardize: Subtract the mean and divide by the standard deviation.
star$stest_std <- (star$stest - mean(star$stest, na.rm = TRUE))/sd(star$stest, na.rm = TRUE)
## Because schools are sampled, we need to cluster standard errors at
## the school level. You would have also gotten full credit if you 
## clustered at the classroom level.
lm_robust(stest_std ~ smale + swhite + sfree, data = star, clusters = schoolid)

## Question 2 ##
## Boys score .20 SD less than girls, and free lunch students score .48 SD
## higher than non-free lunch students, on average. Both of these results
## have p-values less than .05, so they are statisticially different from 
## zero at conventional significance levels. In addition, white students 
## score .14 SD more than non-white students, but this difference is not
## statistically significant. (If you had clustered at the classroom level
## or neglected to cluster, you would have found a significant difference 
## on gender.)

## Question 3 ##
star$sbelow <- ifelse(star$stest < 400, 1, 0)
logitor(sbelow ~ smale + swhite + sfree, data = star, clustervar1 = "schoolid")
logitmfx(sbelow ~ smale + swhite + sfree, data = star, clustervar1 = "schoolid")

## Question 4 ##
## The largest risk factor is free lunch eligibility. Holding constant gender
## and race, free lunch eligibility is associated with a tripling of the odds
## of being below grade level, or a 7 percentage point higher probability of 
## being below grade level.

## Question 5 ##
## First we generate dummy variables for the two treatments.
star$small <- ifelse(star$cltype==1,1,0)
star$aide <- ifelse(star$cltype==3,1,0)
## Now we run the regression. We need to include school dummies because 
## randomization was stratified by school. I have continued to cluster
## standard errors at the school level, but you could have reasonably 
## clustered at the classroom level on the basis of the design, which 
## randomized at the classroom level.
star$schoolid.f <- factor(star$schoolid) # factor variable version of schoolid
lm_robust(stest_std ~ small + aide + schoolid.f, data = star, clusters = schoolid)

## Question 6 ##
## The results imply that being in a small class (relative to a regular size
## class) raises test scores by 0.21 standard deviations, roughly the same
## size as the boy-girl gap. This difference is statistically significant
## at the 0.001 level, and because of the randomized design, it can be 
## interpreted as the causal effect of being in a small class. In contrast,
## the results suggest that adding a teacher's aide to a regular size class 
## has no effect on test scores.

## Question 7 ##
## Generate dummy for inexperienced teacher.
star$inexp <- ifelse(star$texp==0,1,0)
## Generate interactions with treatment dummies.
star$smallXinexp <- star$small*star$inexp
star$aideXinexp <- star$aide*star$inexp
## Run regression
interact_model <- lm_robust(stest_std ~ small + aide + inexp + smallXinexp + aideXinexp + schoolid.f, data = star, clusters = schoolid)
summary(interact_model)

## Question 8 ##
deltaMethod(interact_model,"(smallXinexp + small) / small",rhs=1)

## Question 9 ##
star$cltype.f <- factor(star$cltype, levels=c(1,2,3), labels=c("Small", "Regular", "Regular + aide"))
star %>%
  filter(cltype == 1 | cltype == 2) %>%
  ggplot(aes(x=stest_std, group=cltype, fill=cltype.f)) +
    geom_density(bw=.1,alpha=.4) +
    theme(legend.title = element_blank())
	   
## Question 10 ##
## To estimate the effect of adding one student consistently, we rely
## on two-stage least squares, using the experimental assignment to the
## "small" class type as the instrument for the number of students.
## Because assignment to the "small" class type was stratified by school,
## we include school indicators as exogenous controls. As above, you
## could have clustered SEs at the class or school level.
ivmodel1 <- ivreg(stest_std ~ csize + schoolid.f | small + schoolid.f, data = star)
coeftest(ivmodel1, vcov = vcovHC(ivmodel1, cluster="schoolid"))

## Question 11 ##
## Load the dataset
maimonides <- read_dta("https://github.com/tvogl/econ121/raw/main/data/maimonides.dta")
## Run the regression. Here it is again appropriate to cluster
## at the school level, since sampling is by school.
lm_robust(avg_verb ~ disadv, data = maimonides, clusters = school_id)

## Question 12 ##
## A 1 percentage point increase in the disadvantaged share is associated
## with a .33 point decrease in the average verbal score. Weighting by the 
## number of students would shrink the standard error while not substantially
## changing the coefficient if the individual relationship between student 
## disadvantage and student performance were (i) homogeneous and (ii) homoskedastic,
## and if (iii) the performance of a student did not depend on the composition of
## her classmates. You got full credit if you answered just (i) and (ii).

## Question 13 ##
maimonides$x <- maimonides$grade_size - 40
maimonides_below <- subset(maimonides, x < 0 & !is.na(maimonides$avg_verb))
maimonides_above <- subset(maimonides, x >= 0 & !is.na(maimonides$avg_verb))
## Local polynomial regressions for class size.
yhat_below <- data.frame(locpoly(x=maimonides_below$x, y=maimonides_below$class_size, bandwidth=10, degree=1))
yhat_above <- data.frame(locpoly(x=maimonides_above$x, y=maimonides_above$class_size, bandwidth=10, degree=1))
ggplot() +
  geom_line(data = yhat_below, aes(x = x, y = y)) +
  geom_line(data = yhat_above, aes(x = x, y = y)) +
  labs(x="Distance from cutoff", y="Class size")
## Local polynomial regressions for average verbal score.
yhat_below <- data.frame(locpoly(x=maimonides_below$x, y=maimonides_below$avg_verb, bandwidth=10, degree=1))
yhat_above <- data.frame(locpoly(x=maimonides_above$x, y=maimonides_above$avg_verb, bandwidth=10, degree=1))
ggplot() +
  geom_line(data = yhat_below, aes(x = x, y = y)) +
  geom_line(data = yhat_above, aes(x = x, y = y)) +
  labs(x="Distance from cutoff", y="Average verbal score")
	   
## Question 14 ##
## In the first graph, class size discontinuously declines at the 40-
## student cutoff, suggesting that Maimonides' rule reduces class size
## when it binds in grades close to the cutoff. This is the first-stage 
## relationship.
##
## In the second graph, test scores discontinuously increase at the 
## cutoff, suggesting that Maimonides' rule raises average test scores
## when it binds in grades close to the cutoff. This is the reduced form 
## relationship.

## Question 15 ##
maimonides_local <- maimonides %>% 
                      subset(x>=-10 & x<=10) %>% ## keep local polynomial sample
                      mutate(D = ifelse(x>0,1,0), Dx = D*x)
## First stage regression
lm_robust(class_size ~ D + x + Dx, data = maimonides_local, clusters = school_id)
## Reduced form regression
lm_robust(avg_verb ~ D + x + Dx, data = maimonides_local, clusters = school_id)

## Question 16 ##
## The regression results imply that crossing the 40-student cutoff
## decreases average class size by 10.9 students and raises average
## scores by 4.9 points. These results have a causal interpretation if
##   (i) potential outcomes are continuous in x at 0
## or
##   (ii) schools and parents have imperfect control over grade size.
## You only needed to write one of these assumptions. We can assess the
## assumption(s) validity by
##   (i) testing for a discontinuity in a predetermined characteristic
## or
##   (ii) evaluating the continuity of the histogram of x.

## Question 17 ##
lm_robust(disadv ~ D + x + Dx, data = maimonides_local, clusters = school_id)
## or
ggplot(maimonides_local, aes(x=x)) + 
  geom_histogram(binwidth=1, boundary=-32.5) +
  geom_vline(xintercept = .5)

## Question 18 ##
## We divide the reduced form effect by the first stage effect:
## 4.86 / (-10.88) = 0.45.
## Thus, a 1-student increase decreases average tests scores by 0.45 points. 
## Because some schools may reduce class sizes before the rule binds, it
## is a fuzzy RD. The average effect is specific to compliers near the cutoff.
## Here, compliers are schools with roughly 40 4th-graders that add classes 
## only if the rule forces them to do so. In addition to the assumption from 
## Question 16, we also need to assume that crossing the cutoff only affects
## test scores through its effect on class size (exclusion restriction) and
## that crossing the cutoff does not increase class size in any school (monotonicity).

## Question 19 ##
ivmodel2 <- ivreg(avg_verb ~ class_size + x + Dx | D + x + Dx, data = maimonides_local)
coeftest(ivmodel2, vcov = vcovHC(ivmodel2, cluster="school_id"))

lm_robust(avg_verb ~ class_size, data = maimonides, clusters = school_id)

## Question 20 ##
## The two-stage least squares result suggests a negative .45 point effect of 
## an extra student, consistent with the calculation in Question 18. The OLS
## result suggests a positive .13 point effect. The difference implies
## an upward bias in the OLS result. One possible reason for an upward bias is
## that school administrators may decrease class size in schools with many
## low-performing students.

	   


	   





