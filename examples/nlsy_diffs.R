# uncomment if these packages are not installed
# install.packages(c('tidyverse','estimatr'))

###########################################
#ESTIMATING RACIAL DIFFERENCES IN EARNINGS#
#IN THE NAT'L LONGITUDINAL SURVEY OF YOUTH#
###########################################

# input Stata file (need haven library for importing Stata file)
library(haven)
nlsy79 <- read_dta("https://github.com/tvogl/econ121/raw/main/data/nlsy79.dta")

# data structure
library(tidyverse)
glimpse(nlsy79)

# mean and sd of labor earnings (na.rm=TRUE removes missing values from the calculation)
mean(nlsy79$laborinc18,na.rm=TRUE)
sd(nlsy79$laborinc18,na.rm=TRUE)

# more detailed summary with percentiles
# note that there are many zeros, which implies
# that we cannot take logarithms without first
# thinking about sample selection.
summary(nlsy79$laborinc18)

# we can see this better when we plot histograms by race (need ggplot2 library for graphing)
library(ggplot2)
ggplot(nlsy79, aes(x = laborinc18)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(black ~ .)

# we will estimate differences between blacks and non-blacks.

# means by race: this uses the dplyr library and the pipe operator %>%
nlsy79 %>% group_by(black) %>% summarize(mean=mean(laborinc18, na.rm = TRUE),
                                         sd=sd(laborinc18, na.rm = TRUE),
                                         n=sum(!is.na(laborinc18)))

# these results give us all the information we need to test for differences by race.
# difference
50798-31505
# t-statistic
(50798-31505)/sqrt(70856^2/4558 + 46907^2/2013)
# well above 1.96, so statistically significant by the usual standards.

# alternative ways to run this test are...
# t-test with unequal variances:
t.test(laborinc18 ~ black, data = nlsy79)
# regression with robust SEs (need to load estimatr library for lm_robust function)
library(estimatr)
lm_robust(laborinc18 ~ black,data = nlsy79)

# it is actually uncommon to test for average differences in the level 
# (rather than log) of earnings, including zeros from the non-employed. 
# it would be much more typical to restrict to employed individuals. so 
# let's restrict to people restrict to people who worked for pay at least 
# 1000 hours: equivalent to a part-time job of 20 hours per week for 50 weeks.
summary(nlsy79$hours18)
nlsy79_workers <- subset(nlsy79, hours18>=1000 & laborinc18>0)
summary(nlsy79_workers$hours18)
# means by race
nlsy79_workers %>% group_by(black) %>% summarize(mean=mean(laborinc18, na.rm = TRUE),
                                                 sd=sd(laborinc18, na.rm = TRUE),
                                                 n=sum(!is.na(laborinc18)))
# still an $19k difference, which amounts to a black/non-black ratio of:
73786/55632

# now let's look at log earnings. first remove zeros, then take logs.
nlsy79_workers <- subset(nlsy79_workers, laborinc18>0)
nlsy79_workers$loginc18 <- log(nlsy79_workers$laborinc18)
nlsy79_workers %>% group_by(black) %>% summarize(mean=sprintf("%0.3f",mean(loginc18, na.rm = TRUE)),
                                                 sd=sd(loginc18, na.rm = TRUE),
                                                 n=sum(!is.na(loginc18)))
# difference:
10.851-10.616

# this difference in logs can by roughly interpreted as 
# a 23.5% difference in earnings, although this interpretation
# relies on calculus [dln(y)/dx], so it is really for
# continuous X variables rather than binary ones. if we 
# were being really careful, we would exponentiate the 
# difference of logs:
exp(10.851-10.616)
# which implies a 26.5% difference in geometric means. this
# proportional difference is closer to the 32.6% difference
# in arithmetic means that we saw above. they are different
# because the mean of the log is different from the log of 
# the means.

# the t-statistic:
(10.851-10.616)/sqrt(.867^2/3015 + .849^2/1078)
# well above 1.96, so statistically significant by the usual standards.

# alternative ways to run this test are...
# t-test with unequal variances:
t.test(loginc18 ~ black, data = nlsy79_workers)
# regression with heteroskedasticity-robust standard errors:
lm_robust(loginc18 ~ black,data = nlsy79_workers)
# same results. that is to say, a regression on a "dummy variable"
# for black leads to the same results as a difference of means
# note that the t-statistic is very slightly different from what
# we computed "by hand." that's likely due to rounding errors.
