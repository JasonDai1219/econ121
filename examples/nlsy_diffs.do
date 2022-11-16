*******************************************
*ESTIMATING RACIAL DIFFERENCES IN EARNINGS*
*IN THE NAT'L LONGITUDINAL SURVEY OF YOUTH*
*******************************************

use "https://github.com/tvogl/econ121/raw/main/data/nlsy79.dta",clear

*list variables
describe

*show mean and sd of labor earnings
sum laborinc18

*more detailed summary with percentiles
*note that there are many zeros, which implies
*that we cannot take logarithms without first
*thinking about sample selection.
sum laborinc18,d

*we can also see this by plotting histograms by race
hist laborinc18,by(black) percent

*we will be interested in estimating differences
*between blacks and non-blacks.

*means by race. two methods:
bysort black: sum laborinc18
tab black,sum(laborinc18)

*these results give us all the information we need 
*to test for differences by race. we use the "display"
*command, "di" for short, to perform calculations.
*difference:
di 50798-31505
*t-statistic:
di (50798-31505)/sqrt(70856^2/4558 + 46907^2/2013)
*well above 1.96, so statistically significant by the usual standards.

*alternative ways to run this test are...
*t-test with unequal variances:
ttest laborinc18,by(black) unequal
*regression with heteroskedasticity-robust standard errors:
reg laborinc18 black,r
*the slight differences across the commands are due to
*differences in how they adjust for "degrees of freedom," 
*and possibly also to rounding errors in the computation. 
*note also that the standard devitations are VERY
*different. hence it is very important to allow for
*unequal variances (or equivalently, heteroskedasticity).

*it is actually uncommon to test for average differences
*in the level (rather than log) of earnings, including
*zeros from the non-employed. it would be much more
*typical to restrict to employed individuals. so let's
*restrict to people who worked for pay at least 1000 hours:
*equivalent to a part-time job of 20 hours per week for
*50 weeks.
sum hours18,d
keep if hours18>=1000 & hours18<. & laborinc18>0
*the second part (hours18<.) requires that hours18 is not missing.
*means of by race
tab black,sum(laborinc18)
*still an $18k difference, which amounts to a black/non-black
*ratio of:
di 73786/55632

*now let's look at log earnings.
gen loginc18 = ln(laborinc18)
tab black,sum(loginc18)
*difference:
di 10.851-10.616

*this difference in logs can by roughly interpreted as 
*a 23.5% difference in earnings, although this interpretation
*relies on calculus [dln(y)/dx], so it is really for
*continuous X variables rather than binary ones. if we 
*were being really careful, we would exponentiate the 
*difference of logs:
di exp(10.851-10.616)
*which implies a 26.5% difference in geometric means. this
*proportional difference is closer to the 32.6% difference
*in arithmetic means that we saw above. they are different
*because the mean of the log is different from the log of 
*the means.

*the t-statistic:
di (10.851-10.616)/sqrt(.867^2/3015 + .849^2/1078)
*well above 1.96, so statistically significant by the usual standards.

*alternative ways to run this test are...
*t-test with unequal variances:
ttest loginc18,by(black) unequal
*regression with heteroskedasticity-robust standard errors:
reg loginc18 black,r
*same results. that is to say, a regression on a "dummy variable"
*for black leads to the same results as a difference of means

