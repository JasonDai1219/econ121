*analysis of bacon consumption in the national health interview survey.
*open nhis dta, describe and summarize.
use "https://github.com/tvogl/econ121/raw/main/data/nhis2010.dta",clear

d
sum

*generate a variable that equals one if any sadness, zero otherwise.
*stata counts missing values as very high values, so let's drop observations with
*sadness missing.
tab asad,miss
tab asad,miss nolabel
drop if asad==.
gen anysad = (asad>0)

*generate some covariates.
*gender
tab sex
tab sex,nol
gen male = 2-sex

*marital status: dummy for marriage
tab marstat
tab marstat,nol
gen married = (marstat<20) if marstat<.

**COMPARING OLS, LOGIT, AND PROBIT

*estimate a linear probability model, a logit, and a probit.
*then generate predicted probabilities for each of these approaches.
*then compare the predicted probabilities
reg anysad edyrs age male black hisp asian other married,robust
predict p_ols

probit anysad edyrs age male black hisp asian other married,robust
predict p_probit

logit anysad edyrs age male black hisp asian other married,robust
predict p_logit

sum p_*
corr p_*

**MARGINAL EFFECTS

*now let's compute marginal effects at the means of the independent
*variables. one way to do this is to run a logit or a probit and
*to then use the mfx compute command:
logit anysad edyrs age male black hisp asian other married,robust
mfx compute

probit anysad edyrs age male black hisp asian other married,robust
mfx compute

*alternatively, we can use margins to compute avergae 
*marginal effects. here we do it for the probit above:
margins,dydx(*)

*we can also calculate marginal effects in the probit model
*using the dprobit command:
dprobit anysad edyrs age male black hisp asian other married,robust
*note that the marginal effects are the same as those computed
*by mfx compute. but they are NOT the same as those computed
*by margins. that's because mfx and dprobit evaluate marginal
*effects at the means of the independent variables, while
*margins evaluates the average marginal effect.

*stata does not provide a similar command for logits,
*but you can download one from the internet if you so
*desire. type "net search dlogit2" in stata; follow the 
*instructions.

**ODDS RATIOS

*finally, we can also estimate odds ratios in the logit setting:
logit anysad edyrs age male black hisp asian other married,robust or
logistic anysad edyrs age male black hisp asian other married,robust

*these are especially convenient for binary independent variables.
*for instance, 31% lower odds of reporting sadness than women.
