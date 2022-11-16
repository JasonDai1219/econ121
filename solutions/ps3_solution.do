//This do file presents solutions to ECON 121 Problem Set 3.

//Note: You may have noticed that the dataset includes a
//      primary sampling unit identifier and a sampling 
//      weight. It would be reasonable to cluster standard
//      errors at the PSU level and weight using sampling
//      weights, but these details were not the focus of the
//      problem set and are therefore not required.

/////////////
//Problem 2//
/////////////

//Summary statistics appear below. 14 percent of the sample 
//reports being in fair or poor health, and 13 percent died 
//died before 2019. The sample includes adults 25
//and up, with a median age of 49. (The mean age
//is less meaningful because age was top-coded at 85. You
//did not need to notice this.) 56 percent of the sample
//is female, perhaps surprisingly. This gender imbalance
//has two sources. First, men and women responded to the 
//survey at different rates, so the gender imbalance shrinks
//when we use the sampling weights. Second, men die at 
//higher rates than women, so the gender imbalance grows
//with age.

use "https://github.com/tvogl/econ121/raw/main/data/nhis2010.dta",clear
d

tab health
gen fpoor = (health>3) if health<.
tab fpoor

tab mort

tab age
sum age,d

tab sex
tab sex [aw=sampweight]
tab age sex [aw=sampweight],row nofreq

sum

/////////////
//Problem 3//
/////////////

//5-year mortality is higher for people with fair/poor health
//than for people with good/very good/excellent health. Thus,
//self-reported health status is predictive of mortality. In
//both groups, 5-year mortality rises non-linearly with age.

preserve //so we can return to the original dataset later
collapse mort,by(age fpoor)
twoway (connected mort age if fpoor==0,lcolor(black) mcolor(black)) ///
       (connected mort age if fpoor==1,lcolor(red) mcolor(red)) ///
	   ,legend(order(1 "Good/Very Good/Excellent" 2 "Fair/Poor")) ///
	    ytitle("Predicted probability of dying before 2019") ///
		xtitle("Age")
restore //return to original dataset

/////////////
//Problem 4//
/////////////

//Rates of mortality and fair/poor health decline with
//family income. The same general pattern holds
//for education as well, although individuals with 
//post-graduate education do not appear to be in worse 
//health than college graduates.

//Graphs
graph bar mort,over(incfam)
graph bar fpoor,over(incfam)

//Generate education category variable
recode edyrs (1/11=1) (12=2) (13/15=3) (16=4) (17/19=5),gen(edlev)
label define edlev_lbl 1 "<12 yrs" 2 "12 yrs" 3 "13-15 yrs" 4 "16 yrs" 5 ">16 yrs"
label values edlev edlev_lbl
//Graphs
graph bar mort,over(edlev)
graph bar fpoor,over(edlev)

/////////////
//Problem 5//
/////////////

//Because age and education have non-linear relation-
//ships with health, I include a series of dummy 
//variables for categories. I use the education cate-
//gories from above, and 10-year age intervals.

//For both outcomes and for all three models, the
//results show that mortality and fair/poor health 
//decline with income, decline with education, and 
//rise with age. One surprising result is that 
//conditional on the socioeconomic variables, racial
//gaps in mortality are small and insignificant.
//There are larger racial gaps in fair/poor health.
//Another surprising result is that Hispanics have
//low mortality risk (conditional on the other
//covariates).

//The linear probability results are similar to the
//probit and logit average marginal effects, although 
//the similarity is much stronger for fair/poor health
//than for mortality. You did not need to comment
//on the reason in your response, but the larger 
//difference in the case of mortality is probably
//due to the fact that mortality risk is exceptionally
//low across much of the age distribution, so that
//the marginal effect is calculated in the flatter part 
//of the CDF.

//Generate age categories variable (in decades)
gen agecat = floor(age/10)*10

//Mortality analyses
reg mort i.incfam i.edlev i.agecat black hisp asian other,r
probit mort i.incfam i.edlev i.agecat black hisp asian other,r
margins,dydx(*)
logit mort i.incfam i.edlev i.agecat black hisp asian other,r
margins,dydx(*)

//Fair/poor health analyses
reg fpoor i.incfam i.edlev i.agecat black hisp asian other,r
probit fpoor i.incfam i.edlev i.agecat black hisp asian other,r
margins,dydx(*)
logit fpoor i.incfam i.edlev i.agecat black hisp asian other,r
margins,dydx(*)

/////////////
//Problem 6//
/////////////

//I used the logit model above for this test.

//It is possible to use lincom for coefficients that were 
//estimated using the "i." expansion of categorical variables,
//but it would be easier to work with dummy variables, so for 
//pedagogical purposes, I will generate income and education
//category dummies, and then I will re-run the model.

tab incfam,miss
tab incfam,nol
gen inc_35_50 = (incfam==12) if incfam<.
gen inc_50_75 = (incfam==22) if incfam<.
gen inc_75_100 = (incfam==23) if incfam<.
gen inc_gt_100 = (incfam==24) if incfam<.

tab edlev,miss
gen ed_12 = (edlev==2) if edlev<.
gen ed_13_15 = (edlev==3) if edlev<.
gen ed_16 = (edlev==4) if edlev<.
gen ed_gt_16 = (edlev==5) if edlev<.

logit mort inc_* ed_* i.agecat black hisp asian other,r

//The difference in log odds between Groups A and B
//is given by:

lincom asian + ed_12 - (black + ed_16 + inc_gt_100)

//Since this is positive, we conclude that poorer, less-
//educated Asian group have higher mortality risk than
//richer, more-educated Black group. If we exponentiate
//this difference, we get:
di exp(.705062)

//which implies that the odds of dying are twice as high
//for the poorer, less-educated, Asian group. You did not
//need to state this quantity in your answer.

//It's likely that this model is not the best for 
//testing differences between these groups. It would
//be better to include interactions of race and income.

/////////////
//Problem 7//
/////////////

//We probably should not interpret these results as causal.
//One problem is that there are many confounding variables
//that we do not observe but may jointly determine health
//and income, for instance place of birth. Another problem
//is that there may be reverse causality, i.e. health may
//affect income.

/////////////
//Problem 8//
/////////////

//I used the logit model again, and I exponentiated the 
//coefficients for interpretability. I control for
//insurance status, smoking status, exercise, bacon
//consumption, and obesity. To keep the samples the same
//in the regressions with and without the additional
//control variables, I run the "long" regression first
//and then run the "short" regression with "if e(sample)"
//to use the same sample as the previous regression. This
//detail was not required.

//Mortality results:
//Insurance status, was not significantly associated with mortality.
//Smoking, exercise, drinking, and obesity were highly associated with 
//mortality: ever smoking raised the odds of death by 73%, weekly 
//exercise reduced the odds by 39%, ever binge drinking raised
//the odds by 23%, and obesity raised the odds by 18%. These patterns 
//explain part of the socioeconomic gradient in health. After 
//controlling for these variables, the odds ratio on the highest 
//income category rose from 0.44 to 0.51 and that on the >16 years of 
//education dummy rose from 0.65 to 0.84. Thus, health behavior
//explains a larger share of the education-mortality relationship
//than the income-mortality relationship.

//Fair/poor health results:
//Uninsurance, smoking, and obesity were positively associated
//with fair/poor health. Exercise was negatively associated with
//fair/poor health. Binge drinking had no association. These 
//patterns again explain part of the socioeconomic gradient 
//in health. The odds ratio on the highest income category
//rises from 0.21 to 0.25, while that on >16 years of education
//rises from 0.26 to 0.36.

//Recode behavior variables as 0/1 dummies
sum uninsured smokev vig10fwk alc5upyr bmi
tab uninsured
replace uninsured = 2-uninsured
tab smokev
replace smokev = smokev-1 if smokev<=2
tab vig10fwk
replace vig10fwk = (vig10fwk>0) if vig10fwk<. //any exercise
tab alc5upyr
replace alc5upyr = (alc5upyr>0) if alc5upyr<. //ever binge drink
gen obese = (bmi>=30) if bmi<.

//Mortality analyses
logit mort i.incfam i.edlev i.agecat black hisp asian other ///
      uninsured smokev vig10fwk alc5upyr obese,r or
logit mort i.incfam i.edlev i.agecat black hisp asian other if e(sample),r or

//Fair/poor health analyses
logit fpoor i.incfam i.edlev i.agecat black hisp asian other ///
      uninsured smokev vig10fwk alc5upyr obese,r or
logit fpoor i.incfam i.edlev i.agecat black hisp asian other if e(sample),r or
