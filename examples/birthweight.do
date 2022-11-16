*This example studies the relationship between low birth weight and test scores
use "https://github.com/tvogl/econ121/raw/main/data/nlsy_deming.dta",clear

*Decribe the data. Test scores go from 0 to 100, with a mean in the 40s.
d
sum

*Birth weight is in logs, which is a little complicated to interpret
*Let's convert to ounces
gen bw = exp(lnbw)

*Have a look at the summary statistics
sum bw,d

*Let's give ourselves a sense of how birth weight relates to the
*composite test score by plotting mean test scores by birthweight.
*I use "preserve" and "restore" so that I can come back to the
*original dataset. I also create a binned version of the birthweight
*variable to make the plot less noisy.
preserve
gen bw_bin = floor(bw/10)*10 //now 100 means "100-109.9" and so forth
recode bw_bin (0/70=70) (140/220=140) //compress the long tails into big bins
collapse comp_score_11to14,by(bw_bin)
twoway connected comp_score_11to14 bw_bin
restore

*There is a strong relationship! But how much of this is 
*due to family characteristics? Let's generate separate
*plots for mothers with <12, 12, and >12 years of schooling.
preserve
gen bw_bin = floor(bw/10)*10 //now 100 means "100-109.9" and so forth
recode bw_bin (0/70=70) (140/220=140) //compress the long tails into big bins
recode momed (1/11=1) (12=2) (13/20=3),gen(momedlevel) //mom's ed level
collapse comp_score_11to14,by(bw_bin momedlevel)
twoway (connected comp_score_11to14 bw_bin if momedlevel==1) ///
       (connected comp_score_11to14 bw_bin if momedlevel==2) ///
	   (connected comp_score_11to14 bw_bin if momedlevel==3) ///
	   , legend(label(1 "<HS") label(2 "HS") label(3 ">HS") rows(1))
restore
*Maternal education is clearly associated with test scores. At the same time,
*these plots don't look that much flatter than the full sample plot above.
*How much of the relationship is attributable to maternal characteristics
*rather than child health per se? We need to use fixed effects to find out.

*For simplicity, let's generate a very low birth weight indicator, 
*based on the 53 ounce threshold.
gen vlow_bw = (bw<53) if bw<.

*For this binary categorization, a bar graph may be a convenient way
*to visualize the data.
graph bar comp_score_11to14,over(vlow_bw)

*Let's look at the structure of the panel data for a few key variables
sort mom_id /*sort so that siblings are next to each other in the dataset*/
browse mom_id hispanic black momed male firstborn lnbw comp_score_11to14

*OLS with robust standard errors
reg comp_score_11to14 vlow_bw,robust

*OLS with clustered standard errors
reg comp_score_11to14 vlow_bw,cluster(mom_id)

*Random effects
*The estimated coefficient changes a lot,
*which suggest that the between-family variation
*and the within-family variation lead to different
*coefficients. Most researchers would conclude 
*that we should rely on fixed effects.
xtreg comp_score_11to14 vlow_bw,i(mom_id) re

*Random effects with robust standard errors
*Note that in the xt commands, Stata automatically 
*clusters at the group level.
xtreg comp_score_11to14 vlow_bw,i(mom_id) re robust

*Fixed effects
*Here, the estimated coefficient shrinks even
*more, consistent with upward bias from between-family
*variation.
xtreg comp_score_11to14 vlow_bw,i(mom_id) fe

*Fixed effects with robust standard errors
*xtreg automatically clusters at the group level.
xtreg comp_score_11to14 vlow_bw,i(mom_id) fe robust

*Let's try adding some control variables. We will add black,
*hispanic, and momed as examples of control variables that 
*DO NOT vary within family. We will add male and first born
*as examples of covariates that DO vary within family. Since
*adding control variables changes the sample size, we will 
*rerun the models without control variables in the smaller
*sample.

*OLS with and without control variables. The "if e(sample)"
*in lines 2-3 restricts the sample to the one used in the
*previous regression. Consistent with the comparison of FE and OLS,
*adding the family-level control variables reduces the estimates a lot.
*Adding just the individual-level control variables doesn't do much.
reg comp_score_11to14 vlow_bw hispanic black momed male firstborn,cluster(mom_id)
reg comp_score_11to14 vlow_bw male firstborn if e(sample),cluster(mom_id)
reg comp_score_11to14 vlow_bw if e(sample),cluster(mom_id)

*FE with and without control variables. The family-level control
*variables are dropped because they are collinear with the mother
*fixed effects. Again, the individual-level control variables
*don't much change the estimates on vlow_bw.
xtreg comp_score_11to14 vlow_bw hispanic black momed male firstborn,fe i(mom_id) robust
xtreg comp_score_11to14 vlow_bw male firstborn if e(sample),fe i(mom_id) robust
xtreg comp_score_11to14 vlow_bw if e(sample),fe i(mom_id) robust

