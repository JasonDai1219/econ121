** Question 1 **
** Open dataset
use  "https://github.com/tvogl/econ121/raw/main/data/project_star",clear
** Standardize: Subtract the mean and divide by the standard deviation.
** You could have also copy and pasted the numbers the sum command.
sum stest
gen stest_std = (stest - r(mean))/r(sd)
** Because schools are sampled, we need to cluster standard errors at
** the school level. You would have also gotten full credit if you 
** clustered at the classroom level.
reg stest_std smale swhite sfree,cluster(schoolid)

** Question 2 **
** Boys score .20 SD less than girls, and free lunch students score .48 SD
** higher than non-free lunch students, on average. Both of these results
** have p-values less than .05, so they are statisticially different from 
** zero at conventional significance levels. In addition, white students 
** score .14 SD more than non-white students, but this difference is not
** statistically significant. (If you had clustered at the classroom level
** or neglected to cluster, you would have found a significant difference 
** on gender.)

** Question 3 **
gen sbelow = (stest<400) if stest<.
logistic sbelow smale swhite sfree,cluster(schoolid)
margins,dydx(*)

** Question 4 **
** The largest risk factor is free lunch eligibility. Holding constant gender
** and race, free lunch eligibility is associated with a tripling of the odds
** of being below grade level, or a 7 percentage point higher probability of 
** being below grade level.

** Question 5 **
** First we generate dummy variables for the two treatments.
tab cltype
gen small = (cltype==1)
gen aide = (cltype==3)
** Now we run the regression. We need to include school dummies because 
** randomization was stratified by school. I have continued to cluster
** standard errors at the school level, but you could have reasonably 
** clustered at the classroom level on the basis of the design, which 
** randomized at the classroom level.
reg stest_std small aide i.schoolid,cluster(schoolid)

** Question 6 **
** The results imply that being in a small class (relative to a regular size
** class) raises test scores by 0.21 standard deviations, roughly the same
** size as the boy-girl gap. This difference is statistically significant
** at the 0.001 level, and because of the randomized design, it can be 
** interpreted as the causal effect of being in a small class. In contrast,
** the results suggest that adding a teacher's aide to a regular size class 
** has no effect on test scores.

** Question 7 **
** Generate dummy for inexperienced teacher.
gen inexp = (texp==0) if texp<.
** Generate interactions with treatment dummies.
gen smallXinexp = small*inexp
gen aideXinexp = aide*inexp
** Run regression
reg stest_std small aide inexp smallXinexp aideXinexp i.schoolid,cluster(schoolid)

** Question 8 **
nlcom (_b[smallXinexp]+_b[small])/_b[small]

** Question 9 **
twoway (kdensity stest_std if cltype==2,bw(.1) lcolor(gs10)) ///
       (kdensity stest_std if cltype==1,bw(.1) lcolor(red)) ///
	   ,legend(label(1 "Regular") label(2 "Small") rows(1))
	   
** Question 10 **
** To estimate the effect of adding one student consistently, we rely
** on two-stage least squares, using the experimental assignment to the
** "small" class type as the instrument for the number of students.
** Because assignment to the "small" class type was stratified by school,
** we include school indicators as exogenous controls. As above, you
** could have clustered SEs at the class or school level.
ivregress 2sls stest_std (csize = small) i.schoolid,cluster(schoolid)

** Question 11 **
** Open the dataset
use "https://github.com/tvogl/econ121/raw/main/data/maimonides.dta",clear
** Run the regression. Here it is again appropriate to cluster
** at the school level, since sampling is by school.
reg avg_verb disadv,cluster(school_id)

** Question 12 **
** A 1 percentage point increase in the disadvantaged share is associated
** with a .33 point decrease in the average verbal score. Weighting by the 
** number of students would shrink the standard error while not substantially
** changing the coefficient if the individual relationship between student 
** disadvantage and student performance were (i) homogeneous and (ii) homoskedastic,
** and if (iii) the performance of a student did not depend on the composition of
** her classmates. You got full credit if you answered just (i) and (ii).

** Question 13 **
gen x = grade_size - 40
** Local polynomial regressions for class size.
twoway (lpoly class_size x if x>0,degree(1) bw(10) lcolor(black)) ///
       (lpoly class_size x if x<=0,degree(1) bw(10) lcolor(black)) ///
	   ,legend(off)
** Local polynomial regressions for average verbal score.
twoway (lpoly avg_verb x if x>0,degree(1) bw(10) lcolor(black)) ///
       (lpoly avg_verb x if x<=0,degree(1) bw(10) lcolor(black)) ///
	   ,legend(off)
	   
** Question 14 **
** In the first graph, class size discontinuously declines at the 40-
** student cutoff, suggesting that Maimonides' rule reduces class size
** when it binds in grades close to the cutoff. This is the first-stage 
** relationship.
**
** In the second graph, test scores discontinuously increase at the 
** cutoff, suggesting that Maimonides' rule raises average test scores
** when it binds in grades close to the cutoff. This is the reduced form 
** relationship.

** Question 15 **
keep if x>=-10 & x<=10 // keep local polynomial sample
gen D = (x>0) // dummy for when maimonides rule binds
gen Dx = D*x // interaction
** First stage regression
reg class_size D x Dx,cluster(school_id)
** Reduced form regression
reg avg_verb D x Dx,cluster(school_id)

** Question 16 **
** The regression results imply that crossing the 40-student cutoff
** decreases average class size by 10.9 students and raises average
** scores by 4.9 points. These results have a causal interpretation if
**   (i) potential outcomes are continuous in x at 0
** or
**   (ii) schools and parents have imperfect control over grade size.
** You only needed to write one of these assumptions. We can assess the
** assumption(s) validity by
**   (i) testing for a discontinuity in a predetermined characteristic
** or
**   (ii) evaluating the continuity of the histogram of x.

** Question 17 **
reg disadv D x Dx,cluster(school_id)
** or
hist x,discrete xline(.5)

** Question 18 **
** We divide the reduced form effect by the first stage effect:
** 4.86 / (-10.88) = 0.45.
** Thus, a 1-student increase decreases average tests scores by 0.45 points. 
** Because some schools may reduce class sizes before the rule binds, it
** is a fuzzy RD. The average effect is specific to compliers near the cutoff.
** Here, compliers are schools with roughly 40 4th-graders that add classes 
** only if the rule forces them to do so. In addition to the assumption from 
** Question 16, we also need to assume that crossing the cutoff only affects
** test scores through its effect on class size (exclusion restriction) and
** that crossing the cutoff does not increase class size in any school (monotonicity).

** Question 19 **
ivregress 2sls avg_verb (class_size = D) x Dx,cluster(school_id) // iv regression
use "https://github.com/tvogl/econ121/raw/main/data/maimonides.dta",clear // re-open full dataset
reg avg_verb class_size,cluster(school_id) // ols regression

** Question 20 **
** The two-stage least squares result suggests a negative .45 point effect of 
** an extra student, consistent with the calculation in Question 18. The OLS
** result suggests a positive .13 point effect. The difference implies
** an upward bias in the OLS result. One possible reason for an upward bias is
** that school administrators may decrease class size in schools with many
** low-performing students.

	   


	   





