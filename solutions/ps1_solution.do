// ECON 121 Problem Set 1
// Solution do-file

//////////////
//Question 1//
//////////////

// I had no group.

//////////////
//Question 2//
//////////////

use "https://github.com/tvogl/econ121/raw/main/data/ssa_names.dta",clear

summarize

// The dataset spans 1880 to 2020. Name frequencies range from 5
// to nearly 100,000.

//////////////
//Question 3//
//////////////

// I chose Hilary and Hillary, just as in the example code. I wanted to know
// if naming frequencies changed after presidential elections involving the Clintons.

//////////////
//Question 4//
//////////////

// First, I show how I figured out the most common names. You did not need to
// do this in your code.
* preserve original dataset, so I can return to it after manipulating it
preserve
* collapse the data from name/sex/year frequencies to name/sex frequencies 
* across all years
collapse (sum) freq,by(name sex)
* sort the data by freq, in descending order
gsort -freq
* list the first 10 observations
list if _n<=10
* restore original dataset
restore

// We can see from above that the most common names are James for boys (5.2 mil)
// Mary for girls (4.1 mil). In our original name/sex/year dataset, let's
// summarize the frequencies of those names.
sum freq if name=="James" & sex=="M"
sum freq if name=="Mary" & sex=="F"
// Now compare with Hilary and Hillary.
sum freq if name=="Hilary" & sex=="F"
sum freq if name=="Hillary" & sex=="F"
// The mean frequency is much lower: 242 and 358, respectively, compared with 
// 29.2k and 36.8k for Mary and James.

// You got full credit if you simply compared the mean frequencies of the names.
// However, it's worth noting that there are fewer observations for Hilary and
// Hillary. That's because the dataset does not contain observations for years
// when no girl received these names. Our averages should include zeros for 
// those years. One very easy way to make this adjustment, without requiring
// any new Stata commands, is to adjust the means above by hand. Hilary has 99
// observations, relative to the 141 years in the dataset, so we can compute:
di (99/141)*242.2626
// Hillary has 80 observations, so we can compute:
di (80/141)*358.6125
// These averages include 0s for all years in which no girls were named Hilary
// or Hillary. Naturally, the adjusted averages are even smaller than the 
// unadjusted ones. You could have also computed sum of the name frequencies
// over all years, which would have automatically incorporated the 0s.

//////////////
//Question 5//
//////////////

// Here I use the code from my example program, adding an additional conditional
// if statement to restrict to sex=="F".

twoway (line freq year if name=="Hilary" & sex=="F",lcolor(blue)) ///
       (line freq year if name=="Hillary" & sex=="F",lcolor(orange)) ///
	   ,xline(1992 2008 2016,lcolor(black)) ///
	    legend(label(1 "Hilary") label(2 "Hillary"))
		
* graph export clinton.pdf		

//////////////
//Question 6//
//////////////

// Hilary and Hillary steadily increased in popularity from 1940 to the early
// 1990s but then plummeted after the 1992 Clinton-Bush election. The names
// stabilized in popularity for the next decade and a half, then increased
// somewhat before the 2008 Clinton-Obama primary and fell again. Hillary then
// decreased further after the 2016 Clinton-Trump election.
