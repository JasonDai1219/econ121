# ECON 121 Problem Set 1
# Solution R-script.

############
#Question 1#
############

# I had no group.

############
#Question 2#
############

library(haven)
ssa <- read_dta("https://github.com/tvogl/econ121/raw/main/data/ssa_names.dta")

summary(ssa)

# The dataset spans 1880 to 2020. Name frequencies range from 5
# to nearly 100,000.

############
#Question 3#
############

# I chose Hilary and Hillary, just as in the example code. I wanted to know
# if naming frequencies changed after presidential elections involving the Clintons.

############
#Question 4#
############

# First, I show how I figured out the most common names. You did not need to
# do this in your code. We collapse from name/sex/year frequencies to name/sex 
# frequencies using group_by() in tidyverse (specifically dplyr). We call
# the collapsed dataframe ssa.alltime because it has "all time" frequencies
# for each name/year combination
library(tidyverse)
ssa.alltime <- ssa %>% 
                 group_by(name, sex) %>%
                 summarise(total.freq = sum(freq))
# Now sort ssa.alltime by "all time" frequency in decreasing order.
ssa.alltime <- ssa.alltime[order(-ssa.alltime$total.freq),]
# Look at the first 10 observations
ssa.alltime[c(1:10),]

# We can see from above that the most common names are James for boys (5.2 mil)
# Mary for girls (4.1 mil). You probably didn't create the "all time" totals 
# dataframe, so you could use dplyr syntax to list totals or means of frequencies
# for James, Mary, and your chosen names in the original data.
ssa %>% 
  subset((name=="James"&sex=="M")|(name=="Mary"&sex=="F")|
         (name=="Hilary"&sex=="F")|(name=="Hillary"&sex=="F")) %>%
  group_by(name) %>%
  summarise(total.freq = sum(freq))
  
# You could have also worked with the mean frequencies. See the Stata code for
# an example. Anyway, it is clear that Hilary and Hillary are much less popular
# than James and Mary.

############
#Question 5#
############

# Graph counts of both names, with a vertical line at 1992, 2008, and 2016.
# We will use the ggplot2 package, which is already loaded in the tidyverse.
# In the ggplot function, I first subset the ssa dataframe using dplyr syntax.

ggplot(ssa %>% subset((name=="Hilary"&sex=="F")|(name=="Hillary"&sex=="F")), aes(x=year, y=freq, group=name, color=name)) +
  geom_line() +
  geom_vline(xintercept=c(1992, 2008, 2016))

# ggsave("clinton.pdf")

############
#Question 6#
############

# Hilary and Hillary steadily increased in popularity from 1940 to the early
# 1990s but then plummeted after the 1992 Clinton-Bush election. The names
# stabilized in popularity for the next decade and a half, then increased
# somewhat before the 2008 Clinton-Obama primary and fell again. Hillary then
# decreased further after the 2016 Clinton-Trump election.
