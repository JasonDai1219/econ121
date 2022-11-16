# uncomment if these packages are not installed
# install.packages(c('tidyverse','haven'))

# analyzes naming trends in the social security names database (national).
# takes interest in frequency of hilary/hillary before and after clinton elections.

# input Stata file (need haven library for importing Stata file)
library(haven)
ssa <- read_dta("https://github.com/tvogl/econ121/raw/main/data/ssa_names.dta")

# summarize
summary(ssa)

# data structure (need tidyverse library for the glimpse function)
library(tidyverse)
glimpse(ssa)

# keep only girl names, and only Hilary and Hillary
df <- ssa[which(ssa$sex=="F" & ssa$name %in% c("Hilary", "Hillary")), ]

# summarize and glimpse new data frame
summary(df)
glimpse(df)

# graph counts of both names, with a vertical line at 1992, 2008, and 2016
# we will use the ggplot2 package, which is already loaded in the tideverse
ggplot(df, aes(x=year, y=freq, group=name, color=name)) +
  geom_line() +
  geom_vline(xintercept=c(1992, 2008, 2016))
  
# uncomment next line to save as a pdf in the folder "myfolder"
# ggsave("myfolder/clinton.pdf")

