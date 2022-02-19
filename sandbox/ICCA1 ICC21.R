
rm(list = ls())
gc()

dat <- read.csv(file = 'C:/Users/ciaconangelo/OneDrive - OPEN Health/Documents/RESEARCH_NEW_LAPTOP/NCT00331773/DATA/IQ_scores.csv')
colnames(dat) <- c('mom', 'child')

apply(dat, 2, mean)
apply(dat, 2, sd)
cor(dat)

library(irr) # This is what we were eyeballing - uses complete data
# decomposition of the Mean squares
icc <- irr::icc(dat,
                model = 'twoway',
                type = 'agreement',
                unit = 'single')

icc$value

irr::icc(dat,
         model = 'twoway',
         type = 'consistency',
         unit = 'single')

#
library(psych)
out <- psych::ICC(dat, lmer = T)
out <- as.data.frame(out$results)
out[out$type == 'ICC2', 'ICC']
