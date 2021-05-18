
# Development Simulation Study, version 2
# Missing Data Handing in COA analysis
# This illustrates that you can re-capture the correct values via MMRM



#-------------------------------------------------------
# Notes:
# 5.6.21
# Generate Data such that the variance is much greater than the Beta value
# This would seem to reflect reality
# It also is the only case where there is bias in the estimates

# Why this happens:
# IF you have a small variance relative to your Beta value,
# even after you have drop-out, the remaining values are close to the
# true value because your variance is minimal!

# Further explanation:
# Imagine having a variance of zero - even after half of the subjects are
# gone, you still have a few remaining subjects with perfect estimates



# 5.6.21 - okay so if I turn down my variance from 2 to 1,
# then there is no problem with the MAR drop-out!!!
# Why is that?
# If I crank variance up to 5, I get real serious drop-out --- why!?!?

#5.13.21 EE - fixed bug with emmeans on missing data model
# how? na.action = na.omit in gls call, AND 
#      don't specify data arg in emmeans (or use getData(mod))
# difference in na.omit and na.exclude here: https://stats.stackexchange.com/questions/492955/should-i-use-na-omit-or-na-exclude-in-a-linear-model-in-r
# Note na.omit is NOT just doing complete cases, since earlier timepts in diff rows    

# Do development here, then add functions
rm(list = ls())
gc()

#library(devtools)
#devtools::install()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
library(nlme)
library(emmeans)
library(dplyr)


number.repl <- 100
repl <- 4
#ag <- c('Improved_2+',  'Improved_1',  'Maintained',  'Deteriorated_1',  'Deteriorated_2+')
ag <- c('Improved',   'Maintained',  'Deteriorated')
comp.mean <- as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag)))
comp.mmrm <- as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag)))
mar.mean <- as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag)))
mar.mmrm <- as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag)))

number.timepoints <- 4

for (repl in 1:number.repl){

  set.seed(as.numeric(5122021 + repl))

# Generate data
sim.out <- COA34::sim_pro_dat(N=100,
                              number.timepoints = number.timepoints,
                              number.of.anchor.groups = 3,
                              polychor.value = 0.4,
                              corr = 'ar1',
                              cor.value = 0.8,
                              var.values = c(5))

dat <- sim.out$dat

# Implement drop-out
dat <- COA34::dropout(dat = dat,
               type_dropout  = c('mcar', 'mar', 'mnar'),
               prop.miss = seq(0.0, 0.3, length.out = number.timepoints),
               stochastic.component = 0.2)

# Compute PRO Score delta
dat <- COA34::compute_change_score(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'))

# Compute anchor groups
dat <- COA34::compute_anchor_group(dat = dat,
                                   anchor.variable = 'PGIS_delta',
                                   number.of.anchor.groups = 3)

#-------------------------------------------------------------------------------

table(dat$Time)
levels(dat$Time)
str(dat)

#------------------------------------------------------------
# Descriptive Statistics:
out <- aggregate(Y_comp_delta ~ anchor.groups, function(x) mean(x, na.rm = T),
                 data = dat[dat$Time == paste0('Time_', number.timepoints),], na.action = na.pass)
tmp <- out$Y_comp_delta
names(tmp) <- paste0(out$anchor.groups)
comp.mean <- dplyr::bind_rows(comp.mean, tmp )


out2 <- aggregate(Y_mar_delta ~  anchor.groups, function(x) mean(x, na.rm = T),
                  data = dat[dat$Time == paste0('Time_', number.timepoints),], na.action = na.pass)
tmp2 <- out2$Y_mar_delta
names(tmp2) <- paste0(out2$anchor.groups)
mar.mean <- dplyr::bind_rows(mar.mean, tmp2 )



# Complete
  mod.gls1 <- gls(Y_comp_delta ~  anchor.groups*Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.omit)
  out <- emmeans::emmeans(mod.gls1, ~ anchor.groups | Time,
                          data = getData(mod.gls1),
                          mode = 'df.error')
  out <- as.data.frame(out)
  out <- out[out$Time == paste0('Time_', number.timepoints), ]
  tmp <- out$emmean
  names(tmp) <- paste0(out$anchor.groups)
  comp.mmrm <- dplyr::bind_rows(comp.mmrm, tmp )



#
# MAR
  mod.gls2.o <- gls(Y_mar_delta ~ anchor.groups*Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.omit)
  mod.gls2.e <- gls(Y_mar_delta ~ anchor.groups*Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.exclude)
  
  #error using na.exclude:
  emmeans::emmeans(mod.gls2.e, ~ anchor.groups | Time,
                   data = dat[dat$Time != 'Time_1',],
                   mode = 'df.error')
  sum(is.na(dat$Y_mar)&!is.na(dat$anchor.groups))#these rows cause error
  
  #check model fit is the same for na.omit and na.exclude
  all.equal(mod.gls2.e$coefficients, mod.gls2.o$coefficients)
  all.equal(mod.gls2.e$modelStruct, mod.gls2.o$modelStruct)
  #use omit, since model fit is the same, and omit does not throw error
  mod.gls2 <- mod.gls2.o
  
  out <- emmeans::emmeans(mod.gls2, ~ anchor.groups | Time,
                          data = getData(mod.gls2),
                          #data = dat[dat$Time != 'Time_1',],
                          mode = 'df.error')
  out <- as.data.frame(out)
  out <- out[out$Time == paste0('Time_', number.timepoints), ]
  tmp <- out$emmean
  names(tmp) <- paste0(out$anchor.groups)
  mar.mmrm <- dplyr::bind_rows(mar.mmrm, tmp )


  cat(paste0('Replication: ', repl, '\n'))


}# end repl


#-----------------------------
table(complete.cases(mar.mean))
table(complete.cases(mar.mmrm))
which(!(complete.cases(mar.mean)))
which(!(complete.cases(mar.mmrm)))

# Complete Data:
# Means
round(colMeans(comp.mean),2)
# MMRM
round(colMeans(comp.mmrm),2)

# MAR drop out
# Means
round(colMeans(mar.mean, na.rm = T), 2)
# MMRM
round(colMeans(mar.mmrm, na.rm = T), 2)


