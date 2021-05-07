

# Development, Part 2
# Missing Data Handling in COA analysis
# This is a walk through of how the MMRM peforms when recovering all of the
# different types of drop-out
# pairs with "dev_sim_study_2.R"

#TODO:
# 1. check missing data patterns
# The PGIS_bl = 1 ensures that the correlation between
# cor(dat$PGIS_delta, dat$PGIS_bl) = -0.55
# So if you have the thing you need for the baseline Known-Groups analysis
# then you end up with a weird drop-out pattern; inverse of expected

# 2. check recovery with small sample sizes - esp relevant for introducing
# this approach via scatterplots
# 3. Find some previous trials to base the Beta values/sigma values on
# should have verisimilitude.

#-----------------------------------------------------------

# Do development here, then add functions
rm(list = ls())
gc()

library(usethis)
usethis::use_vignette("Vignette_Scatterplots")

library(devtools)
# devtools::build()
devtools::install()
devtools::document()
library(pkgdown)
#usethis::use_pkgdown()
pkgdown::build_site()
# Create the RMarkdown README file:
# usethis::use_readme_rmd()
# Be sure to knit the file when you edit it!

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
set.seed(5032021)



# rn <- c( "(Intercept)", "PGIS_bl", "ag" , "TimeTime_2" , "TimeTime_3",
#          "TimeTime_4" ,   "ag:TimeTime_2" ,"ag:TimeTime_3", "ag:TimeTime_4" )
# Beta <- matrix(0, nrow = 9, ncol = 1, dimnames = list(rn, 'param'))
#   Beta[grepl('Time_2', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  0.25
#   Beta[grepl('Time_3', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  0.5
#   Beta[grepl('Time_4', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  1.0
#   Beta

# Generate data
sim.out <- COA34::sim_pro_dat(N=1e4,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        #Beta.PRO = Beta,
                        cor.value = 0.8,
                        var.values = c(5))

#sim.out$out.clmm$Beta
dat <- sim.out$dat
#sim.out$Beta

xtabs(~ PGIS + Time, data = dat)
xtabs(~ PGIS_delta + Time, data = dat)
xtabs(~ ag + Time, data = dat)

aggregate(Y_comp ~  Time, FUN = mean, data = dat)
aggregate(Y_comp ~ ag + Time, FUN = mean, data = dat)
# aggregate(Y_comp ~ ag + Time, FUN = mean, data = dat)
# aggregate(Y_comp ~ PGIS_bl + Time, FUN = mean, data = dat)
cor(dat$PGIS_delta, dat$PGIS_bl)
#cor(dat$Y_comp_bl, dat$ag)
# higher Y at baseline, more likely to be in improved group
cor(dat$Y_comp, is.na(dat$Y_mar))
# higher Y, more likely to drop out
# Hence, more drop-out in Improved group



# Implement drop-out
dat <- COA34::dropout(dat = dat,
               type_dropout  = c('mcar', 'mar', 'mnar'),
               prop.miss = c(0, 0.17, 0.34, 0.5),
               stochastic.component = 0.2)

# You already have the PGIS_bl and PGIS_delta in the generated data,
# you wouldn't have that in a real dataset, so drop that first
dat <- dat[, !(colnames(dat) %in% c('PGIS_bl', 'PGIS_delta'))]

# Compute PGIS delta
dat <- COA34::compute_anchor_delta(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   anchor = 'PGIS')
# Compute PRO Score delta
dat <- COA34::compute_change_score(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'))

# Compute anchor groups
dat <- COA34::compute_anchor_group(dat = dat,
                                   anchor.variable = 'PGIS_delta',
                                   number.of.anchor.groups = 5)


#-----------------------------------------------------------
# Missingness - Rates of Drop out
aggregate(Y_comp_delta ~ anchor.groups, function(x) mean(is.na(x)), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mcar_delta ~ anchor.groups, function(x) mean(is.na(x)), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mar_delta ~ anchor.groups, function(x) mean(is.na(x)), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mnar_delta ~ anchor.groups, function(x) mean(is.na(x)), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mar ~ Time, function(x) mean(is.na(x)), data = dat, na.action = na.pass)

# Descriptive Means
# aggregate(Y_comp ~  anchor.groups, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
# aggregate(Y_mcar ~  anchor.groups, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
# aggregate(Y_mar ~  anchor.groups, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)

aggregate(Y_comp_delta ~  anchor.groups, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mcar_delta ~  anchor.groups, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mar_delta ~   anchor.groups, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mnar_delta ~  anchor.groups, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
sim.out$Beta
sim.out$sigma

# Anchor Groups
xtabs(~ anchor.groups + Time, data = dat)
xtabs(~ anchor.groups + ag, data = dat)
library(tidyr)
tmp <- tidyr::pivot_wider(data = dat,
                          id_cols = c('USUBJID', 'Time'),
                          values_from = 'anchor.groups',
                          names_from = 'Time')

tmp <- tmp[,-1]
tmp <- apply(tmp, 1, paste0, collapse = ' : ')
sort(table(tmp))
length(unique(tmp))
#---------------------------------------------------------------


library(nlme)
library(emmeans)
#
#
# Complete
  mod.gls1 <- gls(Y_comp_delta ~  anchor.groups*Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.exclude)
  emmeans(mod.gls1, ~ anchor.groups | Time, mode = 'df.error')

#
#
# MCAR
  mod.gls2 <- gls(Y_mcar_delta ~  anchor.groups*Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.exclude)
  emmeans(mod.gls2, ~ anchor.groups | Time, mode = 'df.error')

#
#
# MAR
  mod.gls3 <- gls(Y_mar_delta ~  anchor.groups*Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.exclude)
  emmeans(mod.gls3, ~ anchor.groups | Time, mode = 'df.error')

#
#
# MNAR
  mod.gls4 <- gls(Y_mnar_delta ~   anchor.groups*Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.exclude)
  emmeans(mod.gls4, ~ anchor.groups | Time, mode = 'df.error')


# Validity - check the known-groups
mod.pgis <- lm(Y_comp ~ as.factor(PGIS), data = dat[dat$Time == 'Time_1', ])
summary(mod.pgis)
emmeans(mod.pgis, ~ PGIS)
