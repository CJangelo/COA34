

# Development, Part 2
# Missing Data Handling in COA analysis
# This is a walk through of how the MMRM peforms when recovering all of the
# different types of drop-out
# pairs with "dev_sim_study_2.R"


#-----------------------------------------------------------

# Do development here, then add functions
rm(list = ls())
gc()

#library(usethis)
#usethis::use_vignette("Vignette_1")

library(devtools)
#devtools::build()
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


# Generate data
sim.out <- COA34::sim_pro_dat(N=1e4,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = c(5))

dat <- sim.out$dat

# Implement drop-out
dat <- COA34::dropout(dat = dat,
               type_dropout  = c('mcar', 'mar', 'mnar'),
               prop.miss = 0.5,
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
