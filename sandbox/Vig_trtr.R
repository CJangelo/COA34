
# test-retest reliability
# add to template
# 5.10.21

# TODO:
# 1. DONE - Expand function so you can pass multiple PRO scores, output a table
# 2. Figure out why MAR drop-out torpedoes the ICC(2,1) values!
# They go from 0.5 to 0.05!

rm(list = ls())
gc()

# library(devtools)
# devtools::install()
# devtools::document()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
library(psych)

set.seed(5102021)


# Generate data
sim.out <- COA34::sim_pro_dat(N=1e3,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = c(5))

dat <- sim.out$dat
sim.out$sigma
sim.out$cor.mat

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

# Test Retest Reliability
icc <- COA34::compute_icc21(dat = dat,
                            PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'),
                            time.var = 'Time',
                            subject.id = 'USUBJID',
                            anchor = 'PGIS_delta',
                            stable.score = 0)
icc$icc.21
str(icc$icc.21)

# ----------------------------------------------------------------------
# Code below was/is basis for the `compute_icc21()` function
# This can go in a Vignette for illustration and for flexbility
#
PRO.score <- c('Y_comp' ,'Y_mcar', 'Y_mar')
score <- 'Y_comp'
time.var <- 'Time'
subject.id <- 'USUBJID'
anchor <- 'PGIS_delta'
#anchor <- 'anchor.groups'
#stable.score <- 'Maintained'
stable.score <- 0


  all.out <- vector()

for (score in PRO.score) {


bl <- sort(unique(dat[,time.var, drop = T]), decreasing = F)[1]
final.timepoint <- sort(unique(dat[,time.var, drop = T]), decreasing = T)[1]

# Stable Subset:
ids.tr <- which(dat[,time.var, drop = T] == final.timepoint &
                  dat[,anchor,drop = T] == stable.score)
ids.tr <- dat[ids.tr, subject.id]
length(unique(ids.tr))
dat.trtr <- dat[dat[,subject.id, drop = T] %in% ids.tr, ]
#dat.icc <- dat.icc[dat.icc$Time %in% as.character(c(bl, final.timepoint)), ]
# Above does not work!
dat.trtr <- dat.trtr[,c(subject.id, score, time.var, 'PGIS_bl')] # Include PGIS_bl
dat.icc1 <- dat.trtr[dat.trtr[,time.var, drop = T] == bl, ]
dat.icc2 <- dat.trtr[dat.trtr[,time.var, drop = T] == final.timepoint, ]
dat.icc1 <- dat.icc1[, c(subject.id, score, 'PGIS_bl')]
dat.icc2 <- dat.icc2[, c(subject.id, score)]
colnames(dat.icc1) <- c(subject.id, 'Y1', 'PGIS_bl')
colnames(dat.icc2) <- c(subject.id, 'Y2')
dat.icc <- merge(x = dat.icc1, y = dat.icc2, by = subject.id)
# cor(dat.icc[,c('Y1', 'Y2')]) # serves as a check
library(psych)
dat.icc <- dat.icc[complete.cases(dat.icc), ]
N <- sum(complete.cases(dat.icc[,c('Y1', 'Y2')]))
out <- psych::ICC(dat.icc[,c('Y1', 'Y2')], lmer = F)
# documentation shows this aligns with Shrout & Fleiss 1979, but it's slow
out <- as.data.frame(out$results)
out[out$type == 'ICC2k', 'ICC']

tab.out <- data.frame(out[out$type == 'ICC2k', 'ICC'], N, score, anchor)
  colnames(tab.out) <- c('ICC(2,1)', 'N', 'PRO Score', 'Anchor')

  all.out <- rbind.data.frame(all.out, tab.out)
  #all.out <- do.call(rbind.data.frame, list())

}

  all.out


library(lme4)
mod.lmer <- lmer(Y_comp ~ PGIS_bl + (1|USUBJID), data = dat)
summary(mod.lmer)
vv <- unlist(VarCorr(mod.lmer))
vv/(vv + summary(mod.lmer)$sigma^2)
sim.out$cor.mat
