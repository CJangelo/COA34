
rm(list = ls())
gc()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
library(nlme)
library(emmeans)
library(dplyr)
library(glmmTMB)


#number.repl <- 30
#ag <- c('Improved_2+',  'Improved_1',  'Maintained',  'Deteriorated_1',  'Deteriorated_2+')
#score.names <- c('Y_comp_delta', 'Y_mcar_delta', 'Y_mar_delta', 'Y_mnar_delta')
number.timepoints <- 7

# use:
#repl <- 1
#score <- 'Y_comp_delta'


# -------------------------------------------------------------------------
# Start Generating Data:

#for (repl in 1:number.repl){
  repl <- 1
  set.seed(as.numeric(6072021 + repl))

# Generate data
sim.out <- COA34::sim_pro_dat(N=1000,
                        number.timepoints = number.timepoints,
                        Beta.PRO = NULL,
                        number.of.anchor.groups = 5,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = c(5))

dat <- sim.out$dat



# Implement drop-out
dat <- COA34::dropout(dat = dat, #score.comp = 'Y',
                      type_dropout  = c('mcar', 'mar', 'mnar'),
                      prop.miss = seq(0.0, 0.3, length.out = number.timepoints),
                      stochastic.component = 0.2)

# Compute PRO Score delta
dat <- COA34::compute_change_score(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   score = c( 'Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'))


# Compute anchor groups
dat <- COA34::compute_anchor_group(dat = dat,
                                   anchor.variable = 'PGIS_delta',
                                   number.of.anchor.groups = 5)


#----------------------------
# Test Retest Reliability
# icc21 <- COA34::compute_icc21(dat = dat,
#                             PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'),
#                             time.var = 'Time',
#                             subject.id = 'USUBJID',
#                             anchor = 'PGIS_delta',
#                             stable.score = 0,
#                             first.timepoint = 'Time_1',
#                             second.timepoint = 'Time_2')


iccA1 <- COA34::compute_iccA1(dat = dat,
                            PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'),
                            time.var = 'Time',
                            subject.id = 'USUBJID',
                            anchor = 'PGIS_delta',
                            stable.score = 0,
                            first.timepoint = 'Time_1',
                            second.timepoint = 'Time_2')

#icc21$icc.21
iccA1$icc.A1
sim.out$cor.mat

#----------------------
# Validity
str(dat)
# PGIS is ordinal; transform it for the purposes of correct correlation
# Leave it numeric to compute the PGIS_delta for the change scores later
dat.cor <- dat
dat.cor$PGIS <-
  factor(dat.cor$PGIS,
         levels = c(0, 1, 2, 3, 4),
         labels = c('None', 'Mild', 'Moderate', 'Severe', 'Very Severe'))
table(dat.cor$PGIS, useNA = 'always')
xtabs(~ dat.cor$PGIS + dat$PGIS)

# Validator Variables 1 and 5 are supposed to be
# ordered categorical variables
dat.cor$Val_1 <- factor(dat.cor$Val_1)
table(dat.cor$Val_1, useNA = 'always')
# only collected at baseline, hence the NAs

dat.cor$Val_5 <- factor(dat.cor$Val_5)
table(dat.cor$Val_5, useNA = 'always')
# Only collected at baseline, hence the NAs

# Check data:
str(dat.cor)
# 5 validator variables (Val_1, Val_2...Val_5)
# 4 PRO scores

out <- COA34::compute_validity(dat = dat.cor,
                               time.var = 'Time',
                               PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'),
                               val.var = c('PGIS', paste0('Val_', 1:5))
                               )
# Note: There's no missing data at baseline, so all of the different
# missing data types have the same N and same values!

# Compare output to generating values:
sim.out$out.val$cor.mat.ref
out

#-----------------------------------
# Known-Groups Validity
out <- COA34::compute_known_groups_validity(dat = dat.cor,
                                            PRO.score = 'Y_comp',
                                            val.var = 'Val_5',
                                            time.var = 'Time')
out


#--------------------------------
# Meaningful Change
thr <- COA34::compute_thresholds(dat = dat,
                                 anchor.group = 'anchor.groups',
                                 time.var = 'Time',
                                 change.score = 'Y_comp_delta')


cap <- COA34::compute_prop_surp(dat = dat,
                                anchor.group = 'anchor.groups',
                                time.var = 'Time',
                                change.score = 'Y_comp_delta',
                                threshold.label = 'Improved_1',
                                mean.or.median = 'median')

str(cap)


table(dat$anchor.groups)
tmp <- gsub(pattern = 'Improved_2+', replacement = paste0("Improved \u2265 2"), x = dat$anchor.groups )
table(tmp)


out <- ggplot2_eCDF(dat = dat,
                    print.to.png = T,
                    file.name = 'Example_eCDF_ePDF_ggplot2',
                    anchor.group = 'anchor.groups',
                    time.var = 'Time',
                    change.score = 'Y_comp_delta')


out$eCDF

out <- ggplot2_eCDF(dat = dat,
                    print.to.png = F,
                    anchor.group = 'anchor.groups',
                    time.var = 'Time',
                    change.score = 'Y_comp_delta')
plot(out$eCDF)
plot(out$ePDF)
# Plot both:
gridExtra::grid.arrange(out$eCDF, out$ePDF, ncol = 1)

# ---------------------------------
# Corrected Thresholds:
  # Fit MMRM
  mod.us1 <- glmmTMB::glmmTMB(Y_mar_delta ~ anchor.groups + us(Time + 0 | USUBJID),
                  data=dat,
                  REML = T,
                  dispformula=~0)

     emm <- emmeans::emmeans(mod.us1, ~ anchor.groups,
                             mode = 'df.error')
    emm <- as.data.frame(emm)
    emm
    round(emm$emmean, 2)
