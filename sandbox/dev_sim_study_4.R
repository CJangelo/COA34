

# [1] 16.724106  7.135810  3.636169

# All of these estimates are pretty shitty. High variance in estimates.
# Version 1: WORKS
# center score at mean of timepoint 1
# Y_comp ~ Y_comp_bl + anchor.groups*Time
# do not analyze time 1 data in model

# Version 2: WORKS
# did not center score, still works - same as Version 1 other than that
# Y_comp ~ Y_comp_bl + anchor.groups*Time

# sim_pro_dat3 in below:

# Version 3: did NOT work
# sim_pro_dat3, incorporate PGIS_bl, did NOT center score
# Y_comp ~ PGIS_bl + anchor.groups*Time
# clearly not right, need to center the score dude

# Version 4: does not work
# same as version 3, but center the score first
# Y_comp ~ PGIS_bl + anchor.groups*Time
# I think this is going to literally just be change scores, same as before

# Version 5: Does not work
# did not center score
# Y_comp ~ Y_comp_bl + PGIS_bl + anchor.groups*Time
# it looks like even if you centered the score this wouldn't work, unclear why

# Version 6: no
# Generate with PGIS_bl, but ignore PGIS_bl
# sim_pro_dat3, incorporate PGIS_bl, did NOT center score
# Y_comp ~ Y_comp_bl + anchor.groups*Time
# this would be be freaking ideal if it worked

# Version 7: no
# Generate with PGIS_bl, but ignore PGIS_bl
# sim_pro_dat3, incorporate PGIS_bl = 1
# center score
# Y_comp ~ Y_comp_bl + anchor.groups*Time
#
#
# Version 8: No
# Generate with no PGIS_bl
# model change scores
# Y_comp_delta ~ Y_comp_bl + anchor.groups*Time
# I'm sure I tried this before, but...

# Version 9:
# Generate with no PGIS_bl, just an intercept
# Center the scores at Time 1 mean
# Y_comp ~ anchor.groups*Time
# This seems reasonable - unfortunately the baseline is just noise, unrelated
# to PGIS_bl...


#------------------------------------------------------------
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

source('./sandbox/sim_pro_dat3.R')
#print.dir <- './sandbox/JNJ_Sim_Study_N100_15per/'
#print.dir <- './sandbox/JNJ_Sim_Study_N100_15per_Approach3b/'
print.dir <- './sandbox/JNJ_Sim_Study_N100_15per_Approach1/'

number.repl <- 100
ag <- c('Improved_2+',  'Improved_1',  'Maintained',  'Deteriorated_1',  'Deteriorated_2+')
score.names <- c('Y_comp_delta', 'Y_mcar_delta', 'Y_mar_delta', 'Y_mnar_delta')
# Initialize outputs: out1 is for the means/medians
out1 <- replicate(n = length(score.names),
          expr = as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag))),
          simplify = F)
  names(out1) <- score.names

# out2 is for the mmrm approach
out2 <- out1

number.timepoints <- 7
repl <- 1
score <- 'Y_comp_delta'



# -------------------------------------------------------------------------
# Start Generating Data:

for (repl in 1:number.repl){

  set.seed(as.numeric(6092021 + repl))

# Generate data
sim.out <- sim_pro_dat3(N=100,
                        number.timepoints = number.timepoints,
                        Beta.PRO = NULL,
                        number.of.anchor.groups = 5,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = c(7.136))

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

#aggregate(Y_mar ~ anchor.groups + Time, FUN = function(x) mean(is.na(x)), data = dat, na.action = na.pass)
  saveRDS(dat, file = paste0(print.dir, 'dat_repl', repl, '.rds'))

#-------------------------------------------------------------------------------



#------------------------------------------------------------
# Descriptive Statistics:
  score <- 'Y_comp_delta'


for (score in score.names) {

  # # Estimated mean delta score at final timepoint:
  # est <- do.call(data.frame,
  #                stats::aggregate(as.formula(paste0(score, ' ~ anchor.groups')),
  #                                 function(x) c('mean' = mean(x, na.rm = T),
  #                                               'median' = median(x, na.rm =T)),
  #                                 data = dat[dat$Time == paste0('Time_', number.timepoints),],
  #                                 na.action = na.pass))
  #
  #   est1 <- est[,grepl('median', colnames(est)), drop = T]
  #   names(est1) <- paste0(est$anchor.groups)
  #
      score.mmrm <- strsplit(score, '_delta')[[1]]
      ####Center the score using the mean at baseline:
      xbar1 <- mean(dat[dat$Time == 'Time_1', score.mmrm])
      dat[,score.mmrm] <- dat[,score.mmrm] - xbar1  #     out1[[score]] <- dplyr::bind_rows(out1[[score]], est1)
  #

#-----------------

#--------------------------
      # Fit MMRM
   # mod.gls <- nlme::gls(as.formula(paste0(score.mmrm, '~ ', score.mmrm, '_bl  + anchor.groups*Time')),
   # mod.gls <- nlme::gls(as.formula(paste0(score.mmrm, '~  anchor.groups*Time')),
   # mod.gls <- nlme::gls(as.formula(paste0(score.mmrm, '~  PGIS_bl + anchor.groups*Time')),
   # mod.gls <- nlme::gls(as.formula(paste0(score.mmrm, '~ anchor.groups*Time')),
   # mod.gls <- nlme::gls(as.formula(paste0(score.mmrm, ' ~ ', score.mmrm, '_bl + anchor.groups*Time')),
   # mod.gls <- nlme::gls(as.formula(paste0(score.mmrm, ' ~ ', score.mmrm, '_bl + anchor.groups*Time')),
   mod.gls <- nlme::gls(as.formula(paste0(score, '~ anchor.groups*Time')),
                         dat = dat[dat$Time != 'Time_1',],
                         correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                         weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                         na.action = na.omit)

   #saveRDS(mod.gls, file = paste0(print.dir, 'mod_gls_repl', repl, '_', score.mmrm, '.rds'))
   saveRDS(mod.gls, file = paste0(print.dir, 'mod_gls_repl', repl, '_', score, '.rds'))


    # emm <- emmeans::emmeans(mod.gls, ~ anchor.groups |Time,
    #                         data = getData(mod.gls),
    #                         mode = 'df.error')
    # emm <- as.data.frame(emm)
    # emm <- emm[emm$Time == paste0('Time_', number.timepoints), ]
    # est2 <- emm$emmean
    # names(est2) <- paste0(emm$anchor.groups)
    #
    #   out2[[score]] <- dplyr::bind_rows(out2[[score]], est2)

} # end loop over scores


  cat(paste0('Replication: ', repl, '\n'))


}# end repl



lapply(out1, function(x) round(colMeans(x, na.rm = T), 2))
lapply(out2, function(x) round(colMeans(x, na.rm = T), 2))
# est <- out2$Y_mar_delta
# apply(est - matrix(c(-2, -1, 0, 1, 2), nrow = nrow(est), ncol = ncol(est), byrow = T), 2, sd)
# hist(out2$Y_mar_delta[, 'Deteriorated_1'])
# quantile(out2$Y_mar_delta[, 'Deteriorated_1'])
# save.image(file = './sandbox/Janssen_sim_study_delta_15per_9June2021.RData')
#load(file = './sandbox/Janssen_sim_study_10per_9June2021.RData')
#load(file = './sandbox/Janssen_sim_study_Y_bl_15per_9June2021.RData')

# mod.lm <- lm(Y_comp ~ PGIS_bl, data = dat[dat$Time == 'Time_1', ])
  #summary(mod.lm)
# saveRDS(out1, file = './sandbox/out1_t4_cor8_Beta0.rds')
# saveRDS(out1, file = './sandbox/out2_t4_cor8_Beta0.rds')

#saveRDS(out1, file = './sandbox/out1_t7_cor8_Beta0.rds')
#saveRDS(out2, file = './sandbox/out2_t7_cor8_Beta0.rds')


# saveRDS(out1, file = './sandbox/out1_t4_cor95_Beta0.rds')
#out3 <- readRDS(file = './sandbox/out2_t4_cor95_Beta0.rds')


# saveRDS(out1, file = './sandbox/out1_t4_cor0_Beta0.rds')
# saveRDS(out2, file = './sandbox/out2_t4_cor0_Beta0.rds')

#saveRDS(out1, file = './sandbox/out1_t4_cs0.rds')
#saveRDS(out2, file = './sandbox/out2_t4_cs0.rds')

# saveRDS(out1, file = './sandbox/out1_t4_cs8.rds')
######out3 <- readRDS(file = './sandbox/out2_t4_cs8.rds')

# saveRDS(out1, file = './sandbox/out1_t5.rds')
# saveRDS(out2, file = './sandbox/out2_t5.rds')


# saveRDS(out1, file = './sandbox/out1_t6.rds')
# saveRDS(out2, file = './sandbox/out2_t6.rds')


# saveRDS(out1, file = './sandbox/out1_t4.rds')
# saveRDS(out2, file = './sandbox/out2_t4.rds')

# saveRDS(out1, file = './sandbox/out1_t7.rds')
# saveRDS(out2, file = './sandbox/out2_t7.rds')

# saveRDS(out1, file = './sandbox/out1_t8.rds')
# saveRDS(out2, file = './sandbox/out2_t8.rds')


# saveRDS(out1, file = './sandbox/out1_t9.rds')
# saveRDS(out2, file = './sandbox/out2_t9.rds')
