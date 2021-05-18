
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


number.repl <- 100
ag <- c('Improved_2+',  'Improved_1',  'Maintained',  'Deteriorated_1',  'Deteriorated_2+')
#ag <- c('Improved',   'Maintained',  'Deteriorated')
score.names <- c('Y_comp_delta', 'Y_mcar_delta', 'Y_mar_delta', 'Y_mnar_delta')
#score.names <- c('Y_delta', 'Y_delta_mcar', 'Y_delta_mar', 'Y_delta_mnar')
# Initialize outputs:
# out1 is for the means/medians
out1 <- replicate(n = length(score.names),
          expr = as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag))),
          simplify = F)
  names(out1) <- score.names

# out2 is for the mmrm approach
out2 <- out1

number.timepoints <- 7
#Beta.PRO <- matrix(0, nrow = 15, ncol = 1)
#Beta.PRO <- NULL
repl <- 1


# -------------------------------------------------------------------------
# Start Generating Data:
  #repl <- 4 # used to test

for (repl in 1:number.repl){

  set.seed(as.numeric(5132021 + repl))

# Generate data
sim.out <- COA34::sim_pro_dat(N=100,
                              number.timepoints = number.timepoints,
                              number.of.anchor.groups = 5,
                              polychor.value = 0.4,
                              Beta.PRO = NULL,
                              corr = 'ar1',
                              cor.value = 0.8,
                              var.values = c(5))

dat <- sim.out$dat


# Implement drop-out
#dat$Y <- dat$Y_comp
dat <- COA34::dropout(dat = dat, #score.comp = 'Y',
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
                                   number.of.anchor.groups = 5)



#-------------------------------------------------------------------------------



#------------------------------------------------------------
# Descriptive Statistics:
score <- 'Y_comp_delta'

for (score in score.names) {

  # Estimated mean delta score at final timepoint:
  est <- do.call(data.frame,
                 aggregate(as.formula(paste0(score, ' ~ anchor.groups')),
                                function(x) c('mean' = mean(x, na.rm = T),
                                              'median' = median(x, na.rm =T)),
                                data = dat[dat$Time == paste0('Time_', number.timepoints),],
                                na.action = na.pass))

    est1 <- est[,grepl('median', colnames(est)), drop = T]
    names(est1) <- paste0(est$anchor.groups)

      out1[[score]] <- dplyr::bind_rows(out1[[score]], est1)


# Complete
    mod.gls <- gls(as.formula(paste0(score, '~ anchor.groups*Time2')),
                   dat = dat[dat$Time != 'Time_1',],
                   correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                   weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                   na.action = na.omit)

  emm <- emmeans::emmeans(mod.gls, ~ anchor.groups | Time,
                          data = getData(mod.gls),
                          mode = 'df.error')
  emm <- as.data.frame(emm)
  emm <- emm[emm$Time == paste0('Time_', number.timepoints), ]
  est2 <- emm$emmean
  names(est2) <- paste0(emm$anchor.groups)

      out2[[score]] <- dplyr::bind_rows(out2[[score]], est2)

} # end loop over scores


  cat(paste0('Replication: ', repl, '\n'))


}# end repl


lapply(out1, colMeans)
lapply(out2, colMeans)


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
