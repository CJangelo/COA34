

# dev_sim_study_6 and _sim_pro_dat4
# both are for the next step, using this data generation technique
# to generate data for psychometric analysis
# This works, use this


# Version 1: No
# generate with PGIS and intercept, no time component,
# center scores, Y_comp ~ PGIS_bl + as.factor(PGIS_delta)
#

# Version 2: yes
# You don't have a time element, so just leave in the Time_1 data
# center scores
# Y_comp ~ as.factor(PGIS_delta)
# No time in MMRM
# This works great, maybe just leave this here?

# Version 3: yes
# See if you can just do the anchor groups instead of as.factor(PGIS_delta)
# generate data using PGIS_delta
# center scores
# Y_comp ~ anchor.groups
# No time in MMRM
# Use this as the approach to generate PRO data


rm(list = ls())
gc()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
library(nlme)
library(emmeans)
library(dplyr)

#source('./sandbox/sim_pro_dat2.R')
#source('./sandbox/sim_pro_dat3.R')
source('./sandbox/sim_pro_dat5.R')

number.repl <- 30
ag <- c('Improved_2+',  'Improved_1',  'Maintained',  'Deteriorated_1',  'Deteriorated_2+')
#ag <- as.character(seq(-4, 4, 1))
score.names <- c('Y_comp_delta', 'Y_mcar_delta', 'Y_mar_delta', 'Y_mnar_delta')
# Initialize outputs: out1 is for the means/medians
out1 <- replicate(n = length(score.names),
          expr = as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag))),
          simplify = F)
  names(out1) <- score.names

# out2 is for the mmrm approach
out2 <- out1
number.timepoints <- 7

# use:
repl <- 1
score <- 'Y_comp_delta'


# -------------------------------------------------------------------------
# Start Generating Data:

for (repl in 1:number.repl){

  set.seed(as.numeric(6072021 + repl))

# Generate data
sim.out <- sim_pro_dat5(N=100,
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



#-------------------------------------------------------------------------------



#------------------------------------------------------------
# Descriptive Statistics:

for (score in score.names) {

  # Estimated mean delta score at final timepoint:
  est <- do.call(data.frame,
                 #stats::aggregate(as.formula(paste0(score, ' ~ as.factor(PGIS_delta)')),
                 stats::aggregate(as.formula(paste0(score, ' ~ anchor.groups')),
                                  function(x) c('mean' = mean(x, na.rm = T),
                                                'median' = median(x, na.rm =T)),
                                  data = dat[dat$Time == paste0('Time_', number.timepoints),],
                                  na.action = na.pass))

    est1 <- est[,grepl('median', colnames(est)), drop = T]
    names(est1) <- paste0(est$anchor.groups)

      out1[[score]] <- dplyr::bind_rows(out1[[score]], est1)



      score.mmrm <- strsplit(score, '_delta')[[1]]
      # Center the score using the mean at baseline:
      xbar1 <- mean(dat[dat$Time == 'Time_1', score.mmrm])
      dat[,score.mmrm] <- dat[,score.mmrm] - xbar1
      # Fit MMRM
   mod.gls <- nlme::gls(as.formula(paste0(score.mmrm, '~ PGIS_bl + anchor.groups*Time')),
                         dat = dat[dat$Time != 'Time_1',],
                         correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                         weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                         na.action = na.omit)



     emm <- emmeans::emmeans(mod.gls, ~ anchor.groups |Time,
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


lapply(out1, function(x) round(colMeans(x, na.rm = T), 2))
lapply(out2, function(x) round(colMeans(x, na.rm = T), 2))


mod.lm <- lm(Y_comp ~ PGIS, data = dat[dat$Time == 'Time_1', ])
  summary(mod.lm)
