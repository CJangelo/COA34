
# 5.14.21
# if you implement drop-out on the change score, works well with 7 timepoints
# MAR: if your change score at the last timepoint was high, you're more likely
# to drop-out at subsequent timepoints


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
#score.names <- c('Y_comp_delta', 'Y_mcar_delta', 'Y_mar_delta', 'Y_mnar_delta')
score.names <- c('Y_delta', 'Y_delta_mcar', 'Y_delta_mar', 'Y_delta_mnar')
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
# dat$Y <- dat$Y_comp
# dat <- COA34::dropout(dat = dat, score.comp = 'Y',
#                       type.dropout  = c('mcar', 'mar', 'mnar'),
#                       prop.miss = seq(0.0, 0.3, length.out = number.timepoints),
#                       stochastic.component = 0.2)

# Compute PRO Score delta
dat <- COA34::compute_change_score(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                  # score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'))
                                   score = c('Y_comp'))

# Compute anchor groups
dat <- COA34::compute_anchor_group(dat = dat,
                                   anchor.variable = 'PGIS_delta',
                                   number.of.anchor.groups = 5)


dat$Y_delta <- dat$Y_comp_delta
dat <- COA34::dropout(dat = dat, score.comp = 'Y_delta',
                      type.dropout  = c('mcar', 'mar', 'mnar'),
                      prop.miss = seq(0.0, 0.3, length.out = number.timepoints),
                      stochastic.component = 0.2)


#-------------------------------------------------------------------------------



#------------------------------------------------------------
# Descriptive Statistics:
score <- 'Y_delta'

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
  mod.gls <- gls(as.formula(paste0(score, ' ~ anchor.groups*Time')),
                  data = dat[dat$Time != 'Time_1',],
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

