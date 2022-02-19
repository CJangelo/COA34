
# Analysis of output
#----------------------------------------------
rm(list = ls())
gc()
library(nlme)
library(emmeans)
print.dir <- './sandbox/JNJ_Sim_Study_N100_15per/'
# print.dir <- './sandbox/JNJ_Sim_Study_N100_15per_Approach3b/'
# print.dir <- './sandbox/JNJ_Sim_Study_N100_15per_Approach1/'
number.repl <- 1000
number.timepoints <- 7
ag <- c('Improved_2+',  'Improved_1',  'Maintained',  'Deteriorated_1',  'Deteriorated_2+')
score.names <- c('Y_comp_delta', 'Y_mcar_delta', 'Y_mar_delta', 'Y_mnar_delta')
# Initialize outputs: out1 is for the means/medians
out1 <- replicate(n = length(score.names),
          expr = as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag))),
          simplify = F)
  names(out1) <- score.names

# out2 is for the mmrm approach
out2 <- out1

repl <- 1
score <- 'Y_comp_delta'


for (repl in 1:number.repl){

    dat <- readRDS(file = paste0(print.dir, 'dat_repl', repl, '.rds'))

    for (score in score.names) {

#--------------------------------------------------------------
# Descriptive Statistics
      est <- do.call(data.frame,
                     stats::aggregate(as.formula(paste0(score, ' ~ anchor.groups')),
                                      function(x) c('mean' = mean(x, na.rm = T),
                                                    'median' = median(x, na.rm =T)),
                                      data = dat[dat$Time == paste0('Time_', number.timepoints),],
                                      na.action = na.pass))

      est1 <- est[,grepl('median', colnames(est)), drop = T]
      names(est1) <- paste0(est$anchor.groups)

      out1[[score]] <- dplyr::bind_rows(out1[[score]], est1)


#-----------------------------------------------------------------
# MMRM
    score.mmrm <- strsplit(score, '_delta')[[1]]
    # TODO: change the name to "score" or "score.mmrm" depending on what you fit
    # really should've called this score.mmrm_Ybl to indicate the model
    # specification
    mod.gls <- readRDS(file = paste0(print.dir, 'mod_gls_repl', repl, '_', score.mmrm, '.rds'))
    mod.gls$call
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

    } # end replications


#---------------------------
# Average estimates:
ae1 <- lapply(out1, function(x) round(colMeans(x, na.rm = T), 2))
ae2 <- lapply(out2, function(x) round(colMeans(x, na.rm = T), 2))

# -----
# Bias
true.param <- matrix(c(-2, -1, 0, 1, 2),
                     nrow = number.repl,
                     ncol = number.repl, byrow = T)
# Absolute Bias:
lapply(out2, function(x) apply(x - true.param, 2, mean))
# Relative Bias:
rb1 <- lapply(out1, function(x) apply((x - true.param)/true.param, 2, mean))
rb2 <- lapply(out2, function(x) apply((x - true.param)/true.param, 2, mean))
# should probably be absolute value



# Proportion drop-out:
aggregate(Y_comp_delta ~ Time, FUN = function(x) mean(is.na(x)), data = dat, na.action = na.pass)
aggregate(Y_mcar_delta ~ Time, FUN = function(x) mean(is.na(x)), data = dat, na.action = na.pass)
aggregate(Y_mar_delta ~ Time, FUN = function(x) mean(is.na(x)), data = dat, na.action = na.pass)
aggregate(Y_mnar_delta ~ Time, FUN = function(x) mean(is.na(x)), data = dat, na.action = na.pass)

aggregate(Y_mar_delta ~ anchor.groups + Time, FUN = function(x) mean(is.na(x)), data = dat, na.action = na.pass)
aggregate(Y_mnar_delta ~ anchor.groups + Time, FUN = function(x) mean(is.na(x)), data = dat, na.action = na.pass)


# ----
# MSE
lapply(out2, function(x) apply((x - true.param)^2, 2, mean))

