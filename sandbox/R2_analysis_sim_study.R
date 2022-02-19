
# Analysis of output
#----------------------------------------------
rm(list = ls())
gc()
library(nlme)
library(emmeans)
print.dir <- './sandbox/JNJ_Sim_Study_N100_15per/'
out.dir <- 'C:/users/ciaconangelo/OneDrive - OPEN Health/Documents/COA34_old/sandbox/'

# print.dir <- './sandbox/JNJ_Sim_Study_N100_15per_Approach3b/'
# print.dir <- './sandbox/JNJ_Sim_Study_N100_15per_Approach1/'
number.repl <- 1000
number.timepoints <- 7
ag <- c('Improved_2+',  'Improved_1',  'Maintained',  'Deteriorated_1',  'Deteriorated_2+')
score.names <- c('Y_comp_delta', 'Y_mcar_delta', 'Y_mar_delta', 'Y_mnar_delta')
out1 <- replicate(n = length(score.names),
          expr = as.data.frame(matrix(ncol=length(ag),nrow=0, dimnames=list(NULL, ag))),
          simplify = F)
  names(out1) <- score.names

out2 <- out1
out3 <- out1

repl <- 1
score <- 'Y_comp_delta'


for (repl in 1:number.repl){

    dat <- readRDS(file = paste0(print.dir, 'dat_repl', repl, '.rds'))

    for (score in score.names) {

#--------------------------------------------------------------
# Descriptive Statistics
      est <- do.call(data.frame,
                     stats::aggregate(as.formula(paste0(score, ' ~ anchor.groups')),
                                      function(x) c('miss' = mean(is.na(x)),
                                                    'median' = median(x, na.rm =T)),
                                      data = dat[dat$Time == paste0('Time_', number.timepoints),],
                                      na.action = na.pass))

      est1 <- est[,grepl('median', colnames(est)), drop = T]
      est2 <- est[,grepl('miss', colnames(est)), drop = T]
      names(est1) <- paste0(est$anchor.groups)
      names(est2) <- paste0(est$anchor.groups)

      out1[[score]] <- dplyr::bind_rows(out1[[score]], est1)
      out2[[score]] <- dplyr::bind_rows(out2[[score]], est2)

#-----------------------------------------------------------------
# MMRM
    score.mmrm <- strsplit(score, '_delta')[[1]]
    mod.gls <- readRDS(file = paste0(print.dir, 'mod_gls_repl', repl, '_', score.mmrm, '.rds'))
    emm <- emmeans::emmeans(mod.gls, ~ anchor.groups |Time,
                            data = getData(mod.gls),
                            mode = 'df.error')
    emm <- as.data.frame(emm)
    emm <- emm[emm$Time == paste0('Time_', number.timepoints), ]
    est3 <- emm$emmean
    names(est3) <- paste0(emm$anchor.groups)

      out3[[score]] <- dplyr::bind_rows(out3[[score]], est3)


} # end loop over scores

      cat(paste0('Replication: ', repl, '\n'))

    } # end replications


#-------------------------------------------
# Compute Estimates, Bias, MSE, Drop-out Rates
# Compile results into table

true.param <- matrix(c(-2, -1, 0, 1, 2),
                     nrow = number.repl,
                     ncol = 5, byrow = T)

all.out <- vector()
    for (score in score.names) {

      out <- vector()
      lab <- vector()
      # Drop out "out2"
      tmp <- lapply(out2, function(x) round(colMeans(x, na.rm = T), 2))
        out <- rbind(out, 'Drop-out' = tmp[[score]])
        lab <- c(lab, '')
      # Median "out1"
      tmp <- lapply(out1, function(x) round(colMeans(x, na.rm = T), 2))
        out <- rbind(out, 'Estimate' = tmp[[score]])
        lab <- c(lab, 'Median')
      tmp <- lapply(out1, function(x) apply((x - true.param)/true.param, 2, mean))
        out <- rbind(out, 'Relative Bias' = tmp[[score]])
        lab <- c(lab, 'Median')
      tmp <- lapply(out1, function(x) apply((x - true.param)^2, 2, mean))
        out <- rbind(out, 'MSE' = tmp[[score]])
        lab <- c(lab, 'Median')
      # MMRM "out3"
      tmp <- lapply(out3, function(x) round(colMeans(x, na.rm = T), 2))
        out <- rbind(out, 'Estimate' = tmp[[score]])
        lab <- c(lab, 'MMRM')
      tmp <- lapply(out3, function(x) apply((x - true.param)/true.param, 2, mean))
        out <- rbind(out, 'Relative Bias' = tmp[[score]])
        lab <- c(lab, 'MMRM')
      tmp <- lapply(out3, function(x) apply((x - true.param)^2, 2, mean))
        out <- rbind(out, 'MSE' = tmp[[score]])
        lab <- c(lab, 'MMRM')

    #---
      out <- cbind.data.frame('Type of Missingness'= paste0(score),
                              'Approach' = lab,
                              'Parameter' = rownames(out),
                              out)
      all.out <- rbind(all.out, out)

    }

rownames(all.out) <- NULL
all.out

library(R2Word)
R2Word::dump_df_mat_to_file(all.out,
                            NA.string = '-',
                            table.title = 'Results of Simulation Study - N=100, 15% separation, 7 timepoints',
                            print.dir = out.dir,
                            file.name ='Sim_study_results_15per_Approach4')

#-------------------------------------------------
# Average estimates:
# med <- lapply(out1, function(x) round(colMeans(x, na.rm = T), 2))
# mmrm <- lapply(out3, function(x) round(colMeans(x, na.rm = T), 2))
# miss <- lapply(out2, function(x) round(colMeans(x, na.rm = T), 2))

# -----
# Bias
# true.param <- matrix(c(-2, -1, 0, 1, 2),
#                      nrow = number.repl,
#                      ncol = number.repl, byrow = T)
# Absolute Bias:
#lapply(med, function(x) apply(x - true.param, 2, mean))
# Relative Bias:
#rb1 <- lapply(out1, function(x) apply((x - true.param)/true.param, 2, mean))
#rb2 <- lapply(out2, function(x) apply((x - true.param)/true.param, 2, mean))
# should probably be absolute value

# rbind.data.frame(med)
# rbind.data.frame(rb1)
# rbind.data.frame(par2)



# ----
# MSE
# lapply(out2, function(x) apply((x - true.param)^2, 2, mean))

