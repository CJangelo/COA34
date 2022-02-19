
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
dat.all <- vector()
out <- data.frame('Approach' = NA,
                  'Data' = NA,
                  'Threshold' = NA,
                  'Prop_Miss' = NA,
                  'Estimate' = NA,
                  'Gen_param' = NA)

repl <- 1
score <- 'Y_comp_delta'


for (repl in 1:number.repl){

    dat <- readRDS(file = paste0(print.dir, 'dat_repl', repl, '.rds'))
        dat.all <- rbind(dat.all, dat)


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

      tmp <- data.frame('Approach' = 'Median',
                        'Data' = score,
                        'Threshold' = est$anchor.groups,
                        'Prop_Miss' = est2,
                        'Estimate' = est1,
                        'Gen_param' = c(-2, -1, 0, 1, 2))


      out <- rbind.data.frame(out, tmp)

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

      tmp <- data.frame('Approach' = 'MMRM',
                        'Data' = score,
                        'Threshold' = emm$anchor.groups,
                        'Prop_Miss' = est2,
                        'Estimate' = est3,
                        'Gen_param' = c(-2, -1, 0, 1, 2))

      out <- rbind.data.frame(out, tmp)

} # end loop over scores

      cat(paste0('Replication: ', repl, '\n'))

    } # end replications




#---------------------------------------------------------------
# Data Mgmt
out$Data <- ifelse(out$Data == 'Y_comp_delta', 'Complete',
       ifelse(out$Data == 'Y_mcar_delta', 'MCAR',
              ifelse(out$Data == 'Y_mar_delta', 'MAR',
                    ifelse(out$Data == 'Y_mnar_delta', 'MNAR', NA))))

out$Data <- factor(out$Data, levels = c('Complete', 'MCAR', 'MAR', 'MNAR'))
#-------------------------------------------------------------
# Output
tab.out <- aggregate(cbind(Prop_Miss,
                           Gen_param,
                           Estimate,
                           'Bias' = Estimate - Gen_param, #BIAS
                           'MSE' = (Estimate - Gen_param)^2, #MSE
                           'RB' =100*(Estimate - Gen_param)/Gen_param) ~ # relative bias
                            Approach + Data + Threshold,
                            FUN = function(x)  mean(x, na.rm = T), data = out)

tab.out <- tab.out[, c('Data', 'Approach', 'Threshold',
                       'Prop_Miss',
                       'Gen_param',
                       'Estimate', 'Bias', 'RB', 'MSE')]

# Full table for internal use:
tab.out.full <- tab.out

# Subset & Format for Presentation to Janssen team:
tab.out <- tab.out[tab.out$Threshold %in% c('Improved_1', 'Deteriorated_1'), ]
tab.out <- tab.out[tab.out$Data != 'MNAR', ]
tab.out$Threshold <- unlist(strsplit(tab.out$Threshold, split ='_1'))
# formatting:
tab.out$`Estimate (Relative Bias)` <-
  sprintf("%.2f (%.0f%%)", tab.out$Estimate, abs(tab.out$RB))
tab.out <- tab.out[, c('Data', 'Approach',
                       'Threshold', 'Gen_param',
                       'Estimate (Relative Bias)')]


# Print out tables
colnames(tab.out)[colnames(tab.out) == 'Gen_param'] <- 'True Value'
tab.out

library(R2Word)
R2Word::dump_df_mat_to_file(tab.out,
                            NA.string = '-',
                            table.title = 'Results of Simulation Study - N=100, 15% separation, 7 timepoints',
                            print.dir = out.dir,
                            file.name ='Sim_study_results_15per_Approach4')

R2Word::dump_df_mat_to_file(tab.out.full,
                            NA.string = '-',
                            table.title = 'Results of Simulation Study - N=100, 15% separation, 7 timepoints',
                            print.dir = out.dir,
                            file.name ='Sim_study_FULL_results_15per_Approach4')

#-----------------------------------------------
# Illustrate Data Generation:
# Subjects in each anchor group:
tab1 <- xtabs(~ anchor.groups + Time, data = dat.all)
tab1 <- addmargins(100*prop.table(tab1, 2), 1)
tab2 <- as.matrix.data.frame(tab1)
tab2 <- cbind.data.frame(rownames(tab1), tab2)
colnames(tab2) <- c('Anchor Groups', paste0('Time ', 1:7))

R2Word::dump_df_mat_to_file(tab2,
                            NA.string = '-',
                            decimals = 0,
                            table.title = 'Percentage of subjects in each anchor group',
                            print.dir = out.dir,
                            file.name ='Data_Gen_Anchor_Groups')

# Drop-out
dat.all$miss <- 1*is.na(dat.all$Y_mar_delta)
tab3 <- aggregate(miss ~ Time, FUN = mean, data = dat.all)
colnames(tab3) <- c('Time', 'Proportion Missing')

R2Word::dump_df_mat_to_file(tab3,
                            NA.string = '-',
                            decimals = 3,
                            table.title = 'Proportion Drop-out at each timepoint',
                            print.dir = out.dir,
                            file.name ='Data_Gen_dropout')

# Drop-out per anchor group
tab4 <- aggregate(miss ~ anchor.groups + Time, FUN = mean, data = dat.all)
colnames(tab4) <- c('Anchor Groups', 'Time', 'Proportion Missing')
tab4 <- tab4[tab4$Time == 'Time_7', ]
R2Word::dump_df_mat_to_file(tab4,
                            NA.string = '-',
                            decimals = 2,
                            table.title = 'Proportion Drop-out at each timepoint
                            stratified on anchor groups',
                            print.dir = out.dir,
                            file.name ='Data_Gen_dropout_ag')


#------
# Change scores
library(ggplot2)
dat.plot <- dat.all
dat.plot$`Anchor Groups` <- dat.plot$anchor.groups

# Boxplots - change scores
dat.plot <- dat.plot[dat.plot$`Anchor Groups` %in% c('Maintained', 'Deteriorated_1', 'Improved_1'), ]
p1 <- ggplot(dat.plot,
             aes(x=Time, y=Y_comp_delta, color = `Anchor Groups`)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = c(-1, 1), linetype = 'dotted') +
  scale_color_manual(name = "Anchor Groups",values= c('darkgreen', 'black', 'red'))+
  theme_minimal() +
  labs( title = 'Change Scores by Anchor Group',
        x = 'Time',
        y = 'Change Score')


png(file = paste0(out.dir, 'Data_Gen_Change_Scores.png'), units="in", width=11, height=8.5, res=300)

  p1

dev.off()


