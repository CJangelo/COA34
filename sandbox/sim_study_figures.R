# Simulation Study
# eCDFs to illustrate the results

rm(list = ls())
gc()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
library(nlme)
library(emmeans)
library(dplyr)

source('./sandbox/sim_pro_dat3.R')
out.dir <- 'C:/users/ciaconangelo/OneDrive - OPEN Health/Documents/COA34_old/sandbox/'
number.timepoints <- 7
repl <- 1
#score <- 'Y_comp_delta'


# -------------------------------------------------------------------------
  set.seed(as.numeric(6162021 + repl))

# Generate data
sim.out <- sim_pro_dat3(N=1000,
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

# For sim study R package, don't return all this extra shit in the data gen
#dat1 <- dat[,c('USUBJID', 'Time', 'anchor.groups', 'Y_comp_delta', 'Y_mar_delta')]
dat1 <- dat
dat1 <- dat1[dat1$Time == 'Time_7', ]
dat1 <- dat1[dat1$anchor.groups %in% c('Improved_1',
                                       #'Maintained',
                                       'Deteriorated_1'), ]

# Kludge for plots - implement limits on scores
dat1$Y_comp_delta <-
  ifelse(dat1$Y_comp_delta > 4, 4,
       ifelse(dat1$Y_comp_delta < -4, -4, dat1$Y_comp_delta))

dat1$Y_mar_delta <-
  ifelse(dat1$Y_mar_delta > 4, 4,
       ifelse(dat1$Y_mar_delta < -4, -4, dat1$Y_mar_delta))

# Current practice:
aggregate(Y_mar_delta ~ anchor.groups, FUN = median, data = dat1)
aggregate(Y_comp_delta ~ anchor.groups, FUN = median, data = dat1)
# MMRM estimated thresholds:
# 1.07 for deteriorated 1
# -0.94 for improved _1
est_i1 <- -0.94
est_d1 <- 1.07
i1 <- which(dat1$anchor.groups == 'Improved_1')
d1 <- which(dat1$anchor.groups == 'Deteriorated_1')
dat1$Y_adj <- dat1$Y_mar_delta
# # Center the Median of Improved at the MMRM threshold:
xbar <- median(dat1$Y_mar_delta[i1], na.rm = T)
dat1$Y_adj[i1] <- dat1$Y_mar_delta[i1] - xbar + est_i1
# Center the Median of Deteriorated at the MMRM threshold:
xbar <- median(dat1$Y_mar_delta[d1], na.rm = T)
dat1$Y_adj[d1] <- dat1$Y_mar_delta[d1] - xbar + est_d1

# Comparison of the Approaches:
aggregate(Y_comp_delta ~ anchor.groups, FUN = median, data = dat1)
aggregate(Y_mar_delta ~ anchor.groups, FUN = median, data = dat1)
aggregate(Y_adj ~ anchor.groups, FUN = median, data = dat1)
# Now render this in eCDFs


# push updated function, needs to handle missing data!
dat.den1 <- COA34::compute_ecdf(dat = dat1,
                                anchor.group = 'anchor.groups',
                                time.var = 'Time',
                                timepoint = 'Time_7',
                                change.score = 'Y_adj')
dat.den1$type <- 'Proposed Approach - Adjusted MAR data'

dat.den2 <- COA34::compute_ecdf(dat = dat1,
                                anchor.group = 'anchor.groups',
                                time.var = 'Time',
                                timepoint = 'Time_7',
                                change.score = 'Y_mar_delta')
dat.den2$type <- 'Current Practice - Unadjusted MAR Data'

dat.den3 <- COA34::compute_ecdf(dat = dat1,
                                anchor.group = 'anchor.groups',
                                time.var = 'Time',
                                timepoint = 'Time_7',
                                change.score = 'Y_comp_delta')
dat.den3$type <- 'Current Practice - Complete Data'

dat.den <- rbind.data.frame(dat.den1, dat.den2, dat.den3)

library(ggplot2)
library(grid)
library(gridExtra)

# eCDF
dat.plot <- dat.den[dat.den$anchor.groups == 'Deteriorated_1', ]
p1 <-
  ggplot2::ggplot(dat.plot, aes(x=density_x,
                               y=CDF,
                               group=type,
                               color=type)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5, 5)) +
  #theme(plot.title = element_text(hjust = 0.5)) +
  labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
           legend.title = 'Anchor Groups',
           subtitle = 'Subjects reporting 1 category deterioration on anchor',
           #caption = cap$ecdf.caption,
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Deteriorated 1 category') +
    scale_color_manual(#labels = plot.legend,
                     values = c('green', 'red', 'blue')) +
  geom_hline(yintercept = 0.50, linetype = 'dotted') +
  geom_vline(xintercept = 1.00)
  #geom_vline(xintercept =  0.9839764) +
  #geom_vline(xintercept =  0.5799885) +
  #geom_vline(xintercept =  1.07)
p1
png(file = paste0(out.dir, 'eCDF_deteriorate.png'), units="in", width=11, height=8.5, res=300)
  p1
dev.off()

#--------------
  dat.plot <- dat.den[dat.den$anchor.groups == 'Improved_1', ]
  p2 <- ggplot2::ggplot(dat.plot, aes(x=density_x,
                                      y=CDF,
                                      group=type,
                                      color=type)) +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5, 5)) +
    labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
          legend.title = 'Anchor Groups',
          subtitle = 'Subjects reporting 1 category improvement on anchor',
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Improved 1 category') +
    scale_color_manual(values = c('green', 'red', 'blue')) +
    geom_hline(yintercept = 0.50, linetype = 'dotted') +
    geom_vline(xintercept = -1.00)


png(file = paste0(out.dir, 'eCDF_improvement.png'), units="in", width=11, height=8.5, res=300)
  p2
dev.off()

#------------------
    dat.plot <- dat.den[dat.den$type == "Current Practice - Unadjusted MAR Data", ]
  p3 <- ggplot2::ggplot(dat.plot, aes(x=density_x,
                                      y=CDF,
                                      group=anchor.groups,
                                      color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5, 5)) +
    labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
          legend.title = 'Anchor Groups',
          subtitle = 'Current Practice - Unadjusted MAR Data',
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Anchor Groups') +
    scale_color_manual(values = c('green', 'red')) +
    geom_hline(yintercept = 0.50, linetype = 'dotted')


  p3

#------------------
  dat.plot <- dat.den[dat.den$type == "Current Practice - Complete Data", ]
  p4 <- ggplot2::ggplot(dat.plot, aes(x=density_x,
                                      y=CDF,
                                      group=anchor.groups,
                                      color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5, 5)) +
    labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
          legend.title = 'Anchor Groups',
          subtitle = 'Current Practice - Complete Data',
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Anchor Groups') +
    scale_color_manual(values = c('green', 'red')) +
    geom_hline(yintercept = 0.50, linetype = 'dotted')


  p4


#------------------
  dat.plot <- dat.den[dat.den$type == "Proposed Approach - Adjusted MAR data", ]
  p5 <- ggplot2::ggplot(dat.plot, aes(x=density_x,
                                      y=CDF,
                                      group=anchor.groups,
                                      color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(-5, 5, by = 1), limits = c(-5, 5)) +
    labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
          legend.title = 'Anchor Groups',
          subtitle = 'Proposed Approach - Adjusted MAR data',
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Anchor Groups') +
    scale_color_manual(values = c('green', 'red')) +
    geom_hline(yintercept = 0.50, linetype = 'dotted')


  p5
