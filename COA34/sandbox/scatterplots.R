

# Scatterplots, lol

# Do development here, then add functions
rm(list = ls())
gc()

library(devtools)
# devtools::install()
# devtools::document()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
set.seed(5042021)


# Generate data
out <- COA34::sim_pro_dat(N=50, polychor.value = 0.4)
dat <- out$dat

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

cap <- COA34::compute_prop_surp(dat = dat,
                                anchor.group = 'anchor.groups',
                                time.var = 'Time',
                                change.score = 'Y_comp_delta',
                                threshold.label = 'Improved_1',
                                mean.or.median = 'median')

thr <- COA34::compute_thresholds(dat = dat,
                                 anchor.group = 'anchor.groups',
                                 time.var = 'Time',
                                 change.score = 'Y_comp_delta')
thr

# save.image(file = 'scatterplots_global_environ.RData')
#------------------------------------------------------------
#
#
#
# Scatterplots
#
#
#
#----------------------------------------------------------
dat.plot <- dat
  t4 <- which(dat.plot[ , 'Time'] == 'Time_4')
  dat4 <- dat[t4, c('USUBJID', 'Y_comp')]
    colnames(dat4) <- c('USUBJID', 'Y_comp_4')
  dat.plot <- merge(x = dat.plot, y = dat4, by = 'USUBJID', all = T)

# X & y limits:
# Make symmetric
# NB: Real scores would range from 0 to 10
xlim.max <- round(max(abs(dat.plot$Y_comp)))
use.range <- c(-1*xlim.max, xlim.max)


imp1.mean <- thr[ thr$`Anchor Group` == 'Improved_1' , 'Mean']
imp1.median <- thr[ thr$`Anchor Group` == 'Improved_1' , 'Median']
man.mean <- thr[ thr$`Anchor Group` == 'Maintained' , 'Mean']
det1.mean <- thr[ thr$`Anchor Group` == 'Deteriorated_1' , 'Mean']
det1.median <- thr[ thr$`Anchor Group` == 'Deteriorated_1' , 'Median']

# make caption:
tmp1 <- c('Improved_1: Mean', 'Improved_1: Median',
          'Maintained',
          'Deteriorated_1: Mean', 'Deteriorated_1: Median')
tmp2 <- sprintf("%.2f", c(imp1.mean, imp1.median, man.mean, det1.mean, det1.median))
cap.sp <- paste0(tmp1, '= ', tmp2)


# Legend:
plot.legend <- apply(thr[, c('Anchor Group', 'N')], 1,
                     function(x) paste0(x[1], ' (N=', x[2], ')'))

# Legend Colors:
  unique(dat.plot$anchor.groups)
ag.cols <- c('Deteriorated_2+' = "darkred",
             "Deteriorated_1" = "red",
             'Maintained' = 'blue',
             'Improved_1' = 'green',
             'Improved_2+' = 'darkgreen')


#########png(file = 'test_scatterplot.png', units="in", width=11, height=8.5, res=300)

ggplot(dat.plot,
       aes(x=Y_comp_bl,
           y=Y_comp_4,
           col = anchor.groups)) +
  theme_minimal() +
  geom_point() +
  geom_abline( aes(slope = 1, intercept = 0, linetype = 'No Change'), size = 1) +
  geom_abline( aes(slope = 1, intercept = imp1.mean, linetype = cap.sp[1]), size =0.5) +
  geom_abline( aes(slope = 1, intercept = imp1.median, linetype = cap.sp[2]), size =0.5) +
  geom_abline( aes(slope = 1, intercept = man.mean, linetype = cap.sp[3]), size =0.5) +
  geom_abline( aes(slope = 1, intercept = det1.median, linetype = cap.sp[4]), size =0.5) +
  geom_abline( aes(slope = 1, intercept = det1.mean, linetype = cap.sp[5]), size =0.5) +
  scale_x_continuous( breaks = use.range[1]:use.range[2], limits = use.range) +
  scale_y_continuous( breaks = use.range[1]:use.range[2], limits = use.range) +
  scale_linetype_manual(
    name = "Threshold",
    values = c('No Change' = 'solid',
               "Improved_1: Mean= -1.39" = "longdash",
               "Improved_1: Median= -1.30" = 'dashed',
                "Maintained= 0.26" = "solid",
                "Deteriorated_1: Mean= 1.48" = "dotted",
               "Deteriorated_1: Median= 1.71" = 'dotdash')) +
  labs( title = 'Baseline & Follow-up Scores by Anchor Group',
        legend.title = 'Anchor Groups',
        x = 'Score at baseline',
        y = 'Score at follow-up',
        color = 'Anchor Groups') +
  scale_color_manual(labels = plot.legend, values=ag.cols)

#dev.off()
