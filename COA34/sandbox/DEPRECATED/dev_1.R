
# Do development here, then add functions
rm(list = ls())
gc()

library(devtools)
devtools::install()
# devtools::document()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
set.seed(4302021)


out <- COA34::sim_pro_dat(N=100)
dat <- out$dat
#dat <- dat[order(dat$Time, dat$USUBJID), ]

dat <- COA34::dropout(dat = dat,
               type_dropout  = c('mcar', 'mar', 'mnar'),
               prop.miss = 0.2,
               stochastic.component = 0.2)

#str(dat)
# You already have the PGIS_bl and PGIS_delta in the generated data,
# you wouldn't have that in a real dataset, so drop that first
dat <- dat[, !(colnames(dat) %in% c('PGIS_bl', 'PGIS_delta'))]


# TO DO: test to see what happens if you drop entire timepoints
# drop entire row, not just make scores NA
# tmp <- dat$USUBJID %in% paste0('Subject_', formatC(x = 90:100, width = 4, flag = '0')) &
#                         dat$Time == 'Time_4'
# table(tmp)
# dat <- dat[!tmp, ]
# table(dat$Time)

dat <- COA34::compute_anchor_delta(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   anchor = 'PGIS')

# table(dat$Time)
# table(dat$PGIS_delta, useNA = 'always')

dat <- COA34::compute_change_score(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   score = 'Y_comp')

# table(dat$Time, useNA = 'always')
# table(is.na(dat$Y_comp_delta), useNA = 'always')


dat <- COA34::compute_anchor_group(dat = dat,
                                   anchor.variable = 'PGIS_delta',
                                   number.of.anchor.groups = 5)

# table(dat$anchor.groups)
# head(dat[, c('USUBJID', 'Time', 'anchor.groups')], 8)
# tail(dat[, c('USUBJID', 'Time', 'anchor.groups')], 8)
# # okay so you have anchor groups for Subject 100 at timepoints 1, 2, 3
# # not anchor group at timepoint 4


thr <- COA34::compute_thresholds(dat = dat,
                                 anchor.group = 'anchor.groups',
                                 time.var = 'Time',
                                 change.score = 'Y_comp_delta')
thr

cap <- COA34::compute_prop_surp(dat = dat,
                                anchor.group = 'anchor.groups',
                                time.var = 'Time',
                                change.score = 'Y_comp_delta',
                                threshold.label = 'Improved_1',
                                mean.or.median = 'median')

cap

# Note for Vignette: Adjust Naming in tables and Figures
table(dat$anchor.groups)
# Show how to change out the names
# gsub()
# or ifelse() statement, using `Improved, 2`
# https://stackoverflow.com/questions/60566227/inserting-math-symbols-in-legends-of-r-plots
# paste0("Improved \u2265 2")


out <- ggplot2_eCDF(dat = dat,
                      anchor.group = 'anchor.groups',
                      time.var = 'Time',
                      change.score = 'Y_comp_delta')


    plot(out$eCDF)
    plot(out$ePDF)

# ADD SCATTERPLOTS
#------------------------------------
# legacy base R plot:
# COA34::plot_ecdf(dat = dat,
#                  anchor.group = 'anchor.groups',
#                  time.var = 'Time',
#                  change.score = 'Y_comp_delta')
#

#-------------------------------------------
    # GO THROUGH EXAMPLE BY HAND:
  # Compute densities for eCDF and ePDF

# Make below part of the Vignette
# walk through the process by-hand to allow them to customize it
# Show them how to print it out to a png
# if you have to focus on 1-2, that works great! make it perfect
# then say, what if you have 175 eCDFs, what do you do?
# just use the function, automatically prints them out
#



anchor.group = 'anchor.groups'; time.var = 'Time'; change.score = 'Y_comp_delta';
threshold.label = 'Improved_2+'; mean.or.median = 'Median'

  dat.den <- COA34::compute_ecdf(dat = dat,
                                 anchor.group = 'anchor.groups',
                                 time.var = 'Time',
                                 change.score = 'Y_comp_delta')

  dat.den
  str(dat.den)

    thr <- COA34::compute_thresholds(dat = dat,
                                   anchor.group = anchor.group,
                                   time.var = time.var,
                                   change.score = change.score)

    plot.legend <- apply(thr[, c('Anchor Group', 'N')], 1,
                         function(x) paste0(x[1], ' (N = ', x[2], ')'))

    #if (shell.table) plot.legend <- apply(thr[, c('Anchor Group')], 1, function(x) paste0(x, collapse = ', N = xx'))

 library(ggplot2)
 library(grid)
 library(gridExtra)

  p1 <- ggplot2::ggplot(dat.den, aes(x=density_x, y=CDF, group=anchor.groups, color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
          legend.title = 'Anchor Groups',
          caption = cap$ecdf.caption,
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Anchor Groups') +
    scale_color_manual(labels = plot.legend,
                     values = unique(dat.den$anchor.groups))

  p1
  p2 <- ggplot2::ggplot(dat.den, aes(x=density_x, y=density_y, group=anchor.groups, color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))+
    labs( title = 'Empirical Density Function of Change in Score from Baseline',
          x = 'Change in score from baseline',
          y = 'Density',
          color = 'Anchor Groups') +
        scale_color_manual(labels = plot.legend,
                     values = unique(dat.den$anchor.groups))

    p2

# Push it to a file:
#png(file = paste0(file.name, '.png'), units="in", width=11, height=8.5, res=300)
# p1
# dev.off()
