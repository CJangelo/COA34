
# Vignette
# eCDFs/ePDFs without the built-in function
# useful to customize eCDFs
# First show the ggplot2_eCDF function
# then the custom version

#-----------------------------------------------------------

# Do development here, then add functions
rm(list = ls())
gc()

library(devtools)
# devtools::build()
# devtools::install()
# devtools::document()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
set.seed(5032021)


# Generate data
sim.out <- COA34::sim_pro_dat(N=1e4,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = c(5))

dat <- sim.out$dat

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

#--------------------------------------------------------------------------
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
# -------------------------------------------------------------------------

    # Custom eCDF & ePDF - illustration to build upon

  dat.den <- COA34::compute_ecdf(dat = dat,
                                 anchor.group = 'anchor.groups',
                                 time.var = 'Time',
                                 change.score = 'Y_comp_delta')

  str(dat.den)

  thr <- COA34::compute_thresholds(dat = dat,
                                   anchor.group = 'anchor.groups',
                                   time.var = 'Time',
                                   change.score = 'Y_comp_delta')

  thr

  plot.legend <- apply(thr[, c('Anchor Group', 'N')], 1,
                         function(x) paste0(x[1], ' (N = ', x[2], ')'))

    if (shell.table) plot.legend <- apply(thr[, c('Anchor Group'), drop = F ], 1, function(x) paste0(x, ' (N = xx)'))

 library(ggplot2)
 library(grid)
 library(gridExtra)

# eCDF
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

# ePDF
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

