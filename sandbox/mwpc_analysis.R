
rm(list = ls())
gc()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
library(nlme)
library(emmeans)
library(dplyr)
library(glmmTMB)



# -------------------------------------------------------------------------
# Generate Data



  set.seed(as.numeric(7012021))

# Generate data
sim.out <- COA34::sim_pro_dat(N=1000,
                        number.timepoints = 7,
                        Beta.PRO = 1,
                        number.of.anchor.groups = 5,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = c(5))

dat <- sim.out$dat



# Implement drop-out
dat <- COA34::dropout(dat = dat, #score.comp = 'Y',
                      type_dropout  = c('mcar', 'mar', 'mnar'),
                      prop.miss = 0.3,
                      stochastic.component = 0.2)

aggregate(Y_mar ~ Time, FUN = function(x) mean(is.na(x)),
          na.action = na.pass, data = dat)


# Compute PRO Score delta
dat <- COA34::compute_change_score(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   score = c( 'Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'))


# Compute anchor groups
dat <- COA34::compute_anchor_group(dat = dat,
                                   anchor.variable = 'PGIS_delta',
                                   number.of.anchor.groups = 5)


#--------------------------------
# Meaningful Change
  dat.den <- COA34::compute_ecdf(dat = dat,
                                 anchor.group = 'anchor.groups',
                                 time.var = 'Time',
                                 change.score = 'Y_comp_delta')


 library(ggplot2)
 library(grid)
 library(gridExtra)


  thr <- COA34::compute_thresholds(dat = dat,
                                   anchor.group = 'anchor.groups',
                                   time.var = 'Time',
                                   change.score = 'Y_comp_delta')

  plot.legend <- apply(thr[, c('Anchor Group', 'N')], 1,
                         function(x) paste0(x[1], ' (N = ', x[2], ')'))

# eCDF
  p1 <- ggplot2::ggplot(dat.den, aes(x=density_x, y=CDF, group=anchor.groups, color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
    labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
          legend.title = 'Anchor Groups',
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Anchor Groups') +
    scale_color_manual(labels = plot.legend,
                    values = unique(dat.den$anchor.groups)) +
      geom_hline(yintercept = 0.50, linetype = 'dotted')


  p1


# ---------------------------------
# Corrected Thresholds:
  # Fit MMRM
  mod.us1 <- glmmTMB::glmmTMB(Y_mar_delta ~ anchor.groups + us(Time + 0 | USUBJID),
                  data=dat,
                  REML = T,
                  dispformula=~0)

     emm <- emmeans::emmeans(mod.us1, ~ anchor.groups,
                             mode = 'df.error')
    emm <- as.data.frame(emm)
    emm
    round(emm$emmean, 2)
