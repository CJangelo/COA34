

# Key here is to simulate a treatment effect!
# Convert this to a Vignette on Data Generation
# Remember to pass the IRT item parameters - make those clear!


rm(list = ls())
gc()

#library(SPR)
library(COA34)


set.seed(06072022)

# Kind of goofy, the way this function handles a scalar value...eh
  Beta.PRO <- matrix(c(0, 0, 1, -2), nrow = 4, ncol = 1)

  sim <- COA34::sim_dat(N = 250, number.groups = 2, number.timepoints = 2,
             reg.formula = formula(~ Group + Time + Time*Group), Beta = Beta.PRO,
             cor.value = 0.8, var.values = 1)

sim$Beta
str(sim)
dat <- sim$dat

# Validator variables:
# make validator variable #1 a categorical variable with 5 response categories
# call that your PGIS anchor
# highly correlated with the PRO score
 out2 <- COA34::sim_val_var_v2(dat = dat, PRO.score = 'Y_comp',
                    n.val = 5,
                    n.cat = c(5, NA, NA, NA, NA),
                    cor.val.ref = c(0.8, 0.7, 0.6, 0.5, 0.4)  )


dat2 <- out2$dat

dat3 <- COA34::dropout(dat = dat2, type_dropout = c('mcar'), prop.miss = .2)

# Simulate item responses
ir <- COA34::sim_irt_item(dat = dat2, J = 9, K = 4,
                          latent.variable = 'Y_comp', time.var = 'Time')

ir_NA <- COA34::sim_irt_item(dat = dat3, J = 9, K = 4,
                             latent.variable = 'Y_mcar', time.var = 'Time')

str(ir_NA)
str(ir)
ir$item.param

dat4 <- ir$dat
dat5 <- ir_NA$dat

# Create the Sum-score
dat4$sum_score <- apply(dat4[, paste0('Item_', 1:9)], 1, sum)
dat5$sum_score <- apply(dat5[, paste0('Item_', 1:9)], 1, sum)

# Really should add this to the COA34 Vignettes
# SPR as well, I think.
write.table(dat4, na = '.', quote = F, sep = ', ', row.names = F,
            file = 'sim_data_PRO_7June2022.txt')
write.table(dat5, na = '.', quote = F, sep = ', ', row.names = F,
            file = 'sim_data_PRO_Missing_7June2022.txt')

# Check the meaningful change values:
dat6 <- COA34::compute_anchor_delta(dat = dat4,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   anchor = 'Val_1')

str(dat6)


dat7 <- COA34::compute_change_score(dat = dat6,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   score = 'sum_score')


dat8 <- COA34::compute_anchor_group(dat = dat7,
                                   anchor.variable = 'Val_1_delta')

table(dat8$anchor.groups)
xtabs(~anchor.groups + Time, data = dat8)
xtabs(~anchor.groups + Time + Group, data = dat8)

thr <- COA34::compute_thresholds(dat = dat8,
                                 anchor.group = 'anchor.groups',
                                 time.var = 'Time',
                                 change.score = 'sum_score_delta')

thr

