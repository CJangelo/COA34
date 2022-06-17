

# Key here is to simulate a treatment effect!


# pull sim_dat from SPR
# attached sim_val_var_v2
# pull sim_irt_item from COA34

rm(list = ls())
gc()

library(SPR)
library(COA34)

# Kind of goofy, the way this function handles a scalar value...eh
  Beta.PRO <- matrix(c(0, 0, 1, -2), nrow = 4, ncol = 1)

  sim <- SPR::sim_dat(N = 250, number.groups = 2, number.timepoints = 2,
             reg.formula = formula(~ Group + Time + Time*Group), Beta = Beta.PRO,
             cor.value = 0.8, var.values = 1)

sim$Beta
str(sim)
dat <- sim$dat

# Validator variables:
 #out2 <- COA34::sim_val_var(dat = dat, PRO.score = 'Y_comp',
 out2 <- sim_val_var_v2(dat = dat, PRO.score = 'Y_comp',
                    n.val = 5,
                    n.cat = c(5, NA, NA, NA, NA),
                    cor.val.ref = c(0.8, 0.7, 0.6, 0.5, 0.4)  )


dat2 <- out2$dat

# Simulate item responses
ir <- COA34::sim_irt_item(dat = dat2, J = 9, K = 4, latent.variable = 'Y_comp', time.var = 'Time')

str(ir)
ir$item.param

dat3 <- ir$dat
