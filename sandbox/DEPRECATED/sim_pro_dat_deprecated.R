#' Simulate Longitudinal PRO Data with anchor and validator variables included
#'
#' @param number.anchor.categories anchor generation - Pass to `sim_dat_ord()`: the number of PGIS anchor categories to be generated,
#' this is passed to the `number.groups` in the `sim_dat_ord` function
#' @param reg.formula anchor generation - Pass to `sim_dat_ord()`: this is a regression formula to pass to the anchor data generation; null is
#' formula(~ Group + Time + Time*Group),
#' @param Beta.anchor anchor generation - Pass to `sim_dat_ord()`: this is the Beta for the regression equation; numeric matrix of Beta values OR a scalar value != 0
#' that will be the final value of the interaction parameters, default is all(Beta == 0) for Type I error simulations
#' @param thresholds anchor generation - Pass to `sim_dat_ord()`: the ordinal generation uses a probit approach, so these are the thresholds
#' @param polychor.struc anchor generation - Pass to `sim_dat_ord()`: options for correlation structure  c('ind', 'ar1', 'cs'), default is 'ar1'
#' @param polychor.value anchor generation - Pass to `sim_dat_ord()`: numeric, the first corr in ar1, the corr in cs option
#' @param Beta.PGIS.delta PRO data generation, the Beta value for the PGIS delta variable
#' @param Beta.PGIS.bl PRO data generation, the Beta value for the PGIS baseline variable
#' @param corr PRO data generation, options for correlation structure  c('ind', 'ar1', 'cs'), default is 'ar1'
#' @param cor.value PRO data generation, numeric, the first corr in ar1, the corr in cs option, default is 0.8
#' @param var.values PRO data generation, numeric, if numeric vector, the var values in
#' the var/covar matrix, if scalar, the final var value in the var/covar matrix, others filled in automatically
#' @param n.val Validator variable (Reference Measure) generation - Pass to `sim_val_var()`:
#' numeric, number of validators
#' @param n.cat Validator variable (Reference Measure) generation - Pass to `sim_val_var()`:
#'  numeric vector,  number of categories in each validator,
#' NA for continuous validator variables
#' @param cor.val.ref Validator variable (Reference Measure) generation - Pass to `sim_val_var()`:
#' numeric vector, the correlation between the PRO score and each of the `n.val` validators
#' @return returns a dataframe containing the simulated data
#' @export
#'
#'
sim_pro_dat <- function(N = 1000,
                        number.timepoints = 4,
                        #--------Pass to sim_dat_ord()
                        number.anchor.categories = 4,
                        reg.formula = formula( ~ Time + Group + Time*Group),
                        Beta.anchor = 0,
                        thresholds = c(0.2, 0.4, 0.6, 0.8),
                        polychor.struc = 'ar1',
                        polychor.value = 0.4,
                        #------- Generate PRO Score (Y_comp)
                        Beta.PGIS.delta = 1,
                        Beta.PGIS.bl = 1,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = 2,
                        #-------- Pass to sim_val_var()
                        n.val = 5, # number of validators
                        n.cat = c(3, NA, NA, NA, 5),
                        cor.val.ref = c(0.5, 0.6, 0.8, 0.2, 0.3)){


#source("C:/Users/ciaconangelo/Documents/RESEARCH_NEW_LAPTOP/R_CODE_Long_Mixed_Models/sim_dat_ord.R")

 #-------------------------------------------------------
  # N = 1e4
  # number.groups = 2
  # number.timepoints = 4

# -------------------------------------------------------

  # Simulate CLMM data
  out <- sim_dat_ord(N = N,
                 number.groups = number.anchor.categories,
                 number.timepoints = number.timepoints,
                 reg.formula = formula( ~ Time + Group + Time*Group),
                 Beta = Beta.anchor,
                 thresholds = c(0.2, 0.40, 0.6, 0.8),
                 corr = polychor.struc,
                 cor.value = polychor.value)


  # Create the anchor groups:
dat <- out$dat
dat <- dat[, c('USUBJID',  'Time', 'Y_comp')]
# dropping Group, was no Group effect here - later go back and do with txa!
colnames(dat) <- c('USUBJID', 'Time', 'PGIS')

# FIX this part - notice this doesn't work if your subjects IDS are ordered this way
tmp1 <- dat[dat$Time == 'Time_1', c('USUBJID', 'PGIS')]
colnames(tmp1) <- c('USUBJID', 'PGIS_bl')
dat <- merge(x = dat, y = tmp1, by = 'USUBJID', all.x = T)
dat$PGIS_delta <- dat$PGIS - dat$PGIS_bl


# -----------------------------------------------------------------
# Create Beta parameters for these design matrix
# "Time" effect has to be zero for test-retest reliability to be high!
# Otherwise even PGIS_delta = 0 stable subgroup has a change in PRO score that is not adjusted for

# 4.8.21: Below are both exactly the same, only issue is that the 10 **could** be difference across the timepoints, if desired!
#X <- model.matrix( ~ PGIS_bl + PGIS_delta, data = dat)
X <- model.matrix( ~ PGIS_delta*Time, data = dat)
Beta <- matrix(0, nrow = ncol(X), dimnames=list(colnames(X), 'param'))
# #   Beta['PGIS_bl', ] <- 1
# #   Beta['PGIS_delta', ] <- 0 # PGIS_delta is all zero at baseline, so this doesn't matter, coudl be 1,354,988,289
Beta[grepl('Time_2', rownames(Beta)) & grepl('PGIS_delta', rownames(Beta)), ] <- 0.25
Beta[grepl('Time_3', rownames(Beta)) & grepl('PGIS_delta', rownames(Beta)), ] <- 0.5
Beta[grepl('Time_4', rownames(Beta)) & grepl('PGIS_delta', rownames(Beta)), ] <- 1


# 4.16.21 - PGIC Data generation
# Note that this is also for the full data generation to perform full psychometric analysis
#
# X <- model.matrix( ~ PGIS_bl + PGIS_delta, data = dat)
# Beta <- matrix(0, nrow = ncol(X), dimnames=list(colnames(X), 'param'))
# Beta['PGIS_bl', ] <- Beta.PGIS.bl
# Beta['PGIS_delta', ] <- Beta.PGIS.delta
# IMPORTANT NOTE: the Y_delta is impacted by both variance at time 1 and final timepoint
# So the Beta has to be large to counteract this

# Matrix multiply:
XB <- X %*% Beta
dat$XB <- as.vector(XB)

# -------------------------------------------------------------------
# DISTRIBUTION OF RESIDUALS
# 4.7.21: Update, no, these don't matter at all
# It's the PGIS_bl coefficient that affects the mod.pgis and the ICC(2,1)
# Default has to be 'cs', you'll see when you try to compute the damn test-retest reliability, lol
# var.values <- 2 # Controls R^2 value in known-groups validity lm(Y_delta ~ PGIS)
# TODO: how and why does this thing impact that R2? The variance at timepoint 1 does not change!?!?
# cor.value <- 0.8 # approximately ICC(2,1) - has to be > 0.7!
# This should be the ICC(2,1); however, it is attenuated by the fact that
# there is a coefficient for PGIS_bl that adds noise to the stable subgroup
# corr <- 'ar1' # ICC(2,1) estimated between first and last timepoint,
# so need the correlation high at first and last timepoint!

#-------------------------------------------------------------------------------------------------

  if (corr == 'ind') {

    cor.mat <- diag(1, nrow = number.timepoints, ncol = number.timepoints)

  }# end independent structure


  if (corr == 'cs') {

    if (is.null(cor.value)) { cor.value <- 0.4 }
    cor.mat <- matrix(cor.value, nrow = number.timepoints, ncol = number.timepoints)
    diag(cor.mat) <- 1

  } # end Compound Symmetry correlation


  if (corr == 'ar1') {

    if (is.null(cor.value)) { cor.value <- 0.8 }

    cor.mat <- diag(1, nrow = number.timepoints, ncol = number.timepoints)
    for (i in 1:number.timepoints) {
      for (j in 1:i) {

        cor.mat[i , j] <- cor.value^(i -j) # AR1
        cor.mat[j, i] <- cor.mat[i, j]

      }
    }

  }# end exponential decay correlations



# Default variance value at last timepoint is 2
  # Can either adjust that last value (rest will be filled in automatically)
  # OR you can pass the full vector
  if (length(var.values) == 1) {

        var.values <- seq(1, var.values, length.out = number.timepoints)

  } else {

    if (length(var.values) != number.timepoints) stop('Vector of variance values does not equal number of timepoints')

  }

  # Variance- Covariance Matrix:
  var.mat <- diag(sqrt(var.values), nrow = number.timepoints, ncol = number.timepoints)
  sigma <- var.mat %*% cor.mat %*% var.mat


  # Simulate the errors:
  error <- MASS:::mvrnorm(n = N, mu = rep(0, number.timepoints), Sigma = sigma)
    colnames(error) <- unique(dat$Time)

  # Associate errors with the correct XB to create correct Y for each subject
  dat$error <- NA

  for (tt in unique(dat$Time)) {

    dat$error[which(dat$Time == tt)] <- error[, tt, drop = T]

  }


  #Y <- XB + error.long
  dat$Y_comp <- as.vector(dat$XB + dat$error)


#-------------------------------------------------------------------------------
# Validator Variables
#source("C:/Users/ciaconangelo/Documents/RESEARCH_NEW_LAPTOP/R_CODE_Long_Mixed_Models/sim_val_var.R")

 out2 <- sim_val_var(dat = dat,
                    n.val = n.val,
                    n.cat = n.cat,
                    cor.val.ref = cor.val.ref  )

# out2 <- sim_val_var(dat = dat,
#                     n.val = 5,
#                     n.cat = c(3, NA, NA, NA, 5),
#                     cor.val.ref = c(0.5, 0.6, 0.4, 0.2, 0.3) )

dat <- out2$dat


# # Create Y_delta
# tmp1 <- dat[dat$Time == 'Time_1', c('USUBJID', 'Y_comp')]
#   colnames(tmp1) <- c('USUBJID', 'Y_bl')
# dat <- merge(x = dat, y = tmp1, by = 'USUBJID', all.x = T)
# dat$Y_delta <- dat$Y_comp - dat$Y_bl


return(list('dat' = dat,
            'Beta' = Beta,
            'out.clmm' = out,
            'out.val' = out2) )

}
