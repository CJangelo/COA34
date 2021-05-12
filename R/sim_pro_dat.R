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
                        number.of.anchor.groups = 5,
                        #------- Generate PRO Score (Y_comp)
                        Beta.PRO = NULL,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = 2,
                        #-------- Pass to sim_val_var()
                        n.val = 5, # number of validators
                        n.cat = c(3, NA, NA, NA, 5),
                        cor.val.ref = c(0.5, 0.6, 0.8, 0.2, 0.3)){



  # Simulate CLMM data
  out <- sim_dat_ord(N = N,
                 number.groups = number.anchor.categories,
                 number.timepoints = number.timepoints,
                 reg.formula = formula( ~ Time + Group + Time*Group),
                 Beta = Beta.anchor,
                 thresholds = c(0.2, 0.40, 0.6, 0.8),
                 corr = polychor.struc,
                 cor.value = polychor.value)


  # Time as an ordered factor:
dat <- out$dat
dat$Time <- factor(dat$Time)
  # Create the anchor groups:
#dat <- out$dat
dat <- dat[, c('USUBJID',  'Time', 'Y_comp')]
# dropping Group, was no Group effect here - later go back and do with txa!
colnames(dat) <- c('USUBJID', 'Time', 'PGIS')

# FIX this part - notice this doesn't work if your subjects IDS are ordered this way
tmp1 <- dat[dat$Time == 'Time_1', c('USUBJID', 'PGIS')]
colnames(tmp1) <- c('USUBJID', 'PGIS_bl')
dat <- merge(x = dat, y = tmp1, by = 'USUBJID', all.x = T)
dat$PGIS_delta <- dat$PGIS - dat$PGIS_bl

av <- dat$PGIS_delta

# Number of anchor groups:
if (number.of.anchor.groups == 5) {

ag <- ifelse(av >= 2, 2,
             ifelse(av == 1, 1,
                    ifelse(av == 0, 0,
                           ifelse(av == -1, -1,
                                  ifelse(av <= -2, -2, NA)))))
}


  if (number.of.anchor.groups == 3) {

 ag <- ifelse(av >= 1, 1,
             ifelse(av == 0, 0,
                    ifelse(av <= -1, -1, NA)))

  }

    if (!(number.of.anchor.groups %in% c(3, 5))) {
      ag <- NA
      }

    dat$ag <- as.vector(ag)


# 5.6.21: Looks good!
# X <- model.matrix( ~ ag*Time, data = dat)
# Beta <- matrix(0, nrow = ncol(X), dimnames=list(colnames(X), 'param'))
# Beta[grepl('Time_2', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  0.25
# Beta[grepl('Time_3', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  0.5
# Beta[grepl('Time_4', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  1.0


# 5.6.21 - this seems to work fine as well
# this is required for the COA34 R package
# Need the relationship between PGIS_bl and Time
X <- model.matrix( ~ PGIS_bl + ag*Time, data = dat)

if (!is.null(Beta.PRO)) {
  #if (all(Beta.PRO == 0)) {
 #     Beta <- matrix(0, nrow = ncol(X), dimnames=list(colnames(X), 'param'))
 # } else {
      Beta <- Beta.PRO
  }
#}

if (is.null(Beta.PRO)) {

  X <- model.matrix( ~ PGIS_bl + ag*Time, data = dat)
  Beta <- matrix(0, nrow = ncol(X), dimnames=list(colnames(X), 'param'))
  Beta['PGIS_bl', ] <- 0
  lo <- length(which(grepl('Time', rownames(Beta)) & grepl('ag', rownames(Beta))))

  Beta[grepl('Time', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-
    seq(0, 1, length.out = lo)

  #Beta[grepl('Time_2', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  0.25
  #Beta[grepl('Time_3', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  0.5
  #Beta[grepl('Time_4', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-  1.0

}




# Matrix multiply:
XB <- X %*% Beta
dat$XB <- as.vector(XB)


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


dat <- out2$dat


return(list('dat' = dat,
            'Beta' = Beta,
            'sigma' = sigma,
            'cor.mat' = cor.mat,
            'out.clmm' = out,
            'out.val' = out2) )

}
