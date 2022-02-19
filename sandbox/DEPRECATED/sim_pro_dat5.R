

# for personal use
# next step is to incorporate a time element


sim_pro_dat5 <- function(N = 1000,
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
dat <- dat[, c('USUBJID',  'Time', 'Y_comp')]
# dropping Group, was no Group effect here - later go back and do with txa!
colnames(dat) <- c('USUBJID', 'Time', 'PGIS')
# use merge to ensure subject IDs all aligned correctly:
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



# ----
# test
#dat$Time_bl <- ifelse(dat$Time == 'Time_1', 1, 0)
X <- model.matrix( ~ PGIS_bl + ag*Time, data = dat)
 Beta <- matrix(0, nrow = ncol(X), dimnames=list(colnames(X), 'param'))

if (!is.null(Beta.PRO)) {
  if (any(Beta.PRO != 0)) {

      Beta <- Beta.PRO
  }
} else {

  # if Beta.PRO is null, it should be = 1
  lo <- length(which(grepl('Time', rownames(Beta)) & grepl('ag', rownames(Beta))))
  Beta[grepl('Time', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-
    seq(0.25, 1, length.out = lo)
  # test
  Beta['PGIS_bl',] <- 1
  Beta['(Intercept)',] <- 3

}

# -------


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
