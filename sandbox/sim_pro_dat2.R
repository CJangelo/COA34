
# this version implements the variance-covariance structure on the change scores

sim_pro_dat2 <- function(N = 1000,
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
X <- model.matrix( ~  ag*Time, data = dat)
 Beta <- matrix(0, nrow = ncol(X), dimnames=list(colnames(X), 'param'))

if (!is.null(Beta.PRO)) {
  if (any(Beta.PRO != 0)) {

      Beta <- Beta.PRO
  }
} else {

  # if Beta.PRO is null, it should be = 1
  lo <- length(which(grepl('Time', rownames(Beta)) & grepl('ag', rownames(Beta))))
  #Beta[grepl('PGIS_bl', rownames(Beta)), ] <- 1
  Beta[grepl('Time', rownames(Beta)) & grepl('ag', rownames(Beta)), ] <-
    seq(0.25, 1, length.out = lo)

}

# -------


# Matrix multiply:
XB <- X %*% Beta
dat$XB <- as.vector(XB)

# Now operate on the change scores:
#dat[dat$Time == 'Time_1', 'XB' ]

#-------------------------------------------------------------------------------------------------

number.timepoints <- number.timepoints - 1

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
    #colnames(error) <- unique(dat$Time)
    colnames(error) <- unique(dat$Time)[-1]

  # Associate errors with the correct XB to create correct Y for each subject
  dat$error <- 0

  #for (tt in unique(dat$Time)) {
  for (tt in unique(dat$Time)[-1]) {

    dat$error[which(dat$Time == tt)] <- error[, tt, drop = T]

  }


  #dat$Y_comp <- as.vector(dat$XB + dat$error)
  dat$Y_comp_delta <- as.vector(dat$XB + dat$error)

  X.bl <- model.matrix( ~  PGIS_bl, data = dat[dat$Time == 'Time_1', ])
    Beta.bl <- matrix(0, nrow = ncol(X.bl), dimnames=list(colnames(X.bl), 'param'))
    Beta.bl['PGIS_bl', ] <- 0
    # next time make this =1, for now just whatever
    XB.bl <- X.bl %*% Beta.bl
    Y_comp_bl <- as.vector(XB.bl + rnorm(n = nrow(XB.bl), mean = 0, sd = 1))
      dat$Y_comp_bl <- 0
    for (tt in unique(dat$Time)) {
      dat$Y_comp_bl[dat$Time == tt] <- Y_comp_bl
    }

    dat$Y_comp <- dat$Y_comp_delta + dat$Y_comp_bl





#-------------------------------------------------------------------------------
# Validator Variables
#source("C:/Users/ciaconangelo/Documents/RESEARCH_NEW_LAPTOP/R_CODE_Long_Mixed_Models/sim_val_var.R")

#  out2 <- sim_val_var(dat = dat,
#                     n.val = n.val,
#                     n.cat = n.cat,
#                     cor.val.ref = cor.val.ref  )
#
#
# dat <- out2$dat


return(list('dat' = dat,
            'Beta' = Beta,
            'sigma' = sigma,
            'cor.mat' = cor.mat,
            'out.clmm' = out,
            'Beta.bl' = Beta.bl))
           # 'out.val' = out2) )

}
