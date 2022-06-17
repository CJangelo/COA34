#' Simulate Longitudinal Ordinal Data
#'
#' @param dat pass the dataframe
#' @param n.val numeric,  number of validators
#' @param n.cat numeric vector,  number of categories in each validator, NA for continuous validator variables
#' @param cor.val.ref Reference Measure generation, numeric vector, the correlation between the PRO score and each of the `n.val` validators
#' @return returns a dataframe containing the simulated data
#' @export


sim_val_var_v2 <- function(dat, PRO.score = 'Y_comp',
                           n.val = 5, # number of validators
                           n.cat = c(3, NA, NA, NA, 5), # NA means it's continuous
                           cor.val.ref = c(0.5, 0.6, 0.8, 0.2, 0.3)
                        ){

  # Checks
if( !all(length(n.cat) == n.val &
         length(cor.val.ref) == n.val &
         nrow(dat) > 1
          )
) stop("Check the generating parameters you're passing to the function")


 # Pull the PRO score that you want to be correlated with validators
score <- dat[, PRO.score]

# Create the correlation matrix
cor.mat.ref <- diag(1, nrow = n.val + 1, ncol = n.val + 1)
cor.val.ref <- c(1, cor.val.ref)

for (i in 1:(n.val+1)) {
  for (j in i:(n.val+1)) {

    cor.mat.ref[i, j] <- cor.val.ref[i]*cor.val.ref[j]
    cor.mat.ref[j, i] <- cor.mat.ref[i, j]

  }
}

diag(cor.mat.ref) <- 1
colnames(cor.mat.ref) <- row.names(cor.mat.ref) <- c('Score', paste0('Val_', 1:n.val))


# Create Correlated Validators:
val <- rnorm(n = n.val*length(score), mean = mean(score), sd = sd(score))
val <- matrix( val, nrow = length(score), ncol = n.val)
val <- cbind(score, val) %*% chol(cor.mat.ref)
val <- val[ , -1] # drop the PRO Score (Y_comp) at baseline
val <- as.data.frame(val)
#cor(cbind(dat$Y_comp, val))# confirmed


# Create categorical variable:
# TODO make an option where you can pass a list of the proportions in each variable
# just pass a list with vectors of the proportions
# list(c(0.3, 0.6), NA, NA, NA, c(0.2, 0.4, 0.6, 0.8)) # This will be used instead of the n.cat

for (cc in 1:n.val) {

  if (is.na(n.cat[cc])) next

  pp <- seq( (1/n.cat[cc]), (1 - 1/n.cat[cc]), length.out = n.cat[cc] - 1 )
  thr <- quantile(val[,cc], probs = pp)
  vv <- matrix(val[, cc], nrow = nrow(val), ncol = length(thr), byrow = F) >
                matrix(thr, nrow = nrow(val), ncol = length(thr), byrow = T)
  val[,cc] <- apply(vv, 1, sum)

}


# # Alternative:
# bl2 <- dat$Y_comp
# # All of the variable distributions must be the same normal distribution
# # same mean and sd - baseline was N(0,1), otherwise, specify:
# val2 <- matrix( rnorm(n = n.val*length(bl2), mean = mean(bl2), sd = sd(bl2)), nrow = length(bl2), ncol = n.val)
# val2 <- cbind(bl2, val2) %*% chol(cor.mat.val)
# cor(cbind(dat$Y_comp[dat$Time == 'Time_1'], val2[dat$Time == 'Time_1',-1]))
# # No, not correct, attenuated - can't do this - you're not controlling for the
# # variable here, wrong
# # You want the correlation at baseline to be 0.4


# Tricky part: gotta move one validator over to
# dat$PGIS_bl <- val$PGIS
# dat$PGIS <- dat$PGIS_delta + dat$PGIS_bl
# val <- val[ , -1] # drop PGIS

dat <- cbind.data.frame(dat,
                        matrix(NA, nrow = nrow(dat), ncol = ncol(val), dimnames = list(NULL, colnames(val)))
                        )

# Validator variables collected at Timepoint 1, so put them there (rest are NA)
#dat[dat$Time == 'Time_1', colnames(val)] <- val
dat[, colnames(val)] <- val


out <- list('dat' = dat,
            'n.val' = n.val,  # number of validators
            'n.cat' = n.cat,
            'cor.mat.ref' = cor.mat.ref

)

return(out)

}
