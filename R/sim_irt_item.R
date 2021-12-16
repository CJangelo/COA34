#' Simulate Items using IRT model
#'
#' Pass the function a latent variable and IRT item parameters, the function
#' will simulate J items with K categories
#'
#'
#' @param dat dataframe with values
#' @param J number of items
#' @param K number of categories
#' @param item.param option to pass the matrix of item parameters; you have to
#' name the slope "slope" and the intercepts "intercept_1", "intercept_2" etc
#' if you leave this empty, item parameters will be used
#' @param anchor this is the anchor used to condition on the stable subset of subjects
#' @param latent.variable pass the latent variable that will be used in the
#' IRT model to simulate the item responses
#' @param time.var specify the time variable, this is used to standardize the
#' latent variable based on the distribution at the first timepoint. That is,
#' the latent variable at timepoint 1 will have a mean of zero and sd = 1
#' @return returns dataframe with J items in it, also returns the item parameters
#' and a separate dataframe with just the item responses
#' @export
#'
#'
sim_irt_item <- function(dat,
                         J = NULL,
                         K = NULL,
                         item.param = NULL,
                         latent.variable = 'Y_comp',
                         time.var = 'Time'){


  #----------
  # Call the latent variable theta
  theta <- dat[, latent.variable, drop = T]
  # standardize using first timepoint
  tt <- unique(dat[,time.var, drop = T])[1]
  xbar <- mean(theta[ dat[,time.var] == tt ])
  stddev <- sd(theta[ dat[,time.var] == tt ])
  theta <- (theta - xbar)/stddev
  # note I think standardizing it works
  # var-covar is already 1 at Time_1 so centering probably is sufficient


  # make the item parameters a matrix
  if (!is.null(item.param)) item.param <- as.matrix(item.param)

  # Assign the item parameters if none passed:
  if (is.null(item.param)) {

    item.param <- matrix(c(2, seq(-2, 2, length.out = K - 1)),
                         nrow = J, ncol = K, byrow = T,
                         dimnames = list(paste0('Item_', 1:J),
                                         c('slope', paste0('intercept_', 1:(K-1)))))

  }

  # Item Generation - Loop over J
  for (j in 1:J) {

    # Prep item param
    A <- theta * item.param[j, 'slope']
    D <- item.param[j, grep('intercept', colnames(item.param))]
    A <- matrix(A, nrow = nrow(dat), ncol = length(D), byrow = F)
    D <- matrix(D, nrow = nrow(dat), ncol = length(D), byrow = T)

    # Probabilities
    P <- 1/(1 + exp(-1*(A + D)))
    P <- cbind(1, P)
    P.star <- P

    for (i in 1:(ncol(P.star)-1)) {
      P.star[ , i] <- P[ , i, drop = F] - P[ , i+1, drop = F]
    }

    P.cumsum <- t(apply(P.star, 1, cumsum))
    dat[, paste0('Item_', j)] <-
      apply(runif(n = nrow(P.cumsum)) >= P.cumsum, 1, sum)

  }#loop over j


  return(list('dat' = dat,
              'item.param' = item.param,
              'item.responses' = dat[, paste0('Item_', 1:J)]))


} # end function


