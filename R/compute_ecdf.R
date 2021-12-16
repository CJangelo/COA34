#' Compute eCDF
#'
#' Compute cumulative density of change score for eCDF plot. This is the true
#' empirical density, using the stats::ecdf function: https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/ecdf.R
#' which uses the stats::approxfun function
#'
#'
#'
#' @param dat pass the dataframe, must contain the anchor group and the change score
#' @param anchor.group indicate the name of the anchor group
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param timepoint default here is to just use the final timepoint, e.g., "Time_4". Please be sure to have
#' ordered your time variable correctly.
#' @param change.score indicate the name of the PRO change score
#' @return character vector
#' @export






compute_ecdf <- function(
                         dat = NULL,
                         anchor.group = NULL,
                         time.var = NULL,
                         timepoint = NULL, #default is the final timepoint; please prperly order your time variable
                         change.score = NULL

                        ){


# Only acceptable defaul is for the timepoint, rest throw an error if unspecified
    if (is.null(dat)) stop('Please specify dataframe in `compute_ecdf()` ')
    if (is.null(anchor.group)) stop('Please specify anchor.group in `compute_ecdf()` ')
    if (is.null(time.var)) stop('Please specify time.var in `compute_ecdf()` ')
    if (is.null(change.score)) stop('Please specify change.score in `compute_ecdf()` ')



  # Select Final Timepoint:
  if (is.null(timepoint)) {
    final.timepoint <- sort(unique(dat[, time.var, drop = T]), decreasing = T)[1]
  } else {
    final.timepoint <- timepoint
  }

# Reduce the dataframe:
  dat <- dat[which(dat[ , time.var] == final.timepoint), ]
  dat <- dat[, c(anchor.group, change.score), drop = F]

  # Initalize:
  dat$density_x <- NA
  dat$CDF <- NA

  for( ag in unique(dat[, anchor.group]) ) {
    ii <- dat[,anchor.group] == ag
    den.x <- dat[ii, change.score]
    Fn <- stats::ecdf(den.x) # this should automatically handle missing
    den.y <- Fn(den.x)
    dat[ii, 'density_x'] <- den.x
    dat[ii, 'CDF'] <- den.y
  }

  # VERY IMPORTANT STEP:
  # Order the delta values WITHIN each group
  #owg <- with(dat, order( get(anchor.group), get(change.score)))
  #dat <- dat[owg, ]
  #this will order the delta values from least to greatest stratified on the anchor group



  return(dat)


}
