#' Compute Thresholds
#'
#' @param dat pass the dataframe
#' @param anchor.group variable of the anchor group you want used here
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param timepoint default here is to just use the final timepoint, e.g., "Time_4". Please be sure to have
#' ordered your time variable correctly.
#' @param change.score the PRO change score you want the threshold computed on
#' @return returns a dataframe with two additional variables, PRO score at baseline and PRO change score
#' @export


compute_thresholds <- function(dat = NULL,
                               anchor.group = NULL,
                               time.var = NULL,
                               timepoint = NULL,
                               change.score = NULL,
                               subject.id = 'USUBJID'
                        ){


  if (is.null(dat)) stop('Please specify dataframe in `compute_thresholds()` ')
  if (is.null(anchor.group)) stop('Please specify anchor.group in `compute_thresholds()` ')
  if (is.null(time.var)) stop('Please specify time.var in `compute_thresholds()` ')
  if (is.null(change.score)) stop('Please specify change.score in `compute_thresholds()` ')



  # Select Final Timepoint:
  if (is.null(timepoint)) {
    final.timepoint <- sort(unique(dat[, time.var, drop = T]), decreasing = T)[1]
  } else {
    final.timepoint <- timepoint
  }


  dat <- dat[which(dat[ , time.var] == final.timepoint), ]
  #N <- length(unique(dat$USUBJID))
  N <- length(unique(dat[ , subject.id, drop = T]))

  # Create table:
  tf <- as.formula(paste0(change.score, '~', anchor.group))
  out <- aggregate(tf,
                   function(x) c('mean' = mean(x, na.rm = T),
                                # 'median' = median(x, na.rm = T),
                                 'median' = quantile(x, probs = 0.5, type = 3, na.rm = T),
                                 'n' = sum(!is.na(x)),
                                 'percent' = 100*sum(!is.na(x))/N),
                   data = dat,
                   na.action = na.pass)
  out <- do.call(data.frame, out)
  out <- cbind(change.score, out)
  colnames(out) <- c('PRO Change Score', 'Anchor Group', 'Mean', 'Median', 'N', 'Percent')

return(out)

}
