#' Proportion of Subjects Surpassing the Threshold
#'
#' Computes the proportion of subjects surpassing the threshold,
#' either improvement or deterioration, in each anchor group.
#' This can then be passed as a caption to the eCDF plot.
#'
#' @param dat pass the dataframe
#' @param anchor.group variable of the anchor group you want used here
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param timepoint default here is to just use the final timepoint, e.g., "Time_4". Please be sure to have
#' ordered your time variable correctly.
#' @param change.score the PRO change score you want the threshold computed on
#' @param threshold.type character, either Improved or Deteriorated
#' @param mean.or.median pick a threshold type
#' @param threshold.label optional numeric value, threshold to pass;
#' default is default is "Improved_1" if "Improved" and "Deteriorated_1" if "Deteriorated"
#' @return list containing a (1) a table of the proportion of subjects in each anchor group
#' that surpass the threshold, and (2) a character vector to be optionally used as the caption in the eCDF plot
#' @export



compute_prop_surp <- function(
                         dat = NULL,
                         anchor.group = NULL,
                         time.var = NULL,
                         timepoint = NULL, #default is the final timepoint; please prperly order your time variable
                         change.score = NULL,
                         threshold.type = NULL, #c('Improved', 'Deteriorated')
                         mean.or.median = NULL,  #c('Mean', 'Median')
                         threshold.label = NULL # OPTIONAL: default is "Improved_1" if "Improved" and "Deteriorated_1" if "Deteriorated"
                        ){


  #lotta people can't spell too good, so:
  threshold.type <- agrep(threshold.type, c('Improved', 'Deteriorated'), ignore.case = T, value = T)
  mean.or.median <- agrep(mean.or.median, c('Mean', 'Median'), ignore.case = T, value = T)


# These are for the compute threshold function:
    if (is.null(dat)) stop('Please specify dataframe in `ecdf_caption()` ')
    if (is.null(anchor.group)) stop('Please specify anchor.group in `ecdf_caption()` ')
    if (is.null(time.var)) stop('Please specify time.var in `ecdf_caption()` ')
    if (is.null(change.score)) stop('Please specify change.score in `ecdf_caption()` ')

  #  These are for the computation of the eCDF caption:
      if (is.null(threshold.type)) stop('Please specify threshold.type in `ecdf_caption()` ')
      if (is.null(mean.or.median)) stop('Please specify mean.or.median in `ecdf_caption()` ')


  tht <- compute_thresholds(dat = dat, anchor.group = anchor.group, time.var = time.var, change.score = change.score)



  # Select Final Timepoint:
  if (is.null(timepoint)) {
    final.timepoint <- sort(unique(dat[, time.var, drop = T]), decreasing = T)[1]
  } else {
    final.timepoint <- timepoint
  }

  dat <- dat[which(dat[ , time.var] == final.timepoint), ]



  if (threshold.type == 'Improved') {

    if (is.null(threshold.label)) threshold.label <- 'Improved_1'
    tmp <- tht[  , 'Anchor Group', drop = T] == threshold.label
    thr <- tht[tmp, mean.or.median, drop = T]
    tf <- as.formula(paste0(change.score, '~', anchor.group))

    out <- aggregate(tf,
                      function(x) c('N total' = sum(!is.na(x)),
                              'N' = sum(x < thr, na.rm = T),
                              'Percent' = 100*mean(x < thr, na.rm = T)),
                      data = dat,
                      na.action = na.pass)

    out <- do.call(data.frame, out)
    out <- cbind.data.frame(threshold.label, out)
      colnames(out) <- c('Threshold', 'Anchor Group', 'Total', 'Achieved Meaningful Change', 'Percentage')
      lab <- 'Percent Achieved Meaningful Change: '


  }# end if Improved


  if (threshold.type == 'Deteriorated') {

    if (is.null(threshold.label)) threshold.label <- 'Deteriorated_1'
    tmp <- tht[  , 'Anchor Group', drop = T] == threshold.label
    thr <- tht[tmp, mean.or.median, drop = T]
    tf <- as.formula(paste0(change.score, '~', anchor.group))

    out <- aggregate(tf,
                      function(x) c('N total' = sum(!is.na(x)),
                              'N' = sum(x > thr, na.rm = T), # switch sign for Improved vs Deteriorated
                              'Percent' = 100*mean(x > thr, na.rm = T)), # switch sign for Improved vs Deteriorated
                      data = dat,
                      na.action = na.pass)

    out <- do.call(data.frame, out)
    out <- cbind.data.frame(threshold.label, out)
      colnames(out) <- c('Threshold', 'Anchor Group', 'Total', 'Deteriorated', 'Percentage')
      lab <- 'Percent Deteriorated: '

  }# end if Deteriorated


  # make caption:
  tmp1 <- out[ , c('Anchor Group', 'Percentage')]
  tmp1$Percentage <- paste0(sprintf("%.1f", tmp1$Percentage), "%")
  tmp2 <- apply(tmp1, 1, paste0, collapse = ': ')
    tmp2 <- paste0(tmp2, collapse = ', ')
  tmp3 <- paste0('Threshold: ',  sprintf("%.2f", thr), ',')

  ecdf.caption <- paste0(c(tmp3, lab, tmp2), collapse = ' ')
  ecdf.caption


  return(list('anchor.table' = out,
              'ecdf.caption' = ecdf.caption
              ))

}
