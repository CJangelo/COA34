#' Compute eCDF
#'
#' Compute cumulative density of change score for eCDF plot
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
  #dat$CDF <- NA
  dat$density_x <- NA
  dat$density_y <- NA
  # dat$lower_ci <- NA
  # dat$upper_ci <- NA

  # VERY IMPORTANT STEP:
  # Order the delta values WITHIN each group
  owg <- with(dat, order( get(anchor.group), get(change.score)))
  dat <- dat[owg, ]
  #this will order the delta values from least to greatest stratified on the anchor group

  #loop this over the groups:
  groups <- dat[, anchor.group, drop = T]
  groups <- unique(groups)

  # Initialize full data frame:
  # Expanded idea - make sure all anchor groups have eCDF values
  # across entire range of PRO scores
  N <- length(dat[, change.score, drop = T])
  df.full <- data.frame(#'CDF' = rep(NA, each = N*length(groups)),
                        'density_x' = rep(NA, each = N*length(groups)),
                        'density_y' = rep(NA, each = N*length(groups)),
                        'CDF' = rep(NA, each = N*length(groups))
                        )
  df.full[, anchor.group] <- rep(groups, each = N)


for(gg in groups){


     # pull change scores in group
      dd <- which(dat[, anchor.group, drop = T] == gg)
      dn <- dat[dd, change.score, drop = T]


      # Density
      # Computed using dn, only change scores from that anchor group
      # However, the density out is computed using the full range of scores
      # This will yield a better looking eCDF (well, that and the kernel smoothing)
      P <- density(dn,
                   kernel='gaussian',
                   bw = 'SJ',
                   n = length(dat[, change.score, drop = T]),
                   from=min(dat[, change.score, drop = T], na.rm = T) - 1,
                   to=max(dat[, change.score, drop = T], na.rm = T) + 1
                   )

      df.full[which(df.full$anchor.group == gg), 'density_x'] <- P$x
      df.full[which(df.full$anchor.group == gg), 'density_y'] <- P$y

      # Test:
      df.full[which(df.full$anchor.group == gg), 'CDF'] <- cumsum(P$y)/sum(P$y)


}# end loop over anchor groups


  return(df.full)


}
