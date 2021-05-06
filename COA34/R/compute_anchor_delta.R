#' Compute the Anchor Change Score
#'
#' @param dat pass the dataframe
#' @param subject.id variable of the subject ID in the dataframe numeric; default is USUBJID
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param anchor numeric anchor variable, default is PGIS
#' @return returns a dataframe with two additional variables, anchor score at baseline and anchor change score
#' @export


compute_anchor_delta <- function(dat = NULL,
                        subject.id = NULL,
                        time.var = NULL,
                        anchor = NULL
                        ){


  if (is.null(dat)) stop('Please specify dataframe in `compute_anchor_delta()` ')
  if (is.null(subject.id)) stop('Please specify subject.id in `compute_anchor_delta()` ')
  if (is.null(time.var)) stop('Please specify time.var in `compute_anchor_delta()` ')
  if (is.null(anchor)) stop('Please specify anchor in `compute_anchor_delta()` ')


  if(any(colnames(dat) %in% paste0(anchor, c( '_bl', '_delta')))) stop('Dataframe contains anchor at baseline variable or anchor delta score variable')

  #print('Sure hope your time variable is ordered correctly lol')


  first.timepoint <- sort(unique(dat[, time.var, drop = T]), decreasing = F)[1]

  t1 <- which(dat[ , time.var] == first.timepoint)
  dat.t1 <- dat[t1, c(subject.id, anchor)]
    colnames(dat.t1) <- c(subject.id, paste0(anchor, '_bl'))
  dat <- merge(x = dat, y = dat.t1, by = subject.id, all = T)
  dat[, paste0(anchor, '_delta')] <- dat[, anchor, drop = T] - dat[, paste0(anchor, '_bl'), drop = T]

return(dat)

}
