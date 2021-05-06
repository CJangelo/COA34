#' Compute the PRO Change Score
#'
#' @param dat pass the dataframe
#' @param subject.id variable of the subject ID in the dataframe numeric; default is USUBJID
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param score numeric PRO score variable, default is Y_comp
#' @return returns a dataframe with two additional variables, PRO score at baseline and PRO change score
#' @export


compute_change_score <- function(dat = NULL,
                        subject.id = NULL,
                        time.var = NULL,
                        score = NULL
                        ){

  if (is.null(dat)) stop('Please specify dataframe in `compute_change_score()` ')
  if (is.null(subject.id)) stop('Please specify subject.id in `compute_change_score()` ')
  if (is.null(time.var)) stop('Please specify time.var in `compute_change_score()` ')
  if (is.null(score)) stop('Please specify score in `compute_change_score()` ')

  if(any(colnames(dat) %in% paste0(score, c( '_bl', '_delta')))) stop('Dataframe contains PRO score at baseline variable or PRO change score variable')

  #print('Sure hope your time variable is ordered correctly lol')


  first.timepoint <- sort(unique(dat[, time.var, drop = T]), decreasing = F)[1]
  final.timepoint <- sort(unique(dat[, time.var, drop = T]), decreasing = T)[1]

  t1 <- which(dat[ , time.var] == first.timepoint)
  dat.bl <- dat[t1, c(subject.id, score)]
    colnames(dat.bl) <- c(subject.id, paste0(score, '_bl'))
  dat <- merge(x = dat, y = dat.bl, by = subject.id, all = T)
  dat[, paste0(score, '_delta')] <- dat[, score, drop = T] - dat[, paste0(score, '_bl'), drop = T]

return(dat)

}
