#' Test-Retest Reliability
#'
#' Uses the `ICC()` function in the `psych` package to compute the ICC(2,1)
#' on a stable subset of subjects. Note that this is equivalent to the ICC(A,1)
#' See Intraclass correlation – A discussion and demonstration of basic features
#' by Liljequist et al 2019. Also see documentation of the `psych` package.
#'
#'
#' @param dat dataframe with values
#' @param PRO.score specify the COA/PRO score you want to compute ICC of.
#' Can pass multiple scores, e.g. `PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar')`
#' @param time.var the time variable, should be an ordered factor
#' @param subject.id the subject ID, should be a character variable
#' @param anchor this is the anchor used to condition on the stable subset of subjects
#' @param stable.score this is the anchor score that indicates the subject is stable.
#' For example, use `anchor = 'PGIS_delta'` and `stable.score = 0` to select
#' the subset of stable sujects.
#' Another example would be `anchor = 'anchor.groups'`
#' and `stable.score = 'Maintained'`.
#' @param first.timepoint specify the first timepoint to be used in the TRTR
#' computation; default is the first timepoint in your `time.var` variable
#' @param second.timepoint specifiy the second timepoint to be used in the TRTR
#' computation; default is the final timepoint in your `time.var` variable
#' @return returns a table `icc.21` with the values
#' @export
#'
#'
compute_icc21 <- function(dat,
                          PRO.score = NULL,
                          time.var = NULL,
                          subject.id = NULL,
                          anchor = NULL,
                          stable.score = NULL,
                          first.timepoint = NULL,
                          second.timepoint = NULL){


  # Check
    if (is.null(dat)) stop('Please pass a dataframe')
    if (is.null(PRO.score)) stop('Please specify the score or scores you`re computing the ICC(2,1) of')
    if (is.null(time.var)) stop('Please specify the variable indicating time')
    if (is.null(subject.id)) stop('Please specify the variable indicating Subject ID')
    if (is.null(anchor)) stop('Please specify the anchor used to establish the stable subset, e.g., `PGIS_delta`')


  out <- vector()

for (score in PRO.score) {

  # First and last timepoints:
  if (is.null(first.timepoint)) {
    first.timepoint <- sort(unique(dat[,time.var, drop = T]), decreasing = F)[1]
  }

  if (is.null(second.timepoint)) {
    second.timepoint <- sort(unique(dat[,time.var, drop = T]), decreasing = T)[1]
  }

    # Stable Subset:
  ids.tr <- which(dat[,time.var, drop = T] == second.timepoint &
                    dat[,anchor,drop = T] == stable.score)
  ids.tr <- dat[ids.tr, subject.id]
    #length(unique(ids.tr))
  dat.trtr <- dat[dat[,subject.id, drop = T] %in% ids.tr, ]
    #dat.icc <- dat.icc[dat.icc$Time %in% as.character(c(bl, final.timepoint)), ]
    # Above does not work!
  dat.trtr <- dat.trtr[,c(subject.id, score, time.var)]
  dat.icc1 <- dat.trtr[dat.trtr[,time.var, drop = T] == first.timepoint, ]
  dat.icc2 <- dat.trtr[dat.trtr[,time.var, drop = T] == second.timepoint, ]
  dat.icc1 <- dat.icc1[, c(subject.id, score)]
  dat.icc2 <- dat.icc2[, c(subject.id, score)]
    colnames(dat.icc1) <- c(subject.id, 'Y1')
    colnames(dat.icc2) <- c(subject.id, 'Y2')
  dat.icc <- merge(x = dat.icc1, y = dat.icc2, by = subject.id)
    # cor(dat.icc[,c('Y1', 'Y2')]) # serves as a check
  #library(psych)
  dat.icc <- dat.icc[complete.cases(dat.icc), ]
  N <- sum(complete.cases(dat.icc[,c('Y1', 'Y2')]))
  icc <- psych::ICC(dat.icc[,c('Y1', 'Y2')], lmer = F)
  # documentation shows this aligns with Shrout & Fleiss 1979, but it's slow
  icc <- as.data.frame(icc$results)
  icc <- data.frame(score, icc[icc$type == 'ICC2', 'ICC'], N, anchor, stable.score)
    colnames(icc) <- c('PRO Score', 'ICC(2,1)', 'N', 'Anchor', 'Stable Anchor Score')

  out <- rbind.data.frame(out, icc)

} # end loop over PRO scores





return(list('icc.21' = out,
            'dat.icc' = dat.icc,
            'score' = score,
            'anchor' = anchor,
            'time.var' = time.var,
            'first.timepoint' = first.timepoint,
            'second.timepoint' = second.timepoint))


}
