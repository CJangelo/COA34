#' Concurrent Validity
#'
#' Uses the `polycor` package to compute the correlations
#' Pulls the baseline data, checks the variable type, selects
#' the correct correlation type
#'
#' @param dat dataframe with values
#' @param PRO.score specify the COA/PRO score you want to compute ICC of.
#' Can pass multiple scores, e.g. `PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar')`
#' @param val.var specify the validator variables (reference measures)
#' Can pass a character string, e.g., `val.var = c('PGIS', 'Val_1', 'Val_2')`
#' @param time.var the time variable, should be an ordered factor
#' @return returns a dataframe with the correlations
#' @export
#'
#'
compute_validity <- function(dat,
                          PRO.score = NULL,
                          time.var = NULL,
                          val.var = NULL){


  # Check
    if (is.null(dat)) stop('Please pass a dataframe')
    if (is.null(PRO.score)) stop('Please specify the PRO/COA score or scores')
    if (is.null(time.var)) stop('Please specify the variable indicating time')
    if (is.null(val.var)) stop('Please specify the validator variables (reference measures)')


bl <- sort(unique(dat[,time.var, drop = T]), decreasing = F)[1]
dat <- dat[which(dat[,time.var] == bl), ]

out <- vector()

for (i in PRO.score) {
  for (j in val.var) {

    dat.cor <- dat[, c(i, j)]
    dat.cor <- dat.cor[complete.cases(dat.cor),]
    n <- nrow(dat.cor)
    x <- dat.cor[,i]
    y <- dat.cor[,j]

    if (all(is.numeric(x) & is.numeric(y))) {
      cor.type <- 'Pearson'
      cor.val <- cor(x = x, y = y, use = 'everything', method = 'pearson')
    }

    if (all(is.factor(x) & is.factor(y))) {
      cor.type <- 'polychoric'
      cor.val <- polycor::polychor(x = x, y = y)
    }

  # Two if statements because the polyserial function is
    # particular about which variable is numeric and which is ordered categorical
    # can't swap x and y around
    if (all(is.factor(x) & !is.factor(y))) {
      cor.type <- 'polyserial'
      cor.val <- polycor::polyserial(x = x, y = y) # x numeric, y categorical
    }

    if (all(!is.factor(x) & is.factor(y))) {
      cor.type <- 'polyserial'
      cor.val <- polycor::polyserial(x = x, y = y) # x numeric, y categorical
    }

    # Output:
    cor.out <- data.frame(i, j, cor.type, n, cor.val)
    out <- rbind.data.frame(out, cor.out)


  }# end loop over val.var
} # end loop over PRO score

colnames(out) <- c('PRO Score',
                   'Validator Variable',
                   'Correlation Type',
                   'Sample Size',
                   'Correlation')


return(out)


} #end function
