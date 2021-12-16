#' Known-Groups Validity
#'
#'
#'
#' @param dat dataframe with values
#' @param PRO.score specify the COA/PRO score you want to compute ICC of.
#' Can only pass one character variable at a time.
#' @param val.var specify the validator variables (reference measures).
#' Can only pass one variable at a time.
#' @param time.var the time variable, should be an ordered factor
#' @return returns two different footnotes to use in the table,
#' descriptive statistics table, and the model output
#' @export
#'
#'
compute_known_groups_validity <- function(dat = NULL,
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

y <- PRO.score
x <- val.var




###
# Ordered Categorical PRO Score:
if (!is.numeric(dat[,y, drop = T])) {

  # Descriptive statistics:
  out <- stats::xtabs(as.formula(paste0('~', x, '+', y)),
                                data = dat,
                                na.action = na.pass)
  out <- addmargins(out, 2)
    out <- as.data.frame.matrix(out)
    out <- data.frame(rownames(out), out)
  tmp <- paste0(levels(dat[,y, drop = T]))
  tmp <- paste0('N,', y, '=', tmp)
    colnames(out) <- c( x, tmp, 'Total N')
    out.descr <- out


  # Fit the Model:
    mod <- stats::glm(as.formula(paste0(y, '~', x)), data = dat, family = 'binomial')
    mod.coef <- summary(mod)$coef
    mod.coef <- as.data.frame(mod.coef)
    mod.coef$Estimate <- exp(mod.coef$Estimate)

    r2 <- rcompanion:::nagelkerke(mod)$Pseudo.R.squared.for.model.vs.null
    r2 <- r2[grepl('Nagelkerke', rownames(r2)), ]

# Pull the labels from the Reference Measure:
    ref.meas.labels <- levels(dat[ , x])

# Create DF
  out <-
    data.frame(
      'PRO Score' = y,
      'Variable' = x,
      'Known-Health Group' = c(paste0(ref.meas.labels[1], ' - Reference Group'),
                                  paste0(ref.meas.labels[-1])),
      'Estimate' = mod.coef[ , 'Estimate', drop = T],
      'P-value' = mod.coef[ , 'Pr(>|z|)', drop = T],
      stringsAsFactors=F
    )

  row.names(out) <- NULL
  colnames(out) <- c('PRO Score','Variable',
                     'Known-Health Group', 'Odds-ratio',
                     'P-value')

  # Drop the intercept:
  out[1, c('Odds-ratio', 'P-value')] <- NA
# Drop the odds ratio because it's the OR for whether
# the reference group OR is different from 1
# The other OR are if the group is different from the reference group
# So the interpetation will be strange if you leave it in

# Pass the Model Pseudo R-Squared for use in the footnote of the table
  mod.r2 <- sprintf("Pseudo R-squared value: %.2f", r2)



} #end glm for Ordered Categorical PROScore


###
# If PRO Score is numeric/continuous

if (is.numeric(dat[,y, drop = T])) {


  # Recent FDA response asked for medians as well as means here - confirm!
  out <- do.call(data.frame, stats::aggregate(as.formula(paste0(y, '~', x)),
        FUN = function(x) c(sum(!is.na(x)),
                            mean(x, na.rm = T),
                            #median(x, na.rm = T)),
                            quantile(x, probs = 0.5, type = 3, na.rm = T)),
        data = dat, na.action = na.pass))

    out <- data.frame(y, x, out)
    colnames(out) <- c('PRO Score', 'Variable',
                       'Known-Health Group', 'N',
                       'Mean', 'Median')
    out.descr <- out



    # Y is continuous score:
    mod <- stats::lm(as.formula(paste0(y, '~', x)), data = dat)
    mod.coef <- summary(mod)$coef

    # Pull the labels from the Reference Measure:
    ref.meas.labels <- levels(dat[ , x])


    # Create DF
    out <-
    data.frame(
      'Score' = y,
      'Variable' = x,
      'Known-Health Group' = c(paste0(ref.meas.labels[1], ' - Reference Group'),
                                  paste0(ref.meas.labels[-1])),
      'N' = out.descr$N,
      'Estimate' = mod.coef[ , 'Estimate'],
      'P-value' = mod.coef[ , 'Pr(>|t|)'],
      stringsAsFactors=F
    )

    out$Estimate[-1] <- out$Estimate[-1] + mod.coef['(Intercept)', 'Estimate']
    row.names(out) <- NULL
    colnames(out) <- c('PRO Score', 'Variable', 'Known-Health Group', 'N', 'Mean', 'P-value') # df colnames are forced to have periods

    # Drop the intercept:
    out[1, 'P-value'] <- NA

    # Pass the Model R-Squared for use in the footnote of the table
    mod.r2 <- sprintf("R-squared value: %.2f", summary(mod)$r.squared)

}


    # Null hypothesis - add footnote to table:
    nh1 <- 'Null hypothesis: mean of Known-Health Group not different from Reference Group.'
    # different version:
    nh2 <- 'P<0.05 indicates known-health group is significantly different from reference group'



    return(list('fn1.nh' = nh1,
                'fn2.nh' = nh2,
                'mod.r2' = mod.r2,
                'out.descr' = out.descr,
                'out.mod' = out,
                'PRO.score' = y,
                'val.var' = x))



} #end function
