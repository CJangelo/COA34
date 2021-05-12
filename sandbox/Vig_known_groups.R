

rm(list = ls())
gc()

# library(devtools)
# devtools::install()
# devtools::document()
library(COA34)
library(rcompanion) # super annoyed that I can't figure this out
#library(emmeans)
set.seed(5112021)

# Generate data
sim.out <- COA34::sim_pro_dat(N=1e3,
                        polychor.value = 0.4,
                        corr = 'ar1',
                        cor.value = 0.8,
                        var.values = c(5))

dat <- sim.out$dat

# Implement drop-out
dat <- COA34::dropout(dat = dat,
                      type_dropout  = c('mcar', 'mar', 'mnar'),
                      prop.miss = 0.5,
                      stochastic.component = 0.2)

# Emphasize in Vignette the importance of checking variables:
str(dat)
# PGIS is ordinal; transform it for the purposes of correct correlation
# Leave it numeric to compute the PGIS_delta for the change scores later
dat.cor <- dat
dat.cor$PGIS <-
  factor(dat.cor$PGIS,
         levels = c(0, 1, 2, 3, 4),
         labels = c('None', 'Mild', 'Moderate', 'Severe', 'Very Severe'))
table(dat.cor$PGIS, useNA = 'always')
xtabs(~ dat.cor$PGIS + dat$PGIS)

# Validator Variables 1 and 5 are supposed to be
# ordered categorical variables
dat.cor$Val_1 <- factor(dat.cor$Val_1)
table(dat.cor$Val_1, useNA = 'always')
# only collected at baseline, hence the NAs

dat.cor$Val_5 <- factor(dat.cor$Val_5)
table(dat.cor$Val_5, useNA = 'always')
# Only collected at baseline, hence the NAs

# Check data:
str(dat.cor)
# 5 validator variables (Val_1, Val_2...Val_5)
# 4 PRO scores

out <- COA34::compute_known_groups_validity(dat = dat.cor,
                                            PRO.score = 'Y_comp',
                                            val.var = 'Val_5',
                                            time.var = 'Time')
out <- out$out.mod


library(R2Word)
R2Word::shell_table(out = out, cols = 4:6, decimals = c(0,2,3), NA.string = '-')

R2Word::dump_df_mat_to_file(out$out.mod,
                            NA.string = '-',
                            decimals = c(0, 2, 3),
                            table.footnote = out$fn2.nh)

#---------------------------------------------------------
# Why are we reporting out the 95% Confidence Interval?
# It's just confusing to them.
# We report the means of the known-health groups
# We report whether they are different from the Reference Group
# also do the medians, remember!

time.var <- 'Time'
PRO.score <- c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar')
val.var <- c('PGIS', 'Val_1', 'Val_5')
dat <- dat.cor


bl <- sort(unique(dat[,time.var, drop = T]), decreasing = F)[1]
dat <- dat[which(dat[,time.var] == bl), ]

y <- PRO.score[1]
#y <- val.var[2]
x <- val.var[1]


# Recent FDA response asked for medians as well as means here - confirm!
  y <- PRO.score[1]; x <- val.var[1]
out1 <- do.call(data.frame, stats::aggregate(as.formula(paste0(y, '~', x)),
        FUN = function(x) c(sum(!is.na(x)),
                            mean(x, na.rm = T),
                            median(x, na.rm = T)),
        data = dat.cor, na.action = na.pass))

out1 <- data.frame(y, x, out1)
colnames(out1) <- c('PRO Score', 'Variable', 'Known-Health Group', 'N', 'Mean', 'Median')
out1

##
  y <- val.var[2]; x <- val.var[1]
out2 <- stats::xtabs(as.formula(paste0('~', x, '+', y)),
                                data = dat.cor,
                                na.action = na.pass)
out2
#as.data.frame(out2)
out2 <- addmargins(out2, 2)
out2 <- as.data.frame.matrix(out2)
out2
out2 <- data.frame(rownames(out2), out2)
tmp <- paste0(levels(dat[,y, drop = T]))
tmp <- paste0('N,', y, '=', tmp)
colnames(out2) <- c( x, tmp, 'Total N')
out2



if (!is.numeric(dat[,y, drop = T])) {


  # Fit the Model:
    mod <- stats::glm(as.formula(paste0(y, '~', x)), data = dat, family = 'binomial')
    mod.coef <- summary(mod)$coef
    mod.coef <- as.data.frame(mod.coef)
    mod.coef$Estimate <- exp(mod.coef$Estimate)


    # n <- length(dat[, y, drop = T])
    # mod.null <- stats::glm(as.formula(paste0(y, '~ 1')), data = dat, family = 'binomial')
    # llnull <- logLik(mod.null)
    # model.lr <- logLik(mod.null) - logLik(mod)
    # r2 <- 1-exp(-model.lr/n)
    # r2.max <- 1-exp(-llnull/n)
    # r2 <- r2/r2.max
    # r2 <- as.numeric(r2)

    r2 <- rcompanion:::nagelkerke(mod)$Pseudo.R.squared.for.model.vs.null
    r2 <- r2[grepl('Nagelkerke', rownames(r2)), ]


# # Check:
# xtabs( as.formula(~ get(x) + get(y)), data = dat)
# 1/(82/(55+62))
# (82/(55+62))/(67/(56+64))
# (82/(55+62))/(65/(65+80))
# (82/(55+62))/(67/(76+60))
# (82/(55+62))/(53/(81+67))
# mod.coef

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
      #'conf.int' = coef.ci,
      'P-value' = mod.coef[ , 'Pr(>|z|)', drop = T],
      stringsAsFactors=F
    )

row.names(out) <- NULL
colnames(out) <- c('PRO Score','Variable', 'Known-Health Group', 'Odds-ratio', 'P-value') # df colnames are forced to have periods

# Drop the intercept:
out[1, c('Odds-ratio', 'P-value')] <- NA
# Drop the odds ratio because it's the OR for whether
# the reference group OR is different from 1
# The other OR are if the group is different from the reference group
# So the interpetation will be strange if you leave it in

# Pass the Model Pseudo R-Squared for use in the footnote of the table
mod.r2 <- sprintf("Pseudo R-squared value: .2f%%", r2)



} #end glm


if (is.numeric(dat[,y, drop = T])) {
    # Y is continuous score:
    mod <- stats::lm(as.formula(paste0(y, '~', x)), data = dat)
    mod.coef <- summary(mod)$coef
    anova(mod)

    # Compute 95% confidence intervals
    # coef.ci <- stats::confint(mod)
    # coef.ci <- apply(coef.ci[, c('2.5 %', '97.5 %')], 1, function(x)
    #   paste0(sprintf("%.2f", x), collapse = ', '))
    # coef.ci <- paste0('(', coef.ci, ')')

    # Pull the labels from the Reference Measure:
    ref.meas.labels <- levels(dat[ , x])


    # Create DF
    out <-
    data.frame(
      'Score' = y,
      'Variable' = x,
      'Known-Health Group' = c(paste0(ref.meas.labels[1], ' - Reference Group'),
                                  paste0(ref.meas.labels[-1])),
      'Estimate' = mod.coef[ , 'Estimate'],
    #  'conf.int' = coef.ci,
      'P-value' = mod.coef[ , 'Pr(>|t|)'],
      stringsAsFactors=F
    )

    out$Estimate <- out$Estimate + mod.coef['(Intercept)', 'Estimate']
    row.names(out) <- NULL
    #colnames(out) <- c('PRO Score', 'Variable', 'Known-Health Group', 'Mean', '95% Confidence Interval', 'P-value') # df colnames are forced to have periods
    colnames(out) <- c('PRO Score', 'Variable', 'Known-Health Group', 'Mean', 'P-value') # df colnames are forced to have periods

    # Drop the intercept:
    out[1, 'P-value'] <- NA

    # Pass the Model R-Squared for use in the footnote of the table
    mod.r2 <- sprintf("R-squared value: %.2f%%", summary(mod)$r.squared)

}


    # Null hypothesis - add footnote to table:
    nh1 <- 'Null hypothesis: mean of Known-Health Group not different from Reference Group.'
    # different version:
    nh2 <- 'P<0.05 indicates known-health group is significantly different from reference group'


out
mod.r2
nh1
nh2
