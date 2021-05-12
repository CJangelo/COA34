
# The most important part of doing analyses in R, especially these
# psychometric analyses, is data management. All the functions do
# the work, you just have to keep track of your data management.
# You should constantly be using
# str(dat), levels(dat$variable), etc
# keep a close eye on whether a variable is numeric or a factor
# if it's a factor, check the levels and check the ordering
# Do this often to avoid a problem - the function won't throw an error,
# you'll just get wrong results
# Be paranoid.


rm(list = ls())
gc()

library(COA34)
set.seed(5102021)


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


# Two useful base R functions (actually in stats package)
# aggregate() and xtabs()

# Item Response Distribution
out1 <- do.call(data.frame, stats::aggregate(Y_mar ~ Time,
        FUN = function(x) c(sum(!is.na(x)),
                            mean(x, na.rm = T),
                            sd(x, na.rm = T),
                            quantile(x, na.rm = T)),
        data = dat, na.action = na.pass))
# note: be careful of NA handling
colnames(out1) <- c('Time', 'N', 'Mean', 'SD', '0%', '25%', '50%', '75%', '100%')
out1

# Cross-Tabs:
out2 <- stats::xtabs( ~  PGIS_delta + Time, data = dat)
# If you're using a loop, use as.formula() with paste0():
x <- 'PGIS_delta'
y <- 'Time'
stats::xtabs( as.formula(paste0('~ ',  x, ' + ', y)), data = dat)
out2 <- addmargins(out2, margin = 1)
out2

# Additonal functionality available here:
# https://cjangelo.github.io/R2Word/articles/introduction.html

# Great library:
library(dplyr)
dplyr::count(dat, Time, PGIS_bl, PGIS_delta, .drop = F)


# gtsummary functions

# scatterplots and histograms of the scores

#
