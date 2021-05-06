
# Do development here, then add functions
rm(list = ls())
gc()

library(devtools)
# devtools::install()
# devtools::document()

library(ggplot2)
library(grid)
library(gridExtra)
library(COA34)
library(nlme)
library(emmeans)
library(dplyr)
#set.seed(5032021)

number.repl <- 100
repl <- 4
#
comp.mean <- as.data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, paste0('PGIS_', -4:4))))
comp.mmrm <- as.data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, paste0('PGIS_', -4:4))))
comp.no.time <- as.data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, paste0('PGIS_', -4:4))))

mar.mean <- as.data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, paste0('PGIS_', -4:4))))
mar.mmrm <- as.data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, paste0('PGIS_', -4:4))))
mar.no.time <- as.data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, paste0('PGIS_', -4:4))))


for (repl in 1:number.repl){

  set.seed(as.numeric(5032021 + repl))

# Generate data
out <- COA34::sim_pro_dat(N=1000, polychor.value = 0.8)
dat <- out$dat

# Implement drop-out
dat <- COA34::dropout(dat = dat,
               type_dropout  = c('mcar', 'mar', 'mnar'),
               prop.miss = c(0, 0, 0.25, .50),
               stochastic.component = 0.2)

# You already have the PGIS_bl and PGIS_delta in the generated data,
# you wouldn't have that in a real dataset, so drop that first
dat <- dat[, !(colnames(dat) %in% c('PGIS_bl', 'PGIS_delta'))]

# Compute PGIS delta
dat <- COA34::compute_anchor_delta(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   anchor = 'PGIS')
# Compute PRO Score delta
dat <- COA34::compute_change_score(dat = dat,
                                   subject.id = 'USUBJID',
                                   time.var = 'Time',
                                   score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'))

# Compute anchor groups
dat <- COA34::compute_anchor_group(dat = dat,
                                   anchor.variable = 'PGIS_delta',
                                   number.of.anchor.groups = 5)


# Missingness - Rates of Drop out
aggregate(Y_comp_delta ~ PGIS_delta, function(x) mean(is.na(x)), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mar_delta ~ PGIS_delta, function(x) mean(is.na(x)), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
aggregate(Y_mar_delta ~ Time, function(x) mean(is.na(x)), data = dat, na.action = na.pass)


# Descriptive Statistics:
out <- aggregate(Y_comp_delta ~ PGIS_delta, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
tmp <- out$Y_comp_delta
names(tmp) <- paste0('PGIS_', out$PGIS_delta)
comp.mean <- dplyr::bind_rows(comp.mean, tmp )

out <- aggregate(Y_mar_delta ~  PGIS_delta, function(x) mean(x, na.rm = T), data = dat[dat$Time == 'Time_4',], na.action = na.pass)
tmp <- out$Y_mar_delta
names(tmp) <- paste0('PGIS_', out$PGIS_delta)
mar.mean <- dplyr::bind_rows(mar.mean, tmp )



# Complete
  mod.gls1 <- gls(Y_comp_delta ~  as.factor(PGIS_delta) + Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.exclude)
  out <- emmeans(mod.gls1, ~ PGIS_delta | Time, mode = 'df.error')
  out <- as.data.frame(out)
  out <- out[out$Time == 'Time_4', ]
  tmp <- out$emmean
  names(tmp) <- paste0('PGIS_', out$PGIS_delta)
  comp.mmrm <- dplyr::bind_rows(comp.mmrm, tmp )
#
  out <- emmeans(mod.gls1, ~ PGIS_delta, mode = 'df.error')
  out <- as.data.frame(out)
  tmp <- out$emmean
  names(tmp) <- paste0('PGIS_', out$PGIS_delta)
  comp.no.time <- dplyr::bind_rows(comp.no.time, tmp )



#
# MAR
  mod.gls2 <- gls(Y_mar_delta ~  as.factor(PGIS_delta) + Time,
                  data = dat[dat$Time != 'Time_1',],
                  correlation = corSymm(form = ~ 1 | USUBJID),    #  unstructured correlation
                  weights = varIdent(form = ~ 1 | Time),          #  freely estimate variance at subsequent timepoints
                  na.action = na.exclude)
  out <- emmeans(mod.gls2, ~ PGIS_delta | Time, mode = 'df.error')
  out <- as.data.frame(out)
  out <- out[out$Time == 'Time_4', ]
  tmp <- out$emmean
  names(tmp) <- paste0('PGIS_', out$PGIS_delta)
  mar.mmrm <- dplyr::bind_rows(mar.mmrm, tmp )

#
  out <- emmeans(mod.gls2, ~ PGIS_delta, mode = 'df.error')
  out <- as.data.frame(out)
  tmp <- out$emmean
  names(tmp) <- paste0('PGIS_', out$PGIS_delta)
  mar.no.time <- dplyr::bind_rows(mar.no.time, tmp )


  cat(paste0('Replication: ', repl, '\n'))


}# end repl


#-----------------------------
table(complete.cases(mar.mean))
table(complete.cases(mar.mmrm))
table(complete.cases(mar.no.time))
which(!(complete.cases(mar.mean)))
which(!(complete.cases(mar.mmrm)))

colMeans(comp.mean)
colMeans(comp.mmrm)
colMeans(comp.no.time)

colMeans(mar.mean, na.rm = T)
colMeans(mar.mmrm, na.rm = T)
colMeans(mar.no.time, na.rm = T)

#save.image(file = 'sim_study_1.RData')
load(file = 'sim_study_1.RData')
#save.image(file = 'sim_study_2.RData') # no dropout at time 2
