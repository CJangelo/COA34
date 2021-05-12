
rm(list = ls())
gc()

#library(devtools)
#devtools::install()
#devtools::document()

library(COA34)
library(polycor) # Add to Description file

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


out <- COA34::compute_validity(dat = dat.cor,
                               time.var = 'Time',
                               PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'),
                               val.var = c('PGIS', paste0('Val_', 1:5))
                               )
out

out1 <- COA34::compute_validity(dat = dat.cor,
                               time.var = 'Time',
                               PRO.score = c('Y_comp'),
                               val.var = c('PGIS', paste0('Val_', 1:5))
                               )
sim.out$out.val$cor.mat.ref
out1

# Scatterplot of Correlations:
library(ggplot2)
p1 <- ggplot(out1, aes(x = `Validator Variable`, y = Correlation)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  scale_y_continuous(lim = c(0,1), breaks = seq(0, 1, by = 0.1))

library(ggrepel)
p2 <- p1 + ggrepel::geom_text_repel(aes(label = `Validator Variable`),
              size = 3.5)
p2

# How to customize the labels
out1$label <-
  ifelse(out1$`Validator Variable` == 'PGIS', 'PGIS anchor',
         ifelse(out1$`Validator Variable` == 'Val_1', 'PGIC',
         ifelse(out1$`Validator Variable` == 'Val_2', 'SF-36',
         ifelse(out1$`Validator Variable` == 'Val_3', 'EQ-5D',
         ifelse(out1$`Validator Variable` == 'Val_4', 'NSCLC-SAQ',
         ifelse(out1$`Validator Variable` == 'Val_5', 'SF-36, Physical Functioning',
                NA))))))

library(ggrepel)
p3 <- p1 + ggrepel::geom_text_repel(aes(label = `label`),
              size = 3.5)
p3

# The compute_validity() function will automatically select the baseline
# timepoint - if you want to use a different timepoint, you'll have to
# pass a dataframe with only that data!
out2 <- COA34::compute_validity(dat = dat.cor[dat.cor$Time == 'Time_4', ],
                               time.var = 'Time',
                               PRO.score = c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar'),
                               val.var = c('PGIS')
                               )

out2

#----------------------------------------
# Code below was/is basis for the `compute_validity()` function
# This can go in a Vignette for illustration and for flexbility
#
time.var <- 'Time'
PRO.score <- c('Y_comp', 'Y_mcar', 'Y_mar', 'Y_mnar')
val.var <- c('PGIS', paste0('Val_', 1:5))

bl <- sort(unique(dat.cor[,time.var, drop = T]), decreasing = F)[1]
dat.cor <- dat.cor[which(dat.cor[,time.var] == bl), ]
#table(dat.cor$Val_5, useNA = 'always')

out <- vector()

for (i in PRO.score) {
  for (j in val.var) {

    tmp <- dat.cor[, c(i, j)]
    tmp <- tmp[complete.cases(tmp),]
    n <- nrow(tmp)
    x <- tmp[,i]
    y <- tmp[,j]

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

colnames(out) <- c('PRO Score', 'Validator Variable', 'Correlation Type', 'Sample Size', 'Correlation')
out
