stats::aggregate(Y_comp ~ anchor.groups + Time,
                                  function(x) c('mean' = mean(x, na.rm = T),
                                                'median' = median(x, na.rm =T)),
                                  data = dat,
                                  na.action = na.pass)


str(dat)
# KNown Groups Validity
tmp <- dat[dat$Time == 'Time_1', ]

mod <- lm(Y_comp ~ as.factor(PGIS_bl), data = tmp)
summary(mod)

sim.out$Beta
sim.out$cor.mat


cor(dat[dat$Time == 'Time_1', 'Y_comp' ], dat[dat$Time == 'Time_2', 'Y_comp'])
cor(dat[dat$Time == 'Time_1', 'Y_comp' ], dat[dat$Time == 'Time_2', 'Y_comp_delta'])


cor(dat[dat$Time == 'Time_1', 'Y_comp' ], dat[dat$Time == 'Time_3', 'Y_comp'])
cor(dat[dat$Time == 'Time_1', 'Y_comp' ], dat[dat$Time == 'Time_3', 'Y_comp_delta'])

cor(dat[dat$Time == 'Time_1', 'Y_comp' ], dat[dat$Time == 'Time_4', 'Y_comp'])
cor(dat[dat$Time == 'Time_1', 'Y_comp' ], dat[dat$Time == 'Time_4', 'Y_comp_delta'])
