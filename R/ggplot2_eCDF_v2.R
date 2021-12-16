#' Plot eCDF using `ggplot2`
#'
#' Revised version 2
#'
#' Plot the eCDF consistent with FDA COA Guidance 3, using `ggplot2` R package
#'
#' @param dat pass the dataframe
#' @param smoothed.ecdf if TRUE, uses the smoothed density to plot the eCDF; default is FALSE
#' @param anchor.group variable of the anchor group you want used here
#' @param anchor.group.colors if you have unique anchor groups, you need to assign
#' colors to those anchor groups, such as c('Deteriorated_1' = 'red', 'Improved_1' = 'green')
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param change.score indicate the name of the PRO change score
#' @param shell.table logical, if true the sample size will be "xx", can be used as shell tables
#' @return Plot of eCDF using ggplot2 package
#' @export



ggplot2_eCDF_v2 <- function(
                        dat = NULL,
                        smoothed.ecdf = F,
                        anchor.group = NULL, #'anchor.groups',
                        anchor.group.colors = NULL,
                        time.var = NULL, # 'Time',
                        change.score = NULL, # 'Y_comp_delta',
                        shell.table = FALSE
                        ){


    if (is.null(dat)) stop('Please specify dataframe in `ggplot2_eCDF ()` ')
    if (is.null(anchor.group)) stop('Please specify anchor.group in `ggplot2_eCDF ()` ')
    if (is.null(time.var)) stop('Please specify time.var in `ggplot2_eCDF ()` ')
    if (is.null(change.score)) stop('Please specify change.score in `ggplot2_eCDF ()` ')



#-------------------------------------------
  # Compute densities for eCDF
  if (smoothed.ecdf == T) {
    dat.den <- COA34::compute_density_smoothed(dat = dat,
                                             anchor.group = anchor.group,
                                             time.var = time.var,
                                             change.score = change.score)
  } else {

    dat.den <- COA34::compute_ecdf(dat = dat,
                                   anchor.group = anchor.group,
                                   time.var = time.var,
                                   change.score = change.score)
  }


#------------------------------------------------
  # Thresholds
  thr <- COA34::compute_thresholds(dat = dat,
                                   anchor.group = anchor.group,
                                   time.var = time.var,
                                   change.score = change.score)

  thr$legend.label <- paste0(thr$`Anchor Group`, " (N=",thr$N,")")

  if (shell.table) thr$legend.label <- paste0(thr$`Anchor Group`, " (N=xx)")



#---------------------------------------------------
# Colors and Legend labels:

  if (is.null(anchor.group.colors)) {

    anchor.group.colors <- c("Deteriorated, 2 or more categories" = "darkred",
                             "Deteriorated, 1 category" = "red",
                             'Maintained' = 'blue',
                             'Improved, 1 category' = 'green',
                             'Improved, 2 or more categories' = 'darkgreen')

  }





#---------------
# range
rr <- c(floor(min(dat.den$density_x, na.rm = T)),
        ceiling(max(dat.den$density_x, na.rm = T)))



#---------------------------------
#
# PLOT eCDF
#
#--------
# eCDF
p1 <- ggplot2::ggplot(
  dat.den, aes(x=density_x, y=CDF, group=anchor.groups, color=anchor.groups)) +
  geom_line() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 1, by = .1), limits = c(0, 1)) +
  scale_x_continuous(breaks = rr[1]:rr[2], limits = rr) +

  scale_color_manual(name = 'Anchor Groups',
                     values = anchor.group.colors,
                     labels = setNames(thr$legend.label, thr$`Anchor Group`)) +

  labs( title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
        x = 'Change in score from baseline',
        y = 'Cumulative proportion of patients') +

  geom_hline(yintercept = 0.50, linetype = 'dotted')


return('ecdf' = p1)




}
