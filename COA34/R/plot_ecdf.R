#' Plot eCDF
#'
#' Plot the eCDF consistent with FDA COA Guidance 3, using base R plot functions.
#' This will be deprecated soon; inferior to `ggplot2` version.
#'
#' @param dat pass the dataframe
#' @param anchor.group variable of the anchor group you want used here
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param change.score indicate the name of the PRO change score
#' @param plot.title character vector to use as the plot title
#' @param plot.xlab character vector to pass to the xlab in base R plot functions
#' @param plot.ylab character vector to pass to the ylab in base R plot functions
#' @param file.name pass the name of the png file to print out
#' @param shell.table logical, if true the sample size will be "xx", can be used as shell tables
#' @return Plot of eCDF
#' @export



plot_ecdf <- function(
                        dat = NULL,
                        anchor.group = NULL, #'anchor.groups',
                        time.var = NULL, # 'Time',
                        change.score = NULL, # 'Y_comp_delta',
                        plot.title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
                        plot.xlab = 'Change in score from baseline',
                        plot.ylab = 'Cumulative proportion of patients',
                        threshold.value = NULL, # not required!
                        #plot.group.labels = NULL, #c('Deteriorated_1' = 'Deteriorated', 'Maintained' = 'Deteriorated', 'No Change',  'Improved'),
                        #plot.colors = NULL, #c('No Change' = 'black', 'Deteriorated' = 'red', 'Improved' = 'blue'), #Pass plot colors
                        file.name = 'Example_eCDF',
                        shell.table = FALSE
                        ){


    if (is.null(dat)) stop('Please specify dataframe in `plot_ecdf()` ')
    if (is.null(anchor.group)) stop('Please specify anchor.group in `plot_ecdf()` ')
    if (is.null(time.var)) stop('Please specify time.var in `plot_ecdf()` ')
    if (is.null(change.score)) stop('Please specify change.score in `plot_ecdf()` ')


  thr <- COA34::compute_thresholds(dat = dat,
                                   anchor.group = anchor.group,
                                   time.var = time.var,
                                   change.score = change.score)

    plot.legend <- apply(thr[, c('Anchor Group', 'N')], 1,
                           function(x) paste0(x, collapse = ', N = '))

    if (shell.table) plot.legend <- apply(thr[, c('Anchor Group')], 1, function(x) paste0(x, collapse = ', N = xx'))



#---------------------------------
  # Compute legend, caption, and threshold
  # cap <- COA34::compute_prop_surp(dat = dat,
  #                                 anchor.group = anchor.group,
  #                                 time.var = time.var,
  #                                 change.score = change.score,
  #                                 threshold.label = threshold.label,
  #                                 mean.or.median = mean.or.median)
  #
  #     plot.legend <- apply(cap$anchor.table[, c('Anchor Group', 'Total')], 1,
  #                          function(x) paste0(x, collapse = ', N = '))

    # I don't see any FDA Guidance plots with this info crammed on there
    # plot.caption = cap$ecdf.caption
    # wrap_strings  <- function(vector_of_strings,width){as.character(sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")}))}
    # pc <- wrap_strings(plot.caption, width = 100)
    #
  # thr <- COA34::compute_thresholds(dat = dat,
  #                                  anchor.group = 'anchor.groups',
  #                                  time.var = 'Time',
  #                                  change.score = 'Y_comp_delta')
  #
  #     tmp <- thr[  , 'Anchor Group', drop = T] == threshold.label
  #     threshold <- thr[tmp, mean.or.median, drop = T]
  #     plot.caption <- paste0('Dotted line: Threshold = ', sprintf("%.2f", threshold))
  #

#-------------------------------------------
  # Compute densities for eCDF and ePDF
  dat.den <- COA34::compute_ecdf(dat = dat,
                                 anchor.group = anchor.group,
                                 time.var = time.var,
                                 change.score = change.score)


# PLOT eCDF
png(file = paste0(file.name, '.png'), units="in", width=11, height=8.5, res=300)

    # initiate plot
    plot(NULL,
          xlim = range(dat.den$density_x, na.rm = T),
            ylim = c(0, 1),
              yaxt = 'n', #xaxt = 'n',
              type = 'l', lwd = 3, col = 'black',
                main = plot.title,
                  xlab = plot.xlab,
                    ylab = plot.ylab)
                     # sub = plot.caption)


# Y axis:
axis(side = 2, at= seq(0, 1, by = 0.1), labels=sprintf("%.1f", seq(0, 1, by = 0.1)), las = 2)
# X axis - add back if there's an issue:
  # minx <- round(min(dat.den$density_x, na.rm = T))
  # maxx <- round(max(dat.den$density_x, na.rm = T))
  # axis(side = 1, at= seq(minx, maxx, by = 1), labels=sprintf("%.1f", seq(minx, maxx, by = 1)))


  #loop this over the groups:
  groups <- dat.den[, anchor.group, drop = T]
  groups <- unique(groups)


it <- 1
for(gg in groups){

     # pull change scores in group
      dd <- which(dat.den[, anchor.group, drop = T] == gg)
      xx <- dat.den[dd, 'density_x', drop = T]
      yy <- dat.den[dd, 'density_y', drop = T]
      yy <- cumsum(yy)/sum(yy)

      lines(x = xx, y = yy, type = 'l', lwd = 3, col = it) # col = plot.colors[gg]
      it <- it + 1

}


# Add the Threshold:
if (!is.null(threshold.value)) abline( v = threshold.value, lty = 3)

# Indicate the Median/50% cumulative proportion
abline( h = 0.5, lty = 1)


legend(x = min(dat.den$density_x, na.rm = T),
       y = 1,
       legend = plot.legend,
       col = 1:length(groups) , lwd = 2, lty = 1, cex = 0.8)

dev.off()

  #return(dat)


}
