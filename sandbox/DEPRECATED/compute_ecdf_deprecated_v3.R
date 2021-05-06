#' Plot eCDF
#'
#' Plot the eCDF consistent with FDA COA Guidance 3
#'
#' @param dat pass the dataframe, must contain the anchor group and the change score
#' @param anchor.group indicate the name of the anchor group
#' @param change.score indicate the name of the PRO change score
#' @param threshold
#' @param plot.title
#' @param plot.xlab
#' @param plot.ylab
#' @param plot.colors
#' @param file.name
#' @return Plot of eCDF
#' @export



plot_ecdf <- function(
                        dat = NULL,
                        anchor.group = NULL, # you can call Group whatever you want
                        change.score = NULL,
                        threshold = NULL, # pass the threshold for meaningful change or it will be computed for you
                        plot.title = 'Empirical Cumulative Distribution of Change in Score from Baseline',
                        plot.xlab = 'Change in score from baseline',
                        plot.ylab = 'Cumulative proportion of patients',
                        plot.group.labels = NULL, #c('Deteriorated_1' = 'Deteriorated', 'Maintained' = 'Deteriorated', 'No Change',  'Improved'),
                        plot.colors = NULL, #c('No Change' = 'black', 'Deteriorated' = 'red', 'Improved' = 'blue'), #Pass plot colors
                        file.name = 'Example_eCDF'
                        #plot.conf.int = TRUE
                        ){


    if (is.null(dat)) stop('Please specify dataframe in `plot_ecdf()` ')
    if (is.null(anchor.group)) stop('Please specify anchor.group in `plot_ecdf()` ')
    if (is.null(threshold)) stop('Please specify threshold in `plot_ecdf()` ')
    if (is.null(change.score)) stop('Please specify change.score in `plot_ecdf()` ')
    if (is.null(plot.colors)) stop('Please specify plot.colors in `plot_ecdf()` ')


    # initiate plot
    plot(NULL,
          xlim = range(dat$density_x, na.rm = T),
            ylim = c(0, 1),
              yaxt = 'n',
              type = 'l', lwd = 3, col = 'black',
                main = plot.title,
                  xlab = plot.xlab,
                    ylab = plot.ylab,
                      sub = caption)


# Y axis:
axis(side = 2, at= seq(0, 1, by = 0.1), labels=sprintf("%.1f", seq(0, 1, by = 0.1)), las = 2)
# X axis
# minx <- min(dat$density_x, na.rm = T)
# maxx <- max(dat$density_x, na.rm = T)
# axis(side = 1, at= seq(minx, maxx, by = 1)) #, labels=sprintf("%.1f", seq(0, 1, by = 0.1)), las = 2)


  #loop this over the groups:
  groups <- dat[, anchor.group, drop = T]
  groups <- unique(groups)
  #if (is.null(plot.group.labels)) {
  #  plot.group.labels

it <- 1
for(gg in groups){

     # pull change scores in group
      dd <- which(dat[, anchor.group, drop = T] == gg)
      xx <- dat[dd, 'density_x', drop = T]
      yy <- dat[dd, 'density_y', drop = T]
      yy <- cumsum(yy)/sum(yy)

      lines(x = xx, y = yy, type = 'l', lwd = 3, col = it) # col = plot.colors[gg]
      it <- it + 1

}


# Add the Threshold:
abline( v = threshold, lty = 3)

# Indicate the Median/50% cumulative proportion
abline( h = 0.5, lty = 1)


# Add Legend:
cap <- COA34::compute_prop_surp(dat = dat, anchor.group = 'anchor.groups',
                     time.var = 'Time', change.score = 'Y_comp_delta',
                     threshold.type = 'Improved', mean.or.median = 'Median')

plot.legend <- apply(cap$anchor.table[, c('Anchor Group', 'Total')], 1,
                     function(x) paste0(x, collapse = ', N = '))

plot.caption = cap$ecdf.caption


legend(x = min(dat$delta, na.rm = T), y = 1, legend = tmp, col = plot.colors[plot.groups], lty = 1, cex = 0.8)



  #return(dat)


}
