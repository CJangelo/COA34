#' Plot eCDF using `ggplot2`
#'
#' Plot the eCDF consistent with FDA COA Guidance 3, using `ggplot2` R package
#'
#' Also requires `gridExtra`
#' @param dat pass the dataframe
#' @param output.plots specify what you want printed out; 'eCDF', 'ePDF', or 'both'.
#' Default is 'both'
#' @param anchor.group variable of the anchor group you want used here
#' @param time.var variable of the Time in the dataframe - PLEASE CHECK THAT THIS IS CORRECTLY ORDERED, default is Time
#' @param change.score indicate the name of the PRO change score
#' @param file.name pass the name of the png file to print out
#' @param shell.table logical, if true the sample size will be "xx", can be used as shell tables
#' @return Plot of eCDF using ggplot2 package
#' @export



ggplot2_eCDF <- function(
                        dat = NULL,
                        output.plots = 'both',
                        anchor.group = NULL, #'anchor.groups',
                        time.var = NULL, # 'Time',
                        change.score = NULL, # 'Y_comp_delta',
                        file.name = 'Example_eCDF_ePDF_ggplot2',
                        shell.table = FALSE
                        ){


    if (is.null(dat)) stop('Please specify dataframe in `ggplot2_eCDF ()` ')
    if (is.null(anchor.group)) stop('Please specify anchor.group in `ggplot2_eCDF ()` ')
    if (is.null(time.var)) stop('Please specify time.var in `ggplot2_eCDF ()` ')
    if (is.null(change.score)) stop('Please specify change.score in `ggplot2_eCDF ()` ')



#-------------------------------------------
  # Compute densities for eCDF and ePDF
  dat.den <- COA34::compute_ecdf(dat = dat,
                                 anchor.group = anchor.group,
                                 time.var = time.var,
                                 change.score = change.score)


    thr <- COA34::compute_thresholds(dat = dat,
                                   anchor.group = anchor.group,
                                   time.var = time.var,
                                   change.score = change.score)


    plot.legend <- apply(thr[, c('Anchor Group', 'N')], 1,
                         function(x) paste0(x[1], ' (N = ', x[2], ')'))


    if (shell.table) plot.legend <- apply(thr[, c('Anchor Group'), drop = F ], 1, function(x) paste0(x, ' (N = xx)'))



# PLOT eCDF
p1 <- ggplot2::ggplot(dat.den, aes(x=density_x, y=CDF, group=anchor.groups, color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs( title = 'eCDF of Change in Score from Baseline',
          legend.title = 'Anchor Groups',
          #caption = cap$ecdf.caption,
          x = 'Change in score from baseline',
          y = 'Cumulative proportion of patients',
          color = 'Anchor Groups') +
    scale_color_manual(labels = plot.legend,
                     values = unique(dat.den$anchor.groups))


p2 <- ggplot2::ggplot(dat.den, aes(x=density_x, y=density_y, group=anchor.groups, color=anchor.groups)) +
    geom_line() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))+
    labs( title = 'ePDF of Change in Score from Baseline',
          x = 'Change in score from baseline',
          y = 'Density',
          color = 'Anchor Groups') +
        scale_color_manual(labels = plot.legend,
                     values = unique(dat.den$anchor.groups))

# Drop a legend:
  #p3 <- p1 + theme(legend.position = 'none')
  #out <- gridExtra::grid.arrange(p3, p2, ncol = 2)


# Push it to a file:
png(file = paste0(file.name, '.png'), units="in", width=11, height=8.5, res=300)

if (output.plots == 'eCDF') p1
if (output.plots == 'ePDF') p2
if (output.plots == 'both') gridExtra::grid.arrange(p1, p2, ncol = 1)
#grid_arrange_shared_legend(p1, p2) # shared legend

dev.off()


print(paste0('Path of PNG file: ', getwd(), '/', file.name))


  return(list('eCDF' = p1,
              'ePDF' = p2)
         )


}
