#' Plots with shared legend eCDF using `ggplot2`
#'
#' Create multiple `ggplot2` plots with a shared legend
#'
#' This is borrowed directly from https://rpubs.com/sjackman/grid_arrange_shared_legend
#' It requires `ggplot2`, `grid`, and `gridExtra`
#'
#' @param pass the plots pass the dataframe
#' @return ggplot2 with a shared legend
#' @export




grid_arrange_shared_legend <- function(...) {

    plots <- list(...)
    g <- ggplot2::ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    gridExtra::grid.arrange(
        do.call(arrangeGrob, lapply(plots, function(x)
            x + theme(legend.position="none"))),
        legend,
        ncol = 1,
        heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}
