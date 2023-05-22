#' Plotter for IMPACT results at FPU level
#'
#' @param df sf dataframe object. Usually an output from from readFPU()
#' @param wrap If the plot needs to be wrapped e.g., in case plotting multiple
#' indicators. For example, use "fctr" if you want to plot irrigated and rainfed
#' areas in case area is being plotted
#' @param ncol Number of columns to be plotted. Defaults to NULL.
#' @param plot_title Title of the plot. Defaults to NULL.
#' @param legend_title Title of the legend, Defaults to NULL.
#' @param scale Numeric value to scale output. Use 1e3 for thousand to million
#' conversion for example. Defaults to 1.
#'
#' @return ggplot object with FPU level IMPACT results
#' @import ggplot2
#' @importFrom ggthemes theme_map
#' @importFrom viridis scale_fill_viridis
#' @importFrom stats reformulate
#' @importFrom utils head
#' @export
#' @examples
#' \dontrun{
#' plotSpatial(df)
#' }
plotSpatial <- function(df, wrap = NULL, ncol = NULL, scale = 1,
                        plot_title = NULL, legend_title = NULL) {

    value <- NULL

    p <- ggplot() +
        geom_sf(data = df, aes(fill = value / scale)) +
        scale_fill_viridis(option = "magma", direction = -1) +
        ggtitle(plot_title) +
        labs(fill = legend_title) +
        theme_map(base_size = 25) +
        theme(strip.background  = element_blank(),
              legend.background = element_rect(fill = "transparent"))
    return(p)
}
