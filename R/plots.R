#' Title
#'
#' @param ... Params to be passed to `theme_grey()`.
#'
#' @returns A ggplot2 theme designed to plot a distribution over the X axis using `geom_point`
#'
#' @export
#' @examples
theme_dist_oneline <- function(...) {
  `%ggreplace%` <- ggplot2::`%+replace%` # custom %+replace% to bypass importing it
  ggplot2::theme_grey(...) %ggreplace%
    ggplot2::theme(
      aspect.ratio = 0.05,
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # panel.border = element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "none"
    )
}

#' Plot distribition in one dimension
#' 
#' The aim of this plot is to be able to picture the distribution of a one-dimensional set of values
#' using the least space possible, the horizontal line. 
#' 
#' Kernel density and interpolation is used to weight every point to be able to use the weighted 
#' value as an aesthetic that helps enhancing which regions are most likely.
#' 
#' This function is expected to be used repeatedly with different samples, and therefore, to be
#' able to visually see the changes in distribution it's desirable that the plot has the same limits 
#' over all samples. This behaviour however is beyond the scope of the function because it has no memory,
#' it's stateless. Therefore, any complex behaviour has to be controlled from outside the function.
#'
#' @param x Vector of numeric values.
#' @param limits Limits of the plot passed to `scale_x_continuous()`.
#'
#' @export
#' @examples
#' plot_dist_oneline(rexp(100,2))
#' plot_dist_oneline(rnorm(100),limits = c(-4,4))
#' 
plot_dist_oneline <- function(x,limits = NULL){
  den <- density(x)
  color_blue <- get_csdta_colours("bright_blue")
  color_red <- get_csdta_colours("red")
  color_dark <- get_csdta_colours("dark")
  color_sea <- get_csdta_colours("sea")
  x_mean <- mean(x)
  data.frame(x = x,density = approx(den$x, den$y, xout = x)$y) |> 
    dplyr::mutate(density_scaled = (density)*50) |> 
    ggplot2::ggplot(ggplot2::aes(x = x, y = 0)) +  
      ggdist::stat_halfeye(
        fill = color_sea,
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.5, 
        point_colour = NA) + 
      ggplot2::geom_boxplot(outlier.shape = NA,box.color = ,whisker.color = color_dark) +
      ggplot2::geom_point(ggplot2::aes(size = density),alpha = 0.1,color = color_blue) +
      #ggplot2::geom_vline(ggplot2::aes(xintercept = x_mean), color = color_red, size = 0.5) +
      ggplot2::geom_segment(x = x_mean, xend = x_mean, y = 0, yend = 1, color = color_red,linewidth = 0.5) +
      ggplot2::scale_x_continuous(limits = limits) +
      theme_dist_oneline()
  
  # boxplot_plot <- data.frame(x = x,density = approx(den$x, den$y, xout = x)$y) |> 
  #   ggplot2::ggplot(ggplot2::aes(x = x, y = 0)) +
      
  #     theme_dist_oneline()
  
  # patchwork::wrap_plots(density_plot,boxplot_plot,ncol = 1)
}

