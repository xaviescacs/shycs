#' Plot a 1D distribution in a horizontal space
#' 
#' The goal of this modules is o able to plot the distribution of a one-dimensional vector in
#' the narrowest possible space (in the vertical axis), using only, or mainly, the horizontal space.
#' 
#' @section Rationale:
#' 
#' The idea behind this is to be able to glimpse the distribution of a vector at a glance, in a
#' non-invasive way, occuping the least space possible on a dashboard or a panel, so the user can
#' have a summarized view of the data.
#' 
#' @section Multiple calls; limits:
#' 
#' This modules is expected to be called many times inside a Shiny app, however, when doing so, if the
#' range of values varies, the subsequent visualization may be hard to compare, making difficult for
#' the user to envision the evolution of the samples.
#' 
#' To tackle this problem, one possible solution is to set the same limits for a series of samples that
#' are expected to be compared by the user. To be able to do this, the module accepts the horizontal (x) 
#' limits of the plot. However, bear in mind that this modules is stateless, and therefore, this behaviour
#' should be implemented by the calling module.
#' 
#' @param id Character. Module's ID.
#' 
#' @importFrom shiny NS tag plotOutput is.reactive moduleServer renderPlot 
#' @export
#' 
#' @rdname mod_plot_dist_oneline
#' @family Shiny modules
plotDistOnelineUI <- function(id){
    plotOutput(NS(id,"distribution"))
}

#' @rdname mod_plot_dist_oneline
#' 
#' @param x Numeric. One-dimensional vector of which the distribution is to be ploted.
#' @param limits Numeric. Length two numeric vector with the horizontal limits of the plot.
#' 
#' @export
plotDistOnelineServer <- function(id,x,limits = NULL) {
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
    output$distribution <- renderPlot({
      req(is.numeric(x()))
      plot_dist_oneline(x(),limits)
    })
  })
}