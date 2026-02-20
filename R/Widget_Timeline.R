#' Interactive Timeline Widget
#'
#' Converts the ggplot2 output from [Visualize()] to an interactive plotly
#' widget with hover tooltips, zooming, and panning.
#'
#' @param dfFlagged A data frame output from [Flag()].
#' @param ... Additional arguments passed to [plotly::ggplotly()].
#'
#' @return A plotly htmlwidget object.
#'
#' @seealso [Visualize()] for the underlying ggplot2 visualization.
#'
#' @export
Widget_Timeline <- function(dfFlagged, ...) {
  p <- Visualize(dfFlagged)
  plotly::ggplotly(p, ...)
}
