#' Interactive Timeline Boxplot Widget
#'
#' Converts the ggplot2 output from [VisualizeBoxplot()] to an interactive
#' plotly widget with hover tooltips, zooming, and panning.
#'
#' @param dfFlagged A data frame output from [Flag()].
#' @param nMonths Optional numeric vector of months to include. If NULL
#'   (default), all months are shown.
#' @param ... Additional arguments passed to [plotly::ggplotly()].
#'
#' @return A plotly htmlwidget object.
#'
#' @seealso [VisualizeBoxplot()] for the underlying ggplot2 visualization.
#'
#' @export
Widget_TimelineBoxplot <- function(dfFlagged, nMonths = NULL, ...) {
  p <- VisualizeBoxplot(dfFlagged, nMonths = nMonths)
  plotly::ggplotly(p, ...)
}
