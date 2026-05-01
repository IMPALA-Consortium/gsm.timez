#' Generic Widget for Timeline Visualizations
#'
#' Creates charts for each metric by applying a visualization function to
#' stacked results data. The visualization function is specified as a string
#' (e.g., "gsm.timez::Visualize_Heatmap") and resolved at runtime.
#'
#' @param dfResults A stacked data frame with a MetricID column, typically
#'   Reporting_Results_allmonths from [gsm.reporting::BindResults()].
#' @param strVisualizeFun Character string specifying the visualization function
#'   to apply, including namespace (e.g., "gsm.timez::Visualize_Heatmap").
#' @param strOutputLabel Character string for the chart tab label in the report
#'   (e.g., "Timeline", "Distribution").
#' @param strIcon Optional Font Awesome icon name (e.g., "chart-line"). If
#'   provided, the icon is prepended to the tab label.
#' @param bInteractive Logical. If TRUE (default), creates fully interactive
#'   plotly charts. If FALSE, creates static plotly charts (no zoom, pan, hover)
#'   which render faster and produce smaller HTML output. Use FALSE for
#'   visualizations with many elements (e.g., heatmaps).
#' @param dfBounds Optional data frame with a \code{MetricID} column (e.g.,
#'   \code{Reporting_Bounds}). When provided, it is filtered to the current
#'   MetricID and passed as the second positional argument to the visualization
#'   function, before \code{...}. Use this for functions like
#'   \code{\link{Visualize_Funnel}} that require a separate bounds data frame.
#' @param ... Additional arguments passed to the visualization function.
#'
#' @return A named list of plotly htmlwidget objects, keyed by MetricID.
#'
#' @importFrom fontawesome fa
#' @export
Widget <- function(dfResults, strVisualizeFun, strOutputLabel, strIcon = NULL,
                   bInteractive = TRUE, dfBounds = NULL, ...) {
  fnVisualize <- eval(parse(text = strVisualizeFun))
  strMetrics <- unique(dfResults$MetricID)

  strLabel <- if (!is.null(strIcon)) {
    paste0(fontawesome::fa(strIcon, fill = "#337ab7"), " ", strOutputLabel)
  } else {
    strOutputLabel
  }

  lCharts <- strMetrics %>%
    purrr::map(function(metric) {
      dfMetric <- dfResults %>% dplyr::filter(.data$MetricID == metric)
      if (!is.null(dfBounds)) {
        dfBoundsMetric <- dfBounds %>% dplyr::filter(.data$MetricID == metric)
        p <- fnVisualize(dfMetric, dfBoundsMetric, ...)
      } else {
        p <- fnVisualize(dfMetric, ...)
      }

      if (bInteractive) {
        chart <- plotly::ggplotly(p)
      } else {
        chart <- plotly::ggplotly(p) %>%
          plotly::config(staticPlot = TRUE)
      }

      attr(chart, "output_label") <- strLabel
      stats::setNames(list(chart), strOutputLabel)
    }) %>%
    stats::setNames(strMetrics)

  lCharts
}
