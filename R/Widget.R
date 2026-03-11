#' Generic Widget for Timeline Visualizations
#'
#' Creates interactive plotly charts for each metric by applying a visualization
#' function to stacked results data. The visualization function is specified as
#' a string (e.g., "gsm.timez::Visualize") and resolved at runtime.
#'
#' @param dfResults A stacked data frame with a MetricID column, typically
#'   Reporting_Results_allmonths from [gsm.reporting::BindResults()].
#' @param strVisualizeFun Character string specifying the visualization function
#'   to apply, including namespace (e.g., "gsm.timez::Visualize").
#' @param strOutputLabel Character string for the chart tab label in the report
#'   (e.g., "Timeline", "Distribution").
#' @param strIcon Optional Font Awesome icon name (e.g., "chart-line"). If
#'   provided, the icon is prepended to the tab label.
#' @param ... Additional arguments passed to the visualization function.
#'
#' @return A named list of plotly htmlwidget objects, keyed by MetricID.
#'
#' @export
Widget <- function(dfResults, strVisualizeFun, strOutputLabel, strIcon = NULL, ...) {
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
      p <- fnVisualize(dfMetric, ...)
      chart <- plotly::ggplotly(p)
      attr(chart, "output_label") <- strLabel
      stats::setNames(list(chart), strOutputLabel)
    }) %>%
    stats::setNames(strMetrics)

  lCharts
}
