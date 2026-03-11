#' Merge Multiple Chart Lists
#'
#' Combines chart lists by merging charts within each MetricID. Used to combine
#' default charts from [gsm.kri::MakeCharts()] with custom charts from [Widget()].
#'
#' @param ... Named chart lists to merge. Each should be a list keyed by MetricID,
#'   where each element is a list of charts.
#'
#' @return A merged list with all charts combined per metric.
#'
#' @export
MergeChartLists <- function(...) {
  lInputs <- list(...)
  allMetrics <- unique(unlist(lapply(lInputs, names)))

  lCharts <- allMetrics %>%
    purrr::map(function(metric) {
      chartsForMetric <- lapply(lInputs, function(x) x[[metric]])
      chartsForMetric <- chartsForMetric[!sapply(chartsForMetric, is.null)]
      chartsForMetric <- unname(chartsForMetric)
      do.call(c, chartsForMetric)
    }) %>%
    stats::setNames(allMetrics)

  lCharts
}
