#' Merge Multiple Chart Lists
#'
#' Combines chart lists by merging charts within each MetricID. Used to combine
#' default charts from [gsm.kri::MakeCharts()] with custom charts from [Widget()].
#'
#' @param ... Two or more chart lists to merge. Each should be a list keyed by
#'   MetricID (the workflow `meta.ID` string), where each element is a list of
#'   charts. Arguments do not need to be named.
#'
#' @return A list keyed by MetricID containing all charts from all input lists
#'   for each metric. Metrics present in only some inputs are included unchanged.
#'
#' @export
Merge_ChartLists <- function(...) {
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
