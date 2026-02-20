#' Create Timeline Charts for Reporting
#'
#' Generates a list of interactive plotly charts for timeline data, suitable
#' for inclusion in HTML reports. This function creates both a line chart
#' (Widget_Timeline) and a boxplot (Widget_TimelineBoxplot) from the flagged
#' timeline data.
#'
#' @param dfFlagged A data frame output from Flag() containing columns:
#'   GroupID, NMonth, Metric, Numerator, Denominator, Score, and Flag.
#' @param strMetricID Optional metric identifier string. Used for naming
#'   charts when combining with other GSM metrics. Default is "Timeline".
#' @param nMonths Optional numeric vector of NMonth values to include in the
#'   boxplot. If NULL (default), all months are shown.
#' @param ... Additional arguments passed to Widget_Timeline and
#'   Widget_TimelineBoxplot.
#'
#' @return A named list containing:
#'   \describe{
#'     \item{Timeline}{Interactive line chart (plotly object)}
#'     \item{TimelineBoxplot}{Interactive boxplot (plotly object)}
#'   }
#'   List names include the MetricID if provided (e.g., "Timeline_Timeline",
#'   "Timeline_TimelineBoxplot").
#'
#' @examples
#' library(dplyr)
#' dfSubjects <- data.frame(
#'   SubjectID = c(1, 2, 3, 4),
#'   SiteID = c("A", "A", "B", "B")
#' )
#' dfNumerator <- data.frame(
#'   SubjectID = c(1, 1, 2, 3, 4, 4, 4),
#'   EventDate = as.Date(c(
#'     "2022-01-01", "2022-01-15", "2022-02-01",
#'     "2022-01-10", "2022-01-05", "2022-01-20", "2022-02-01"
#'   ))
#' )
#' dfDenominator <- data.frame(
#'   SubjectID = c(1, 1, 2, 2, 3, 3, 4, 4),
#'   VisitDate = as.Date(c(
#'     "2022-01-01", "2022-01-20", "2022-01-01", "2022-02-01",
#'     "2022-01-01", "2022-01-15", "2022-01-01", "2022-02-01"
#'   ))
#' )
#'
#' dfFlagged <- Timeline(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strGroupCol = "SiteID",
#'   strSubjectCol = "SubjectID",
#'   strNumeratorDateCol = "EventDate",
#'   strDenominatorDateCol = "VisitDate"
#' ) %>%
#'   TimeZScore() %>%
#'   Flag()
#'
#' lCharts <- MakeCharts_Timeline(dfFlagged)
#' names(lCharts)
#'
#' @export
MakeCharts_Timeline <- function(
    dfFlagged,
    strMetricID = "Timeline",
    nMonths = NULL,
    ...) {
  lCharts <- list()

  chart_line <- Widget_Timeline(dfFlagged, ...)
  chart_boxplot <- Widget_TimelineBoxplot(dfFlagged, nMonths = nMonths, ...)

  lCharts[[paste0(strMetricID, "_Timeline")]] <- chart_line
  lCharts[[paste0(strMetricID, "_Boxplot")]] <- chart_boxplot

  lCharts
}
