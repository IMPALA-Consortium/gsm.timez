#' Calculate Predicted Bounds for TimeZScore Visualization
#'
#' This function calculates predicted Metric bounds for each month based on
#' the cumulative mean and standard deviation from \code{\link{TimeZScore}}.
#' The bounds are used for visualization in \code{\link{Visualize}}.
#'
#' @param dfAnalyzed A data frame output from \code{\link{TimeZScore}}. Must
#'   contain columns: \code{NMonth}, \code{GroupID}, \code{metric_mean}, and
#'   \code{metric_sd}.
#' @param vThreshold Numeric vector of threshold values for bounds. Default is
#'   \code{c(-3, -2, 2, 3)}. A threshold of 0 (for the mean line) is
#'   automatically included.
#' @param nMinSiteFraction Minimum fraction of peak site count required to
#'   return non-\code{NA} bounds for a month. Months where the number of
#'   distinct \code{GroupID}s is below this fraction of the maximum are
#'   returned with \code{Metric = NA}. Default is \code{0.2} (20\%).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{NMonth}: Time point (month number).
#'     \item \code{Threshold}: Threshold value (e.g., -3, -2, 0, 2, 3).
#'     \item \code{Metric}: Predicted Metric value at this threshold,
#'       calculated as \code{metric_mean + Threshold * metric_sd}.
#'   }
#'
#' @details
#' This function follows the GSM ecosystem convention of calculating bounds
#' separately from visualization. The bounds can then be passed to
#' \code{\link{Visualize}} via the \code{dfBounds} parameter.
#'
#' The predicted Metric at each threshold is calculated as:
#' \deqn{Metric = metric\_mean + Threshold \times metric\_sd}
#'
#' @seealso \code{\link{TimeZScore}} for calculating scores,
#'   \code{\link{Visualize}} for plotting with bounds.
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
#' dfAnalyzed <- Timeline(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strGroupCol = "SiteID",
#'   strSubjectCol = "SubjectID",
#'   strNumeratorDateCol = "EventDate",
#'   strDenominatorDateCol = "VisitDate"
#' ) %>%
#'   TimeZScore()
#'
#' dfBounds <- TimeZScore_PredictBounds(dfAnalyzed)
#'
#' @export
TimeZScore_PredictBounds <- function(dfAnalyzed, vThreshold = c(-3, -2, 2, 3), nMinSiteFraction = 0.2) {
  # Validate input columns
  required_cols <- c("NMonth", "GroupID", "metric_mean", "metric_sd")
  stopifnot(all(required_cols %in% names(dfAnalyzed)))

  # Include 0 threshold for mean line if not already present
  all_thresholds <- sort(unique(c(vThreshold, 0)))

  # Identify sparse months (fewer than nMinSiteFraction of peak site count)
  sparse_months <- GetSparseMonths(dfAnalyzed, nMinSiteFraction)

  # Get unique NMonth with their stats and expand to all thresholds
  dfAnalyzed %>%
    dplyr::distinct(.data$NMonth, .data$metric_mean, .data$metric_sd) %>%
    tidyr::expand_grid(Threshold = all_thresholds) %>%
    dplyr::mutate(
      Metric = .data$metric_mean + .data$Threshold * .data$metric_sd
    ) %>%
    dplyr::left_join(sparse_months %>% dplyr::select("NMonth", "sparse"), by = "NMonth") %>%
    dplyr::mutate(Metric = dplyr::if_else(.data$sparse, NA_real_, .data$Metric)) %>%
    dplyr::select("NMonth", "Threshold", "Metric") %>%
    dplyr::arrange(.data$NMonth, .data$Threshold)
}
