#' Calculate Predicted Bounds for Analyze_TimeZFunnel Visualization
#'
#' Applies \code{gsm.core::Analyze_NormalApprox_PredictBounds()} to each month of
#' \code{dfAnalyzed}, producing funnel plot bounds for each month.
#'
#' @param dfAnalyzed A data frame output from [Analyze_TimeZFunnel()].
#'   Must contain columns: \code{NMonth}, \code{GroupID}, \code{GroupLevel},
#'   \code{Numerator}, \code{Denominator}, and \code{Metric}.
#' @param vThreshold Numeric vector of threshold values for bounds. Default is
#'   \code{c(-3, -2, 2, 3)}. A threshold of 0 (for the mean line) is
#'   automatically included by \code{gsm.core::Analyze_NormalApprox_PredictBounds}.
#' @param nMinSiteFraction Minimum fraction of peak site count required to
#'   return non-\code{NA} bounds for a month. Months where the number of
#'   distinct \code{GroupID}s is below this fraction of the maximum are
#'   returned with \code{Metric = NA}. Default is \code{0.2} (20%).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{NMonth}: Time point (month number).
#'     \item \code{Threshold}: Threshold value (e.g., -3, -2, 0, 2, 3).
#'     \item \code{Denominator}: Sample size / exposure value.
#'     \item \code{LogDenominator}: Log of Denominator.
#'     \item \code{Numerator}: Predicted numerator at this threshold.
#'     \item \code{Metric}: Predicted Metric value at this threshold.
#'   }
#'   Multiple rows are returned per month per threshold, covering the range
#'   of Denominator values observed in the data.
#'
#' @seealso [Analyze_TimeZFunnel()] for calculating funnel scores,
#'   \code{gsm.core::Analyze_NormalApprox_PredictBounds} for the underlying
#'   bounds calculation.
#'
#' @export
PredictBounds_TimeZFunnel <- function(dfAnalyzed, vThreshold = c(-3, -2, 2, 3), nMinSiteFraction = 0.2) {
  # Validate input columns
  required_cols <- c("NMonth", "GroupID", "GroupLevel", "Numerator", "Denominator", "Metric")
  stopifnot(all(required_cols %in% names(dfAnalyzed)))

  # Identify sparse months (fewer than nMinSiteFraction of peak site count)
  sparse_months <- GetSparseMonths(dfAnalyzed, nMinSiteFraction)

  # Apply gsm.core::Analyze_NormalApprox_PredictBounds to each month's cross-section
  dfAnalyzed %>%
    dplyr::group_by(.data$NMonth) %>%
    dplyr::group_modify(~ suppressMessages(gsm.core::Analyze_NormalApprox_PredictBounds(
      .x,
      vThreshold = vThreshold,
      strType = "rate"
    ))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(sparse_months %>% dplyr::select("NMonth", "sparse"), by = "NMonth") %>%
    dplyr::mutate(Metric = dplyr::if_else(.data$sparse, NA_real_, .data$Metric)) %>%
    dplyr::select(-"sparse")
}
