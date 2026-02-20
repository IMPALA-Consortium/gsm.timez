#' Calculate Cumulative Z-Scores for Timeline Data
#'
#' This function takes the output from \code{\link{Timeline}} and calculates
#' cumulative z-scores for each row using an expanding window indexed by month.
#' The z-score compares each site's ratio against the cumulative study-wide
#' distribution of ratios up to that month.
#'
#' @param dfTimeline A data frame output from \code{\link{Timeline}}. Must
#'   contain columns: \code{GroupID}, \code{GroupLevel}, \code{Numerator},
#'   \code{Denominator}, and \code{NMonth}.
#'
#' @return The input data frame with two additional columns:
#'   \itemize{
#'     \item \code{Metric}: The ratio of Numerator to Denominator
#'       (Numerator / Denominator).
#'     \item \code{Score}: The z-score calculated using an expanding window.
#'       For month N, the z-score is calculated using all ratios from months
#'       1 through N across all groups. If fewer than 2 ratios exist in the
#'       cumulative window, Score is 0.
#'   }
#'
#' @details
#' The z-score is calculated as:
#' \deqn{z = \frac{Metric - mean(Metrics)}{sd(Metrics)}}
#'
#' Where \code{Metrics} includes all Metric values from all groups where
#' \code{NMonth <= current_NMonth}.
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
#' dfTimeline <- Timeline(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strGroupCol = "SiteID",
#'   strSubjectCol = "SubjectID",
#'   strNumeratorDateCol = "EventDate",
#'   strDenominatorDateCol = "VisitDate"
#' )
#'
#' TimeZScore(dfTimeline)
#'
#' @export
TimeZScore <- function(dfTimeline) {
  # Validate input columns
  required_cols <- c("GroupID", "GroupLevel", "Numerator", "Denominator", "NMonth")
  stopifnot(all(required_cols %in% names(dfTimeline)))

  # Calculate Metric (ratio)
  df <- dfTimeline %>%
    dplyr::mutate(Metric = .data$Numerator / .data$Denominator)

  # Compute cumulative mean and sd for each NMonth using expanding window
  # Get one row per NMonth with all Metrics accumulated up to that month
  cumulative_stats <- df %>%
    dplyr::arrange(.data$NMonth) %>%
    dplyr::group_by(.data$NMonth) %>%
    dplyr::summarise(metrics = list(.data$Metric), .groups = "drop") %>%
    dplyr::mutate(
      metrics = purrr::accumulate(.data$metrics, c),
      metric_mean = purrr::map_dbl(.data$metrics, mean),
      metric_sd = purrr::map_dbl(.data$metrics, ~ if (length(.x) < 2) 0 else sd(.x))
    ) %>%
    dplyr::select("NMonth", "metric_mean", "metric_sd")

  # Join stats back and calculate Score
  df %>%
    dplyr::left_join(cumulative_stats, by = "NMonth") %>%
    dplyr::mutate(
      Score = dplyr::if_else(.data$metric_sd == 0, 0, (.data$Metric - .data$metric_mean) / .data$metric_sd)
    ) %>%
    dplyr::select(-"metric_mean", -"metric_sd")
}
