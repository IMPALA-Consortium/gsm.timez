#' Calculate Funnel Plot Scores for Timeline Data
#'
#' This function takes the output from \code{\link{Timeline}} and calculates
#' scores using the funnel plot methodology (Poisson-based normal approximation)
#' as described in the Zink et al. paper. The score adjusts for sample size,
#' so larger sites are held to tighter standards.
#'
#' @param dfTimeline A data frame output from \code{\link{Timeline}}. Must
#'   contain columns: \code{GroupID}, \code{GroupLevel}, \code{Numerator},
#'   \code{Denominator}, and \code{NMonth}.
#'
#' @return The input data frame with all original columns preserved, plus
#'   additional columns from \code{gsm.core::Analyze_NormalApprox}:
#'   \itemize{
#'     \item \code{Metric}: The ratio of Numerator to Denominator
#'       (Numerator / Denominator).
#'     \item \code{OverallMetric}: The study-wide pooled rate at each month
#'       (sum of Numerators / sum of Denominators).
#'     \item \code{Factor}: The factor used in the normal approximation for
#'       calculating bounds.
#'     \item \code{Score}: The adjusted z-score that accounts for sample size.
#'       Calculated as (Metric - OverallMetric) / SE, where SE is based on
#'       Poisson variance assumptions.
#'   }
#'
#' @details
#' This function applies \code{gsm.core::Analyze_NormalApprox()} to each
#' month's cross-section of sites. The score is calculated using the funnel
#' plot methodology:
#'
#' \deqn{Score = \frac{Metric - OverallMetric}{\sqrt{OverallMetric \times Factor / Denominator}}}
#'
#' This approach assumes count data follows a Poisson distribution, where
#' variance equals the mean. The key advantage over simple z-scores is that
#' larger sites (higher Denominator) are expected to have less variability,
#' so they receive narrower confidence bounds.
#'
#' @seealso \code{\link{TimeZScore}} for the empirical z-score approach,
#'   \code{\link{VisualizeFunnel}} for plotting funnel scores.
#'
#' @examples
#' \dontrun{
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
#' TimeZScoreFunnel(dfTimeline)
#' }
#'
#' @export
TimeZScoreFunnel <- function(dfTimeline) {
  # Validate input columns
  required_cols <- c("GroupID", "GroupLevel", "Numerator", "Denominator", "NMonth")
  stopifnot(all(required_cols %in% names(dfTimeline)))


  # Calculate Metric (ratio)
  df <- dfTimeline %>%
    dplyr::mutate(Metric = .data$Numerator / .data$Denominator)

  # Apply gsm.core::Analyze_NormalApprox to each month's cross-section
  df_analyzed <- df %>%
    dplyr::group_by(.data$NMonth) %>%
    dplyr::group_modify(~ suppressMessages(gsm.core::Analyze_NormalApprox(.x, strType = "rate"))) %>%
    dplyr::ungroup()

  # Analyze_NormalApprox drops extra columns, here we're joining them back dynamically
  dropped <- setdiff(names(df), names(df_analyzed))
  if (length(dropped) > 0) {
    df_analyzed <- df_analyzed %>%
      dplyr::left_join(
        df %>% dplyr::select("NMonth", "GroupID", dplyr::all_of(dropped)) %>% dplyr::distinct(),
        by = c("NMonth", "GroupID")
      )
  }

  df_analyzed
}
