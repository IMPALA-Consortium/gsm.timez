#' Calculate Funnel Plot Scores for Timeline Data
#'
#' Applies \code{gsm.core::Analyze_NormalApprox()} to each month of
#' \code{dfTimeline}, producing a funnel plot score for each site-month
#' combination.
#'
#' @param dfTimeline A data frame output from [Timeline()]. Must
#'   contain columns: \code{GroupID}, \code{GroupLevel}, \code{Numerator},
#'   \code{Denominator}, and \code{NMonth}.
#'
#' @return All columns from [Timeline()], plus the following
#'   additional columns:
#'   \itemize{
#'     \item \code{Metric}: The ratio of Numerator to Denominator
#'       (Numerator / Denominator). Computed before calling
#'       \code{gsm.core::Analyze_NormalApprox}.
#'     \item \code{OverallMetric}, \code{Factor}, \code{Score}: See
#'       [gsm.core::Analyze_NormalApprox()] for definitions.
#'   }
#'
#' @seealso [gsm.core::Analyze_NormalApprox()], [Timeline()]
#'
#' @export
Analyze_TimeZFunnel <- function(dfTimeline) {
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
