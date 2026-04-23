#' Flag
#'
#' @description
#' Wrapper around `gsm.core::Flag()`. Adds a Flag column to analyzed data
#' identifying possible statistical outliers based on threshold comparisons.
#' Also stores `vThreshold` and `vFlag` as attributes for downstream use
#' by `Visualize()` (for bounds and legend).
#'
#' Months where the number of distinct sites falls below `nMinSiteFraction`
#' of the peak site count are considered statistically unreliable and have
#' their `Flag` set to `NA`, consistent with the `NA` bounds returned by
#' [TimeZScore_PredictBounds()] for the same months.
#'
#' @param dfAnalyzed `data.frame` where flags should be added. Must contain
#'   columns `NMonth` and `GroupID` for sparse-month detection.
#' @param vThreshold `numeric` Vector of threshold values in ascending order.
#'   Default: `c(-3, -2, 2, 3)`.
#' @param vFlag `numeric` Vector of flag values. Must have length equal to
#'   `length(vThreshold) + 1`. Default: `c(-2, -1, 0, 1, 2)`.
#' @param nMinSiteFraction Minimum fraction of peak site count required to
#'   return a non-`NA` flag for a month. Months where the number of distinct
#'   `GroupID`s is below this fraction of the maximum are returned with
#'   `Flag = NA`. Default is `0.2` (20\%), matching [TimeZScore_PredictBounds()].
#'   Set to `0` to disable sparse-month masking.
#' @param ... Additional arguments passed to `gsm.core::Flag()`.
#'
#' @return `data.frame` with an additional `Flag` column and attributes
#'   `vThreshold` and `vFlag` for downstream use.
#'
#' @seealso [gsm.core::Flag()], [TimeZScore_PredictBounds()]
#'
#' @export
Flag <- function(dfAnalyzed,
                 vThreshold = c(-3, -2, 2, 3),
                 vFlag = c(-2, -1, 0, 1, 2),
                 nMinSiteFraction = 0.2,
                 ...) {
  result <- gsm.core::Flag(
    dfAnalyzed = dfAnalyzed,
    vThreshold = vThreshold,
    vFlag = vFlag,
    ...
  )

  if (nMinSiteFraction > 0) {
    sparse_months <- GetSparseMonths(result, nMinSiteFraction)
    result <- result %>%
      dplyr::left_join(
        sparse_months %>% dplyr::select("NMonth", "sparse"),
        by = "NMonth"
      ) %>%
      dplyr::mutate(Flag = dplyr::if_else(.data$sparse, NA_integer_, .data$Flag)) %>%
      dplyr::select(-"sparse")
  }

  attr(result, "vThreshold") <- vThreshold
  attr(result, "vFlag") <- vFlag

  result
}
