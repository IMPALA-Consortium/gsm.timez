#' Flag
#'
#' @description
#' Wrapper around `gsm.core::Flag()`. Adds a Flag column to analyzed data
#' identifying possible statistical outliers based on threshold comparisons.
#' Also stores `vThreshold` and `vFlag` as attributes for downstream use
#' by `Visualize()` (for bounds and legend).
#'
#' @param dfAnalyzed `data.frame` where flags should be added.
#' @param vThreshold `numeric` Vector of threshold values in ascending order.
#'   Default: `c(-3, -2, 2, 3)`.
#' @param vFlag `numeric` Vector of flag values. Must have length equal to
#'   `length(vThreshold) + 1`. Default: `c(-2, -1, 0, 1, 2)`.
#' @param ... Additional arguments passed to `gsm.core::Flag()`.
#'
#' @return `data.frame` with an additional `Flag` column and attributes
#'   `vThreshold` and `vFlag` for downstream use.
#'
#' @seealso [gsm.core::Flag()]
#'
#' @export
Flag <- function(dfAnalyzed,
                 vThreshold = c(-3, -2, 2, 3),
                 vFlag = c(-2, -1, 0, 1, 2),
                 ...) {
  result <- gsm.core::Flag(
    dfAnalyzed = dfAnalyzed,
    vThreshold = vThreshold,
    vFlag = vFlag,
    ...
  )

  attr(result, "vThreshold") <- vThreshold
  attr(result, "vFlag") <- vFlag

  result
}
