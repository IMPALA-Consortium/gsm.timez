#' Get Sparse Months
#'
#' @param df A data frame containing at minimum columns `NMonth` (month index)
#'   and `GroupID` (site identifier).
#' @param nMinSiteFraction Minimum fraction of peak site count; months below
#'   this fraction are marked sparse.
#' @return A data frame with columns `NMonth` and `sparse` (logical).
GetSparseMonths <- function(df, nMinSiteFraction) {
  df %>%
    dplyr::group_by(.data$NMonth) %>%
    dplyr::summarise(n_sites = dplyr::n_distinct(.data$GroupID), .groups = "drop") %>%
    dplyr::mutate(sparse = .data$n_sites / max(.data$n_sites) < nMinSiteFraction)
}
