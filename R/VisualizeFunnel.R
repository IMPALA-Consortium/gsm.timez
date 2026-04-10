#' Visualize Funnel Plot Results as Heat Map
#'
#' Creates a heat map showing Flag categories for each site over time,
#' matching the visualization style from Zink et al. (Figures 7 & 8).
#'
#' @param dfFlagged A data frame output from Flag() applied to
#'   \code{\link{TimeZScoreFunnel}} output. Must contain columns:
#'   GroupID, NMonth, and Flag. Must also have `vFlag` attribute
#'   (set by \code{gsm.timez::Flag()}).
#'
#' @return A ggplot2 heat map object with:
#'   \itemize{
#'     \item X-axis: Time (NMonth)
#'     \item Y-axis: Sites (GroupID), sorted numerically if IDs are numeric,
#'       otherwise alphabetically
#'     \item Fill color: Flag category (Much Lower, Lower, Within Limits,
#'       Higher, Much Higher)
#'   }
#'
#' @details
#' This visualization displays all sites and time periods in a single view,
#' with each cell colored by its Flag value. This makes it easy to identify:
#' \itemize{
#'   \item Sites with persistent outlier status across time
#'   \item Time periods when many sites became outliers
#'   \item The overall pattern of outliers in the study
#' }
#'
#' The color scheme uses a diverging blue-gray-red palette: blue shades for
#' under-reporting (Flag = -2, -1), gray for values within limits (Flag = 0),
#' and red shades for over-reporting (Flag = 1, 2).
#'
#' @seealso \code{\link{TimeZScoreFunnel}} for calculating funnel scores,
#'   \code{\link{Flag}} for assigning flag categories,
#'   \code{\link{Visualize}} for the empirical z-score visualization.
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
#' dfFlagged <- Timeline(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strGroupCol = "SiteID",
#'   strSubjectCol = "SubjectID",
#'   strNumeratorDateCol = "EventDate",
#'   strDenominatorDateCol = "VisitDate"
#' ) %>%
#'   TimeZScoreFunnel() %>%
#'   Flag()
#'
#' VisualizeFunnel(dfFlagged)
#' }
#'
#' @export
VisualizeFunnel <- function(dfFlagged) {

  # Read flag attribute

  vFlag <- attr(dfFlagged, "vFlag")


  # ============================================================================
  # TEMPORARY: Limit to 15 sites for faster plotly generation during testing.
  # TODO: REMOVE THIS BLOCK BEFORE PRODUCTION
  # ============================================================================
  vSiteIDs <- unique(dfFlagged$GroupID)
  vSiteIDs <- head(vSiteIDs, 15)
  dfFlagged <- dfFlagged %>%
    dplyr::filter(.data$GroupID %in% vSiteIDs)
  # ============================================================================

  # Prepare data for plotting
  dfPlot <- dfFlagged %>%
    dplyr::select("GroupID", "NMonth", "Flag") %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = vFlag))

  # Sort groups numerically if possible, otherwise alphabetically
  vGroups <- unique(dfPlot$GroupID)
  vGroupsNumeric <- suppressWarnings(as.numeric(vGroups))
  if (all(!is.na(vGroupsNumeric))) {
    vGroups <- vGroups[order(vGroupsNumeric)]
  } else {
    vGroups <- sort(vGroups)
  }

  # Build heat map
  p <- ggplot2::ggplot(dfPlot, ggplot2::aes(
    x = .data$NMonth,
    y = .data$GroupID,
    fill = .data$Flag
  )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_y_discrete(limits = rev(vGroups)) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(
        grDevices::colorRampPalette(c(
          "#2166AC",
          "#67A9CF",
          "#999999",
          "#EF8A62",
          "#B2182B"
        ))(length(vFlag)),
        as.character(vFlag)
      ),
      labels = stats::setNames(
        as.character(vFlag),
        as.character(vFlag)
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Month",
      y = "Site",
      fill = "Category"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )

  p
}
