#' Visualize Funnel Plot Results as Heat Map
#'
#' Creates a heat map showing Flag categories for each site over time,
#' matching the visualization style from Zink et al. (Figures 7 & 8).
#'
#' @param dfFlagged A data frame output from Flag() applied to
#'   \code{\link{Analyze_TimeZFunnel}} output. Must contain columns:
#'   GroupID, NMonth, and Flag. Must also have `vFlag` attribute
#'   (set by \code{gsm.timez::Flag()}).
#'
#' @return A ggplot2 heat map object with:
#'   \itemize{
#'     \item X-axis: Time (NMonth)
#'     \item Y-axis: Sites (GroupID), sorted so sites flagged in the most recent
#'       month appear at the top, with ties broken by total number of flagged
#'       months (descending), then by GroupID
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
#' @seealso \code{\link{Analyze_TimeZFunnel}} for calculating funnel scores,
#'   \code{\link{Flag}} for assigning flag categories.
#' 
#' @export
Visualize_Heatmap <- function(dfFlagged) {
  # Read flag attribute

  vFlag <- attr(dfFlagged, "vFlag")


  # Prepare data for plotting
  dfPlot <- dfFlagged %>%
    dplyr::select("GroupID", "NMonth", "Flag") %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = vFlag))

  # Sort sites: flagged in last evaluated month first, then by total flagged months.
  # Use nLastValidMonth (last month where ≥20% threshold was met) as the reference,
  # since months after that threshold have NA flags.
  flag_int_all <- as.integer(as.character(dfPlot$Flag))
  nLastValidMonth <- max(dfPlot$NMonth[!is.na(flag_int_all)])

  dfSortKeys <- dfPlot %>%
    dplyr::mutate(flag_int = as.integer(as.character(.data$Flag))) %>%
    dplyr::group_by(.data$GroupID) %>%
    dplyr::summarise(
      last_flagged = as.integer(any(
        .data$NMonth == nLastValidMonth & !is.na(.data$flag_int) & .data$flag_int != 0
      )),
      n_flagged = sum(!is.na(.data$flag_int) & .data$flag_int != 0),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$last_flagged), dplyr::desc(.data$n_flagged), .data$GroupID)
  vGroups <- dfSortKeys$GroupID

  # ============================================================================
  # TEMPORARY: Limit to 30 sites for faster plotly generation during testing.
  # ============================================================================
  vGroups <- utils::head(vGroups, 30)
  dfPlot <- dfPlot %>% dplyr::filter(.data$GroupID %in% vGroups)

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
      na.value = "#E8E8E8",
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
