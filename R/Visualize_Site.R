#' Visualize Single Site Trajectory with Flag Indicators
#'
#' Creates a line plot highlighting a single site's cumulative metric trajectory
#' against all other sites, with colored dots marking flagged months.
#'
#' @param dfFlagged A data frame output from [Flag()] containing columns:
#'   `GroupID`, `NMonth`, `Metric`, `Flag`, and (when `dfBounds` is provided)
#'   `Denominator`. Must carry `vFlag` and `vThreshold` attributes
#'   (set by `gsm.timez::Flag()`).
#' @param dfBounds Optional. A data frame with pre-calculated funnel bounds from
#'   [PredictBounds_TimeZFunnel()]. If provided, inner threshold
#'   lines are drawn for the selected site based on its denominator at each month.
#'   If `NULL` (default), no bounds are shown.
#' @param strSiteID Character string specifying the site ID to highlight.
#'   Must exist in `dfFlagged$GroupID`.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @seealso [Flag()] for assigning flag categories,
#'   [PredictBounds_TimeZFunnel()] for calculating bounds,
#'   [Visualize_Heatmap()] for the heat map visualization.
#'
#' @export
Visualize_Site <- function(dfFlagged, dfBounds = NULL, strSiteID) {
  # Read flag attributes
  vFlag <- sort(attr(dfFlagged, "vFlag"))
  if (is.null(vFlag)) {
    stop("dfFlagged must have 'vFlag' attribute (set by gsm.timez::Flag())")
  }
  vThreshold <- attr(dfFlagged, "vThreshold")

  # Validate strSiteID exists
  if (!strSiteID %in% dfFlagged$GroupID) {
    stop(paste0("Site '", strSiteID, "' not found in dfFlagged$GroupID."))
  }


  # Prepare data for plotting
  dfPlot <- dfFlagged %>%
    dplyr::select("GroupID", "NMonth", "Metric", "Flag") %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = vFlag))

  # Split data for layered plotting
  dfOtherSites <- dfPlot %>%
    dplyr::filter(.data$GroupID != strSiteID)

  dfSelectedSite <- dfPlot %>%
    dplyr::filter(.data$GroupID == strSiteID)

  dfFlaggedPoints <- dfSelectedSite %>%
    dplyr::filter(.data$Flag != 0)

  if (!is.null(dfBounds)) {
    dfSiteDenom <- dfFlagged %>%
      dplyr::filter(.data$GroupID == strSiteID) %>%
      dplyr::select("NMonth", "Denominator")

    dfBoundsPlot <- dfBounds %>%
      dplyr::left_join(
        dfSiteDenom %>% dplyr::rename(Denominator_site = "Denominator"),
        by = "NMonth"
      ) %>%
      dplyr::group_by(.data$NMonth, .data$Threshold) %>%
      dplyr::slice_min(abs(.data$Denominator - .data$Denominator_site), n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"Denominator_site")

    sorted_thresh <- sort(vThreshold)
    sorted_nonzero_flags <- sort(vFlag[vFlag != 0])
    threshold_flag_map <- stats::setNames(sorted_nonzero_flags, as.character(sorted_thresh))

    dfAllBoundLines <- dfBoundsPlot %>%
      dplyr::mutate(Flag = factor(
        dplyr::if_else(
          .data$Threshold == 0,
          0L,
          threshold_flag_map[as.character(.data$Threshold)]
        ),
        levels = vFlag
      )) %>%
      dplyr::arrange(match(.data$Flag, vFlag))
  }

  # Build plot
  p <- ggplot2::ggplot(dfPlot, ggplot2::aes(
    x = .data$NMonth,
    y = .data$Metric,
    group = .data$GroupID
  ))

  # Layer 1: Other sites as light gray lines (background context)
  if (nrow(dfOtherSites) > 0) {
    p <- p + ggplot2::geom_line(
      data = dfOtherSites,
      color = "gray80",
      linewidth = 0.3,
      alpha = 0.5
    )
  }

  # Layer 2: All bound lines (mean + thresholds) colored by flag, sorted for plotly legend order
  if (!is.null(dfBounds) && nrow(dfAllBoundLines) > 0) {
    p <- p + ggplot2::geom_line(
      data = dfAllBoundLines,
      ggplot2::aes(
        x = .data$NMonth, y = .data$Metric,
        group = .data$Threshold, color = .data$Flag
      ),
      linetype = "dashed",
      inherit.aes = FALSE
    )
  }

  # Layer 3: Selected site as dark gray line (on top of bounds)
  p <- p + ggplot2::geom_line(
    data = dfSelectedSite,
    color = "gray30",
    linewidth = 0.6,
    alpha = 0.8
  )

  # Layer 4: Flagged points only (colored dots)
  if (nrow(dfFlaggedPoints) > 0) {
    p <- p + ggplot2::geom_point(
      data = dfFlaggedPoints,
      ggplot2::aes(color = .data$Flag),
      size = 2,
      alpha = 0.8
    )
  }

  # Layer 5: Last point for selected site (current status when not flagged)
  dfLastPoint <- dfSelectedSite %>%
    dplyr::filter(.data$NMonth == max(.data$NMonth)) %>%
    dplyr::filter(Flag == 0)

  if (nrow(dfLastPoint) > 0) {
    p <- p + ggplot2::geom_point(
      data = dfLastPoint,
      ggplot2::aes(color = .data$Flag),
      size = 2,
      alpha = 0.8
    )
  }

  # Layer 6: Invisible dummy points for complete legend
  dfLegend <- data.frame(
    NMonth = min(dfPlot$NMonth),
    Metric = min(dfPlot$Metric),
    GroupID = NA_character_,
    Flag = factor(vFlag, levels = vFlag)
  )
  p <- p + ggplot2::geom_point(
    data = dfLegend,
    ggplot2::aes(color = .data$Flag),
    alpha = 0,
    show.legend = TRUE
  )

  # Color scale (funnel colors - blue-gray-red diverging palette)
  p <- p +
    ggplot2::scale_color_manual(
      breaks = as.character(vFlag),
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
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 3, alpha = 1))
    ) +
    ggplot2::labs(
      title = paste("Site:", strSiteID),
      x = "Month",
      y = "Metric (Numerator / Denominator)",
      color = "Flag"
    ) +
    ggplot2::theme_minimal()

  p
}
