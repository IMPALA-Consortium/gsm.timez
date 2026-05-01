#' Visualize Funnel Plot for a Single Month
#'
#' Creates a classic funnel plot showing Metric vs Denominator with curved
#' confidence bounds for a single month. Sites are plotted as points colored
#' by their Flag value, with flagged sites labeled.
#'
#' @param dfFlagged A data frame output from [Flag()] applied to
#'   [Analyze_TimeZFunnel()] output. Must contain columns:
#'   \code{GroupID}, \code{NMonth}, \code{Denominator}, \code{Metric}, \code{Flag}.
#'   Must have \code{vFlag} attribute (set by \code{gsm.timez::Flag()}).
#' @param dfBounds A data frame output from [PredictBounds_TimeZFunnel()].
#'   Must contain columns: \code{NMonth}, \code{Threshold}, \code{Denominator},
#'   \code{Metric}.
#' @param NMonth Optional integer specifying which month to plot. If \code{NULL}
#'   (default), uses the largest month with at least the minimum fraction of
#'   sites active (as specified in [Flag()] and
#'   [PredictBounds_TimeZFunnel()] via \code{nMinSiteFraction}). If
#'   provided, must meet the same criterion; an error is raised otherwise.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @seealso [Analyze_TimeZFunnel()] for calculating funnel scores,
#'   [PredictBounds_TimeZFunnel()] for calculating bounds,
#'   [Visualize_Heatmap()] for the heat map visualization across all months.
#'
#' @export
Visualize_Funnel <- function(dfFlagged, dfBounds, NMonth = NULL) {
  # Read flag attribute

  vFlag <- sort(attr(dfFlagged, "vFlag"))
  if (is.null(vFlag)) {
    stop("dfFlagged must have 'vFlag' attribute (set by gsm.timez::Flag())")
  }
  vThreshold <- attr(dfFlagged, "vThreshold")

  valid_months <- dfFlagged %>%
    dplyr::filter(!is.na(.data$Flag)) %>%
    dplyr::pull(.data$NMonth) %>%
    unique()

  if (is.null(NMonth)) {
    NMonth <- max(valid_months)
  } else if (!NMonth %in% valid_months) {
    stop(paste0("NMonth ", NMonth, " is sparse or not present in the data."))
  }

  # Filter to selected month
  dfPlotData <- dfFlagged %>%
    dplyr::filter(.data$NMonth == !!NMonth) %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = vFlag))

  sorted_thresh <- sort(vThreshold)
  sorted_nonzero_flags <- sort(vFlag[vFlag != 0])
  threshold_flag_map <- stats::setNames(sorted_nonzero_flags, as.character(sorted_thresh))

  dfPlotBounds <- dfBounds %>%
    dplyr::filter(.data$NMonth == !!NMonth) %>%
    dplyr::mutate(Flag = factor(
      dplyr::if_else(
        .data$Threshold == 0,
        0L,
        threshold_flag_map[as.character(.data$Threshold)]
      ),
      levels = vFlag
    ))

  dfAllBoundLines <- dfPlotBounds %>%
    dplyr::mutate(
      linetype_val = dplyr::if_else(.data$Threshold == 0, "solid", "dashed"),
      Threshold = factor(.data$Threshold)
    ) %>%
    dplyr::arrange(match(.data$Flag, vFlag))

  # Flagged points for labeling
  dfLabels <- dfPlotData %>%
    dplyr::filter(.data$Flag != 0)

  # Build plot
  p <- ggplot2::ggplot() +
    # Layer 1: All bound curves (mean solid, thresholds dashed), ordered by Flag for plotly legend
    ggplot2::geom_line(
      data = dfAllBoundLines,
      ggplot2::aes(
        x = .data$Denominator,
        y = .data$Metric,
        group = .data$Threshold,
        color = .data$Flag,
        linetype = .data$linetype_val
      ),
      linewidth = 0.7
    ) +
    ggplot2::scale_linetype_identity() +
    # Layer 3: Site points
    ggplot2::geom_point(
      data = dfPlotData,
      ggplot2::aes(
        x = .data$Denominator,
        y = .data$Metric,
        color = .data$Flag
      ),
      size = 2.5,
      alpha = 0.8
    ) +
    # Layer 4: Labels for flagged sites
    ggplot2::geom_text(
      data = dfLabels,
      ggplot2::aes(
        x = .data$Denominator,
        y = .data$Metric,
        label = .data$GroupID
      ),
      size = 2.5,
      vjust = -1,
      check_overlap = TRUE
    ) +
    # Color scale (blue-gray-red diverging palette)
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
    # Log scale for x-axis
    ggplot2::scale_x_log10() +
    # Labels
    ggplot2::labs(
      title = paste0("Funnel Plot - Month: ", NMonth),
      x = "Denominator (log scale)",
      y = "Metric",
      color = "Flag"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right"
    )

  p
}
