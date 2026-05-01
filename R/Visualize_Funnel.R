#' Visualize Funnel Plot for a Single Month
#'
#' Creates a classic funnel plot showing Metric vs Denominator with curved
#' confidence bounds for a single month. Sites are plotted as points colored
#' by their Flag value, with flagged sites labeled.
#'
#' @param dfFlagged A data frame output from \code{\link{Flag}} applied to
#'   \code{\link{Analyze_TimeZFunnel}} output. Must contain columns:
#'   \code{GroupID}, \code{NMonth}, \code{Denominator}, \code{Metric}, \code{Flag}.
#'   Must have \code{vFlag} attribute (set by \code{gsm.timez::Flag()}).
#' @param dfBounds A data frame output from \code{\link{PredictBounds_TimeZFunnel}}.
#'   Must contain columns: \code{NMonth}, \code{Threshold}, \code{Denominator},
#'   \code{Metric}.
#' @param NMonth Optional integer specifying which month to plot. If \code{NULL}
#'   (default), uses the largest month where at least 75\% of sites are active,
#'   ensuring good site coverage in the plot.
#'
#' @return A ggplot2 object showing:
#'   \itemize{
#'     \item Funnel bound curves (dashed lines for thresholds, solid for mean)
#'     \item Site points colored by Flag value
#'     \item Labels for flagged sites (Flag != 0)
#'   }
#'
#' @details
#' This visualization shows a single month's cross-section as a traditional
#' funnel plot. The x-axis shows Denominator (log scale), and the y-axis shows
#' Metric. Funnel curves narrow as Denominator increases, reflecting the
#' expectation that larger sites have less variability.
#'
#' @seealso \code{\link{Analyze_TimeZFunnel}} for calculating funnel scores,
#'   \code{\link{PredictBounds_TimeZFunnel}} for calculating bounds,
#'   \code{\link{Visualize_Heatmap}} for the heat map visualization across all months.
#' 
#' @export
Visualize_Funnel <- function(dfFlagged, dfBounds, NMonth = NULL) {
  # Read flag attribute

  vFlag <- attr(dfFlagged, "vFlag")
  if (is.null(vFlag)) {
    stop("dfFlagged must have 'vFlag' attribute (set by gsm.timez::Flag())")
  }

  # Default to largest month with >= 75% site coverage
  if (is.null(NMonth)) {
    NMonth <- find_default_month(dfFlagged)
  }

  # Filter to selected month
  dfPlotData <- dfFlagged %>%
    dplyr::filter(.data$NMonth == !!NMonth) %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = vFlag))

  dfPlotBounds <- dfBounds %>%
    dplyr::filter(.data$NMonth == !!NMonth)

  # Separate bounds by line type
  dfMeanLine <- dfPlotBounds %>%
    dplyr::filter(.data$Threshold == 0)

  dfBoundLines <- dfPlotBounds %>%
    dplyr::filter(.data$Threshold != 0) %>%
    dplyr::mutate(Threshold = factor(.data$Threshold))

  # Flagged points for labeling
  dfLabels <- dfPlotData %>%
    dplyr::filter(.data$Flag != 0)

  # Build plot
  p <- ggplot2::ggplot() +
    # Layer 1: Bound curves (dashed)
    ggplot2::geom_line(
      data = dfBoundLines,
      ggplot2::aes(
        x = .data$Denominator,
        y = .data$Metric,
        group = .data$Threshold
      ),
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Layer 2: Mean line (solid)
    ggplot2::geom_line(
      data = dfMeanLine,
      ggplot2::aes(
        x = .data$Denominator,
        y = .data$Metric
      ),
      linetype = "solid",
      color = "black",
      linewidth = 1
    ) +
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
      title = paste0("Funnel Plot \u2014 Month: ", NMonth),
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

# Internal helper: find largest month with >= 75% site coverage
find_default_month <- function(df, threshold = 0.75) {
  total_sites <- dplyr::n_distinct(df$GroupID)

  df %>%
    dplyr::group_by(.data$NMonth) %>%
    dplyr::summarise(n_sites = dplyr::n_distinct(.data$GroupID), .groups = "drop") %>%
    dplyr::mutate(pct_sites = .data$n_sites / total_sites) %>%
    dplyr::filter(.data$pct_sites >= threshold) %>%
    dplyr::pull(.data$NMonth) %>%
    max()
}
