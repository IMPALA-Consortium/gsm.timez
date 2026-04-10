#' Visualize Site Cumulative Events Over Time with Flag Indicators
#'
#' Creates a line plot showing the cumulative Numerator (events) for each group
#' over sequential months, with dots colored by Flag value to highlight outliers.
#'
#' @param dfFlagged A data frame output from Flag() containing columns:
#'   GroupID, NMonth, Metric, Flag. Must have `vThreshold` and `vFlag`
#'   attributes (set by `gsm.timez::Flag()`).
#' @param dfBounds Optional. A data frame with pre-calculated bounds from
#'   \code{\link{TimeZScore_PredictBounds}}. If provided, bounds (mean line
#'   and upper/lower threshold lines) are drawn on the plot. If NULL (default),
#'   no bounds are shown.
#'
#' @return A ggplot2 object showing Metric vs NMonth with gray lines per site,
#'   dots colored by Flag value (diverging scale: red=outlier, gray=normal).
#'   If dfBounds is provided, also shows a solid line for mean and dashed lines
#'   for upper/lower bounds based on the inner thresholds from vThreshold.
#'
#' @seealso \code{\link{TimeZScore_PredictBounds}} for calculating bounds separately.
#'
#' @examples
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
#'   TimeZScore() %>%
#'   Flag()
#'
#' Visualize(dfFlagged)
#'
#' @export
Visualize <- function(dfFlagged, dfBounds = NULL) {

  # Read threshold and flag attributes
  vThreshold <- attr(dfFlagged, "vThreshold")
  vFlag <- attr(dfFlagged, "vFlag")

  # Prepare reference lines data frame if dfBounds is provided
  dfRefLines <- NULL
  if (!is.null(dfBounds)) {
    # Compute inner bounds (middle two thresholds)
    n <- length(vThreshold)
    if (n %% 2 != 0) {
      stop("vThreshold must have an even number of elements to determine bounds")
    }
    lower_threshold <- vThreshold[n / 2]
    upper_threshold <- vThreshold[n / 2 + 1]

    dfRefLines <- dfBounds %>%
      dplyr::mutate(
        line_type = dplyr::case_when(
          .data$Threshold == upper_threshold ~ "upper_bound",
          .data$Threshold == lower_threshold ~ "lower_bound",
          .data$Threshold == 0 ~ "mean",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(.data$line_type)) %>%
      dplyr::select("NMonth", "line_type", value = "Metric")
  }

  # Select only columns needed for plotting (improves plotly performance)
  dfPlot <- dfFlagged %>%
    dplyr::select("GroupID", "NMonth", "Metric", "Flag") %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = vFlag))

  # Identify sites that ever had a flag
  flagged_site_ids <- dfPlot %>%
    dplyr::filter(.data$Flag != 0) %>%
    dplyr::pull(.data$GroupID) %>%
    unique()

  # Split data for layered plotting
  dfNonFlaggedSites <- dfPlot %>%
    dplyr::filter(!.data$GroupID %in% flagged_site_ids)
  dfFlaggedSites <- dfPlot %>%
    dplyr::filter(.data$GroupID %in% flagged_site_ids)
  dfFlaggedPoints <- dfFlaggedSites %>%
    dplyr::filter(.data$Flag != 0)

  # Build plot incrementally to handle empty data frames (plotly compatibility)
  p <- ggplot2::ggplot(dfPlot, ggplot2::aes(
    x = .data$NMonth,
    y = .data$Metric,
    group = .data$GroupID
  ))


  # Layer 1: Non-flagged sites as thin light gray lines (background context)
  if (nrow(dfNonFlaggedSites) > 0) {
    p <- p + ggplot2::geom_line(
      data = dfNonFlaggedSites,
      color = "gray80",
      linewidth = 0.3,
      alpha = 0.5
    )
  }

  # Layer 2: Flagged sites as thin dark gray lines (full trajectory)
  if (nrow(dfFlaggedSites) > 0) {
    p <- p + ggplot2::geom_line(
      data = dfFlaggedSites,
      color = "gray30",
      linewidth = 0.3,
      alpha = 0.7
    )
  }

  # Layer 3: Reference lines (mean and bounds) - only when dfBounds provided
  if (!is.null(dfRefLines)) {
    p <- p + ggplot2::geom_line(
      data = dfRefLines,
      ggplot2::aes(x = .data$NMonth, y = .data$value,
                   group = .data$line_type,
                   linetype = .data$line_type,
                   linewidth = .data$line_type),
      color = "black",
      inherit.aes = FALSE
    ) +
      ggplot2::scale_linetype_manual(
        values = c("upper_bound" = "dashed",
                   "lower_bound" = "dashed",
                   "mean"        = "solid"),
        guide = "none"
      ) +
      ggplot2::scale_linewidth_manual(
        values = c("upper_bound" = 0.5,
                   "lower_bound" = 0.5,
                   "mean"        = 2),
        guide = "none"
      )
  }

  # Layer 4: Only flagged points (where flag != 0)
  if (nrow(dfFlaggedPoints) > 0) {
    p <- p + ggplot2::geom_point(
      data = dfFlaggedPoints,
      ggplot2::aes(color = .data$Flag),
      size = 2,
      alpha = 0.6
    )
  }

  # Layer 5: Invisible dummy points for all flag levels (forces complete legend)
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

  # Configure color scale and legend
  p <- p +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        grDevices::colorRampPalette(c(
          "#D73027",
          "#FC8D59",
          "#FEE08B",
          "#999999",
          "#FEE08B",
          "#FC8D59",
          "#D73027"
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
      x = "Month",
      y = "Metric (Numerator / Denominator)",
      color = "Flag"
    ) +
    ggplot2::theme_minimal()

  p
}
