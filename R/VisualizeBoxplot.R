#' Visualize Metric Distribution by Month with Boxplots
#'
#' Creates a boxplot showing the distribution of Metric values for each
#' sequential month, with individual points colored by Flag value to highlight
#' outliers. Points are rendered behind the boxplots with reduced opacity.
#'
#' @param dfFlagged A data frame output from Flag() containing columns:
#'   GroupID, NMonth, Metric, and Flag.
#' @param nMonths Optional numeric vector of NMonth values to include in the
#'   plot. If NULL (default), all months are shown. Use this to filter to
#'   specific months, e.g., `nMonths = 1:10` for the first 10 months.
#'
#' @return A ggplot2 object showing boxplots of Metric by NMonth with
#'   individual points colored by Flag value (diverging scale: red=low outlier,
#'   gray=normal, blue=high outlier).
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
#' # Show all months
#' VisualizeBoxplot(dfFlagged)
#'
#' # Show only first 10 months
#' VisualizeBoxplot(dfFlagged, nMonths = 1:10)
#'
#' @export
VisualizeBoxplot <- function(dfFlagged, nMonths = NULL) {
  # Filter to specified months if provided
  if (!is.null(nMonths)) {
    dfFlagged <- dfFlagged %>%
      dplyr::filter(.data$NMonth %in% nMonths)
  }

  # Convert Flag to factor for discrete color scale
  dfFlagged <- dfFlagged %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = c(-2, -1, 0, 1, 2)))

  # Convert NMonth to factor for discrete x-axis
  dfFlagged <- dfFlagged %>%
    dplyr::mutate(NMonth = factor(.data$NMonth))

  ggplot2::ggplot(dfFlagged, ggplot2::aes(x = .data$NMonth, y = .data$Metric)) +
    # Points first (behind boxplots)
    ggplot2::geom_point(
      ggplot2::aes(color = .data$Flag),
      position = ggplot2::position_jitter(width = 0.2, seed = 42),
      size = 2,
      alpha = 0.4
    ) +
    # Boxplots on top (no fill, suppress outlier points to avoid duplicates)
    ggplot2::geom_boxplot(
      fill = NA,
      outlier.shape = NA,
      alpha = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "-2" = "#D73027", "-1" = "#FC8D59",
        "0" = "#999999",
        "1" = "#91BFDB", "2" = "#4575B4"
      ),
      labels = c(
        "-2" = "Red Low", "-1" = "Amber Low",
        "0" = "Normal",
        "1" = "Amber High", "2" = "Red High"
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Month",
      y = "Metric (Numerator / Denominator)",
      color = "Flag"
    ) +
    ggplot2::theme_minimal()
}
