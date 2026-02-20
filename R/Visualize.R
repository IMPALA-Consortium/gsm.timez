#' Visualize Site Cumulative Events Over Time with Flag Indicators
#'
#' Creates a line plot showing the cumulative Numerator (events) for each group
#' over sequential months, with dots colored by Flag value to highlight outliers.
#'
#' @param dfFlagged A data frame output from Flag() containing columns:
#'   GroupID, NMonth, Numerator, and Flag.
#'
#' @return A ggplot2 object showing Numerator vs NMonth with gray lines per site
#'   and dots colored by Flag value (diverging scale: red=low outlier,
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
#' Visualize(dfFlagged)
#'
#' @export
Visualize <- function(dfFlagged) {
  # Convert Flag to factor for discrete color scale
  dfFlagged <- dfFlagged %>%
    dplyr::mutate(Flag = factor(.data$Flag, levels = c(-2, -1, 0, 1, 2)))

  ggplot2::ggplot(dfFlagged, ggplot2::aes(
    x = .data$NMonth,
    y = .data$Metric,
    group = .data$GroupID
  )) +
    ggplot2::geom_line(color = "gray70", alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(color = .data$Flag), size = 2.5, alpha = 0.4) +
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
