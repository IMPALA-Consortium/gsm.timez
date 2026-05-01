#' Visualize Flagged Months by Site
#'
#' Creates a heat map of site flags over time.
#'
#' @param dfFlagged A data frame output from [Flag()] applied to
#'   [Analyze_TimeZFunnel()] output. Must contain columns:
#'   `GroupID`, `NMonth`, and `Flag`. Must also carry `vFlag` attribute
#'   (set by [Flag()]).
#' @param nSites Integer or `NULL`. Maximum number of sites to display. Sites
#'   are ranked by: (1) whether they were flagged in the most recent evaluated
#'   month, (2) total number of flagged months, (3) `GroupID` alphabetically.
#'   When `NULL` (the default) all sites are shown and no title is added to the
#'   plot.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @seealso [Analyze_TimeZFunnel()] for calculating funnel scores,
#'   [Flag()] for assigning flag categories.
#'
#' @export
Visualize_Heatmap <- function(dfFlagged, nSites = NULL) {
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

  nSitesTotal <- length(vGroups)
  if (!is.null(nSites)) {
    vGroups <- utils::head(vGroups, nSites)
  }
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

  if (!is.null(nSites)) {
    nSitesShown <- min(nSites, nSitesTotal)
    p <- p + ggplot2::labs(
      title = sprintf("Showing top %d of %d sites", nSitesShown, nSitesTotal)
    )
  }

  p
}
