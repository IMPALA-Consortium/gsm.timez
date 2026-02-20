#' Generate Timeline HTML Report
#'
#' Generates an HTML report containing interactive visualizations and summary
#' statistics for timeline analysis results. The report includes a line chart
#' showing metrics over time, a boxplot of metric distributions, and a table
#' of flagged groups.
#'
#' @param dfFlagged A data frame output from Flag() containing columns:
#'   GroupID, GroupLevel, NMonth, DenominatorMonth, Numerator, Denominator,
#'   Metric, Score, and Flag.
#' @param lCharts Optional list of charts from MakeCharts_Timeline(). If NULL,
#'   charts will be generated automatically from dfFlagged.
#' @param strStudyID Optional study identifier for the report title.
#' @param strGroupLevel Group level label (e.g., "Site", "Country").
#'   Default is extracted from dfFlagged$GroupLevel or "Site".
#' @param strOutputDir Output directory for the generated report. Default is
#'   current working directory.
#' @param strOutputFile Output filename. If NULL, generates a timestamped
#'   filename like "Timeline_Report_2024-01-15.html".
#' @param strInputPath Path to the R Markdown template. Default uses the
#'   package's built-in template.
#'
#' @return File path of the saved report (returned invisibly).
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
#'   TimeZScore() %>%
#'   Flag()
#'
#' Report_Timeline(dfFlagged, strOutputFile = "my_report.html")
#' }
#'
#' @export
Report_Timeline <- function(
    dfFlagged,
    lCharts = NULL,
    strStudyID = NULL,
    strGroupLevel = NULL,
    strOutputDir = getwd(),
    strOutputFile = NULL,
    strInputPath = system.file("report", "Report_Timeline.Rmd", package = "gsm.timez")) {
  if (is.null(lCharts)) {
    lCharts <- MakeCharts_Timeline(dfFlagged)
  }

  if (is.null(strGroupLevel)) {
    if ("GroupLevel" %in% names(dfFlagged) && length(unique(dfFlagged$GroupLevel)) == 1) {
      strGroupLevel <- unique(dfFlagged$GroupLevel)
    } else {
      strGroupLevel <- "Site"
    }
  }

  if (is.null(strOutputFile)) {
    strOutputFile <- paste0("Timeline_Report_", Sys.Date(), ".html")
  }


  strOutputPath <- file.path(strOutputDir, strOutputFile)

  dfSummary <- .MakeSummaryStats(dfFlagged, strGroupLevel)
  dfFlaggedGroups <- .MakeFlaggedGroupsTable(dfFlagged)

  params <- list(
    dfFlagged = dfFlagged,
    lCharts = lCharts,
    dfSummary = dfSummary,
    dfFlaggedGroups = dfFlaggedGroups,
    strStudyID = strStudyID,
    strGroupLevel = strGroupLevel,
    strReportDate = as.character(Sys.Date())
  )

  rmarkdown::render(
    input = strInputPath,
    output_file = strOutputFile,
    output_dir = strOutputDir,
    params = params,
    envir = new.env(),
    quiet = TRUE
  )

  message("Report saved to: ", strOutputPath)
  invisible(strOutputPath)
}

#' Generate Summary Statistics for Timeline Report
#'
#' Internal function to create summary statistics for the report.
#'
#' @param dfFlagged Flagged timeline data frame.
#' @param strGroupLevel Group level label.
#'
#' @return A data frame with summary statistics.
#'
#' @keywords internal
.MakeSummaryStats <- function(dfFlagged, strGroupLevel) {
  n_groups <- length(unique(dfFlagged$GroupID))
  n_months <- length(unique(dfFlagged$NMonth))

  latest_month <- dfFlagged %>%
    dplyr::filter(.data$NMonth == max(.data$NMonth))

  flag_summary <- dfFlagged %>%
    dplyr::count(.data$Flag) %>%
    dplyr::mutate(
      FlagLabel = dplyr::case_when(
        .data$Flag == -2 ~ "Red Low",
        .data$Flag == -1 ~ "Amber Low",
        .data$Flag == 0 ~ "Normal",
        .data$Flag == 1 ~ "Amber High",
        .data$Flag == 2 ~ "Red High",
        TRUE ~ "Unknown"
      )
    )

  latest_flagged <- latest_month %>%
    dplyr::filter(.data$Flag != 0) %>%
    nrow()

  data.frame(
    Statistic = c(
      paste0("Total ", strGroupLevel, "s"),
      "Total Months",
      "Total Observations",
      paste0("Currently Flagged ", strGroupLevel, "s"),
      "Red Flags (All Time)",
      "Amber Flags (All Time)"
    ),
    Value = c(
      n_groups,
      n_months,
      nrow(dfFlagged),
      latest_flagged,
      sum(flag_summary$n[flag_summary$Flag %in% c(-2, 2)]),
      sum(flag_summary$n[flag_summary$Flag %in% c(-1, 1)])
    )
  )
}

#' Generate Flagged Groups Table
#'
#' Internal function to create a table of currently flagged groups.
#'
#' @param dfFlagged Flagged timeline data frame.
#'
#' @return A data frame of flagged groups at the latest time point.
#'
#' @keywords internal
.MakeFlaggedGroupsTable <- function(dfFlagged) {
  flag_labels <- c(
    "-2" = "Red Low",
    "-1" = "Amber Low",
    "0" = "Normal",
    "1" = "Amber High",
    "2" = "Red High"
  )

  latest_month <- max(dfFlagged$NMonth)

  dfFlagged %>%
    dplyr::filter(.data$NMonth == latest_month, .data$Flag != 0) %>%
    dplyr::mutate(
      FlagLabel = flag_labels[as.character(.data$Flag)]
    ) %>%
    dplyr::select(
      "GroupID",
      "NMonth",
      "Numerator",
      "Denominator",
      "Metric",
      "Score",
      "Flag",
      "FlagLabel"
    ) %>%
    dplyr::arrange(dplyr::desc(abs(.data$Flag)), dplyr::desc(abs(.data$Score)))
}
