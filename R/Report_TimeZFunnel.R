#' Generate a custom KRI HTML report
#'
#' Wrapper around [gsm.kri::Report_KRI()] that uses the custom report template
#' bundled with `gsm.timez` (`inst/report/Report_TimeZFunnel.Rmd`).
#'
#' @inheritParams gsm.kri::Report_KRI
#'
#' @return Path to the generated HTML report (invisibly).
#'
#' @export
Report_TimeZFunnel <- function(
    lCharts = NULL,
    dfResults = NULL,
    dfMetrics = NULL,
    dfGroups = NULL,
    strOutputDir = getwd(),
    strOutputFile = NULL,
    strInputPath = system.file("report", "Report_TimeZFunnel.Rmd", package = "gsm.timez")) {
  gsm.kri::Report_KRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfMetrics = dfMetrics,
    dfGroups = dfGroups,
    strOutputDir = strOutputDir,
    strOutputFile = strOutputFile,
    strInputPath = strInputPath
  )
}
