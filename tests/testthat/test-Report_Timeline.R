# Tests for Report_Timeline function

# Helper to create test flagged data
make_test_flagged_data <- function() {
  dfSubjects <- data.frame(
    SubjectID = c(1, 2, 3, 4),
    SiteID = c("A", "A", "B", "B")
  )
  dfNumerator <- data.frame(
    SubjectID = c(1, 1, 2, 3, 4, 4, 4),
    EventDate = as.Date(c(
      "2022-01-01", "2022-01-15", "2022-02-01",
      "2022-01-10", "2022-01-05", "2022-01-20", "2022-02-01"
    ))
  )
  dfDenominator <- data.frame(
    SubjectID = c(1, 1, 2, 2, 3, 3, 4, 4),
    VisitDate = as.Date(c(
      "2022-01-01", "2022-01-20", "2022-01-01", "2022-02-01",
      "2022-01-01", "2022-01-15", "2022-01-01", "2022-02-01"
    ))
  )

  Timeline(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strGroupCol = "SiteID",
    strSubjectCol = "SubjectID",
    strNumeratorDateCol = "EventDate",
    strDenominatorDateCol = "VisitDate"
  ) %>%
    TimeZScore() %>%
    Flag()
}

test_that("Report_Timeline generates HTML file", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  dfFlagged <- make_test_flagged_data()

  temp_dir <- tempdir()
  output_file <- "test_report.html"

  report_path <- Report_Timeline(
    dfFlagged = dfFlagged,
    strStudyID = "TEST-001",
    strOutputDir = temp_dir,
    strOutputFile = output_file
  )

  expect_true(file.exists(report_path))
  expect_match(report_path, "\\.html$")

  unlink(report_path)
})


test_that("Report_Timeline handles NULL lCharts parameter", {
  dfFlagged <- make_test_flagged_data()

  temp_dir <- tempdir()
  output_file <- "test_null_charts.html"

  local_mocked_bindings(
    render = function(input, output_file, output_dir, params, envir, quiet) {
      file.create(file.path(output_dir, output_file))
    },
    .package = "rmarkdown"
  )

  report_path <- Report_Timeline(
    dfFlagged = dfFlagged,
    lCharts = NULL,
    strOutputDir = temp_dir,
    strOutputFile = output_file
  )

  expect_true(file.exists(report_path))
  unlink(report_path)
})


test_that("Report_Timeline extracts GroupLevel from data", {
  dfFlagged <- make_test_flagged_data()
  dfFlagged$GroupLevel <- "Country"

  temp_dir <- tempdir()
  output_file <- "test_group_level.html"

  captured_params <- NULL
  local_mocked_bindings(
    render = function(input, output_file, output_dir, params, envir, quiet) {
      captured_params <<- params
      file.create(file.path(output_dir, output_file))
    },
    .package = "rmarkdown"
  )

  report_path <- Report_Timeline(
    dfFlagged = dfFlagged,
    strGroupLevel = NULL,
    strOutputDir = temp_dir,
    strOutputFile = output_file
  )

  expect_equal(captured_params$strGroupLevel, "Country")
  unlink(report_path)
})


test_that("Report_Timeline uses default Site when GroupLevel not in data", {
  dfFlagged <- make_test_flagged_data()
  dfFlagged$GroupLevel <- NULL

  temp_dir <- tempdir()
  output_file <- "test_default_level.html"

  captured_params <- NULL
  local_mocked_bindings(
    render = function(input, output_file, output_dir, params, envir, quiet) {
      captured_params <<- params
      file.create(file.path(output_dir, output_file))
    },
    .package = "rmarkdown"
  )

  report_path <- Report_Timeline(
    dfFlagged = dfFlagged,
    strGroupLevel = NULL,
    strOutputDir = temp_dir,
    strOutputFile = output_file
  )

  expect_equal(captured_params$strGroupLevel, "Site")
  unlink(report_path)
})


test_that("Report_Timeline generates default filename when NULL", {
  dfFlagged <- make_test_flagged_data()

  temp_dir <- tempdir()

  local_mocked_bindings(
    render = function(input, output_file, output_dir, params, envir, quiet) {
      file.create(file.path(output_dir, output_file))
    },
    .package = "rmarkdown"
  )

  report_path <- Report_Timeline(
    dfFlagged = dfFlagged,
    strOutputDir = temp_dir,
    strOutputFile = NULL
  )

  expect_match(basename(report_path), "^Timeline_Report_\\d{4}-\\d{2}-\\d{2}\\.html$")
  unlink(report_path)
})


test_that("Report_Timeline uses pre-computed lCharts", {
  dfFlagged <- make_test_flagged_data()
  lCharts <- MakeCharts_Timeline(dfFlagged)

  temp_dir <- tempdir()
  output_file <- "test_precomputed.html"

  captured_params <- NULL
  local_mocked_bindings(
    render = function(input, output_file, output_dir, params, envir, quiet) {
      captured_params <<- params
      file.create(file.path(output_dir, output_file))
    },
    .package = "rmarkdown"
  )

  report_path <- Report_Timeline(
    dfFlagged = dfFlagged,
    lCharts = lCharts,
    strOutputDir = temp_dir,
    strOutputFile = output_file
  )

  expect_equal(captured_params$lCharts, lCharts)
  unlink(report_path)
})


test_that("Report_Timeline passes all parameters to render", {
  dfFlagged <- make_test_flagged_data()

  temp_dir <- tempdir()
  output_file <- "test_params.html"

  captured_params <- NULL
  local_mocked_bindings(
    render = function(input, output_file, output_dir, params, envir, quiet) {
      captured_params <<- params
      file.create(file.path(output_dir, output_file))
    },
    .package = "rmarkdown"
  )

  report_path <- Report_Timeline(
    dfFlagged = dfFlagged,
    strStudyID = "STUDY-123",
    strGroupLevel = "Region",
    strOutputDir = temp_dir,
    strOutputFile = output_file
  )

  expect_equal(captured_params$strStudyID, "STUDY-123")
  expect_equal(captured_params$strGroupLevel, "Region")
  expect_s3_class(captured_params$dfFlagged, "data.frame")
  expect_true("lCharts" %in% names(captured_params))
  expect_s3_class(captured_params$dfSummary, "data.frame")
  expect_s3_class(captured_params$dfFlaggedGroups, "data.frame")
  expect_true(!is.null(captured_params$strReportDate))

  unlink(report_path)
})


test_that(".MakeSummaryStats returns correct structure", {
  dfFlagged <- data.frame(
    GroupID = rep(c("A", "B", "C"), each = 3),
    NMonth = rep(1:3, 3),
    Flag = c(0, 0, 1, -1, 0, 2, 0, 0, -2)
  )

  result <- gsm.timez:::.MakeSummaryStats(dfFlagged, "Site")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("Statistic", "Value"))
  expect_true(nrow(result) > 0)
})


test_that(".MakeFlaggedGroupsTable filters to latest month", {
  dfFlagged <- data.frame(
    GroupID = rep(c("A", "B"), each = 3),
    NMonth = rep(1:3, 2),
    Numerator = 1:6,
    Denominator = 10,
    Metric = (1:6) / 10,
    Score = c(-1, 0, 2, 0, 1, -2),
    Flag = c(-1, 0, 2, 0, 1, -2)
  )

  result <- gsm.timez:::.MakeFlaggedGroupsTable(dfFlagged)

  expect_s3_class(result, "data.frame")
  expect_true(all(result$NMonth == 3))
  expect_true(all(result$Flag != 0))
})


test_that("Reporting workflow YAML exists and is valid", {
  report_yaml_path <- system.file(
    "workflow/3_reporting/Report.yaml",
    package = "gsm.timez"
  )

  expect_true(file.exists(report_yaml_path))

  yaml_content <- yaml::read_yaml(report_yaml_path)

  expect_true("meta" %in% names(yaml_content))
  expect_true("spec" %in% names(yaml_content))
  expect_true("steps" %in% names(yaml_content))
  expect_equal(yaml_content$meta$ID, "Report")
  expect_equal(yaml_content$meta$Type, "Reporting")
})
