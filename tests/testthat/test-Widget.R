# Tests for Widget function

# Helper to create test data with MetricID
create_test_results <- function(strMetricID = "Test_Metric") {
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

  dfFlagged <- Timeline(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strGroupCol = "SiteID",
    strSubjectCol = "SubjectID",
    strNumeratorDateCol = "EventDate",
    strDenominatorDateCol = "VisitDate"
  ) %>%
    TimeZScore() %>%
    Flag() %>%
    dplyr::mutate(MetricID = strMetricID)

  dfFlagged
}


test_that("Widget works with Visualize", {
  dfResults <- create_test_results()

  result <- Widget(dfResults, "gsm.timez::Visualize", "Visualize")

  expect_type(result, "list")
  expect_equal(names(result), "Test_Metric")
  expect_equal(names(result$Test_Metric), "Visualize")
  expect_s3_class(result$Test_Metric$Visualize, "plotly")
  expect_equal(attr(result$Test_Metric$Visualize, "output_label"), "Visualize")
})


test_that("Widget works with VisualizeBoxplot", {
  dfResults <- create_test_results()

  result <- Widget(dfResults, "gsm.timez::VisualizeBoxplot", "VisualizeBoxplot")

  expect_type(result, "list")
  expect_equal(names(result), "Test_Metric")
  expect_equal(names(result$Test_Metric), "VisualizeBoxplot")
  expect_s3_class(result$Test_Metric$VisualizeBoxplot, "plotly")
  expect_equal(attr(result$Test_Metric$VisualizeBoxplot, "output_label"), "VisualizeBoxplot")
})


test_that("Widget handles multiple metrics", {
  dfResults1 <- create_test_results("Metric_A")
  dfResults2 <- create_test_results("Metric_B")
  dfResults <- dplyr::bind_rows(dfResults1, dfResults2)

  result <- Widget(dfResults, "gsm.timez::Visualize", "Visualize")

  expect_equal(names(result), c("Metric_A", "Metric_B"))
  expect_equal(names(result$Metric_A), "Visualize")
  expect_equal(names(result$Metric_B), "Visualize")
})
