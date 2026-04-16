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


create_test_funnel_data <- function(strMetricID = "Test_Metric") {
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

  dfAnalyzed <- Timeline(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strGroupCol = "SiteID",
    strSubjectCol = "SubjectID",
    strNumeratorDateCol = "EventDate",
    strDenominatorDateCol = "VisitDate"
  ) %>%
    TimeZScoreFunnel()

  dfFlagged <- dfAnalyzed %>%
    Flag(vThreshold = c(-1.5, -1, 2, 3)) %>%
    dplyr::mutate(MetricID = strMetricID)

  dfBounds <- TimeZScoreFunnel_PredictBounds(dfAnalyzed, vThreshold = c(-1.5, -1, 2, 3)) %>%
    dplyr::mutate(MetricID = strMetricID)

  list(dfFlagged = dfFlagged, dfBounds = dfBounds)
}


test_that("Widget passes dfBounds filtered by MetricID to VisualizeFunnelPlot", {
  lData <- create_test_funnel_data("Test_Metric")

  result <- Widget(
    lData$dfFlagged,
    "gsm.timez::VisualizeFunnelPlot",
    "Funnel Plot",
    dfBounds = lData$dfBounds
  )

  expect_type(result, "list")
  expect_equal(names(result), "Test_Metric")
  expect_equal(names(result$Test_Metric), "Funnel Plot")
  expect_s3_class(result$Test_Metric$`Funnel Plot`, "plotly")
})


test_that("Widget with dfBounds handles multiple metrics independently", {
  lData1 <- create_test_funnel_data("Metric_A")
  lData2 <- create_test_funnel_data("Metric_B")
  dfResults <- dplyr::bind_rows(lData1$dfFlagged, lData2$dfFlagged)
  dfBounds <- dplyr::bind_rows(lData1$dfBounds, lData2$dfBounds)

  result <- Widget(dfResults, "gsm.timez::VisualizeFunnelPlot", "Funnel Plot",
                   dfBounds = dfBounds)

  expect_equal(names(result), c("Metric_A", "Metric_B"))
  expect_s3_class(result$Metric_A$`Funnel Plot`, "plotly")
  expect_s3_class(result$Metric_B$`Funnel Plot`, "plotly")
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
