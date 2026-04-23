# Tests for TimeZScoreFunnel function

test_that("TimeZScoreFunnel requires gsm.core package", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20),
    Denominator = c(100, 200),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  expect_no_error(TimeZScoreFunnel(dfTimeline))
})


test_that("TimeZScoreFunnel returns correct column names", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B", "C"),
    GroupLevel = "Site",
    Numerator = c(10, 20, 15),
    Denominator = c(100, 200, 150),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  result <- TimeZScoreFunnel(dfTimeline)

  # Should have columns from Analyze_NormalApprox
  expect_true("GroupID" %in% names(result))
  expect_true("Metric" %in% names(result))
  expect_true("Score" %in% names(result))
  expect_true("OverallMetric" %in% names(result))
  expect_true("NMonth" %in% names(result))
})


test_that("TimeZScoreFunnel calculates Metric correctly", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20),
    Denominator = c(100, 200),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  result <- TimeZScoreFunnel(dfTimeline)

  # Metric should be Numerator / Denominator
  expected_metric <- dfTimeline$Numerator / dfTimeline$Denominator
  expect_equal(result$Metric, expected_metric)
})


test_that("TimeZScoreFunnel works with multiple months", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B", "A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20, 15, 25),
    Denominator = c(100, 200, 150, 250),
    DenominatorMonth = as.Date(c("2022-01-01", "2022-01-01", "2022-02-01", "2022-02-01")),
    NMonth = c(1, 1, 2, 2)
  )

  result <- TimeZScoreFunnel(dfTimeline)

  # Should have same number of rows
  expect_equal(nrow(result), nrow(dfTimeline))

  # Each month should have its own OverallMetric
  month1_overall <- unique(result$OverallMetric[result$NMonth == 1])
  month2_overall <- unique(result$OverallMetric[result$NMonth == 2])

  expect_length(month1_overall, 1)
  expect_length(month2_overall, 1)
})


test_that("TimeZScoreFunnel errors on invalid input", {
  skip_if_not_installed("gsm.core")

  # Missing required columns
  invalid_df <- data.frame(
    subject = 1:3,
    GroupID = c("A", "A", "B")
  )

  expect_error(TimeZScoreFunnel(invalid_df))
})


test_that("TimeZScoreFunnel works with clindata", {
  skip_if_not_installed("gsm.core")
  skip_if_not_installed("clindata")

  dfTimeline <-
    Timeline(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol = "invid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )

  result <- TimeZScoreFunnel(dfTimeline)

  # Result should have same number of rows as input
  expect_equal(nrow(result), nrow(dfTimeline))

  # Should have Score column
  expect_true("Score" %in% names(result))

  # Score should be numeric
  expect_type(result$Score, "double")
})


test_that("TimeZScoreFunnel preserves extra columns from input", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20),
    Denominator = c(100, 200),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  result <- TimeZScoreFunnel(dfTimeline)

  expect_true("DenominatorMonth" %in% names(result))
  expect_equal(result$DenominatorMonth, dfTimeline$DenominatorMonth)
})
