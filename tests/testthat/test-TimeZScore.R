# Tests for TimeZScore function

test_that("TimeZScore returns correct column names", {
  dfTimeline <-
    Timeline(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )

  result <- TimeZScore(dfTimeline)

  # Should have original columns plus Metric, metric_mean, metric_sd, scale, and Score
  expect_equal(
    names(result),
    c("GroupID", "GroupLevel", "Numerator", "Denominator", "DenominatorMonth", "NMonth", "Metric", "metric_mean", "metric_sd", "scale", "Score")
  )
})


test_that("TimeZScore works with clindata", {
  dfTimeline <-
    Timeline(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )

  result <- TimeZScore(dfTimeline)

  # Result should have same number of rows as input

  expect_equal(nrow(result), nrow(dfTimeline))

  # All groups should be present
  expect_setequal(result$GroupID, unique(dfTimeline$GroupID))

  # Should have Metric and Score columns
  expect_true("Metric" %in% names(result))
  expect_true("Score" %in% names(result))

  # Metric should be Numerator / Denominator
  expect_equal(result$Metric, result$Numerator / result$Denominator)
})


test_that("TimeZScore calculates z-scores correctly for month 1", {
  dfSubjects <- data.frame(
    SubjectID = c(1, 2, 3, 4),
    SiteID = c("A", "A", "B", "B")
  )
  dfNumerator <- data.frame(
    SubjectID = c(1, 1, 2, 3),
    EventDate = as.Date(c("2022-01-01", "2022-01-15", "2022-02-01", "2022-01-10"))
  )
  dfDenominator <- data.frame(
    SubjectID = c(1, 1, 2, 2, 3, 3, 4, 4),
    VisitDate = as.Date(c(
      "2022-01-01", "2022-01-20", "2022-01-01", "2022-02-01",
      "2022-01-01", "2022-01-15", "2022-01-01", "2022-02-01"
    ))
  )

  dfTimeline <- Timeline(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strGroupCol = "SiteID",
    strSubjectCol = "SubjectID",
    strNumeratorDateCol = "EventDate",
    strDenominatorDateCol = "VisitDate"
  )

  result <- TimeZScore(dfTimeline)

  # Score should be numeric
  expect_type(result$Score, "double")

  # For month 1 rows (NMonth == 1), if there are >= 2 Metrics,
  # Score should be calculated; otherwise 0
  month1_rows <- result %>% filter(NMonth == 1)
  if (nrow(month1_rows) >= 2) {
    # Check that Scores are calculated (not all zero unless sd is 0)
    month1_metrics <- month1_rows$Metric
    expected_mean <- mean(month1_metrics)
    expected_sd <- sd(month1_metrics)
    if (expected_sd > 0) {
      expected_scores <- (month1_metrics - expected_mean) / expected_sd
      expect_equal(month1_rows$Score, expected_scores)
    }
  }
})


test_that("TimeZScore returns zero Score when fewer than 2 Metrics", {
  # Create minimal timeline with only 1 row for month 1
  dfTimeline <- data.frame(
    GroupID = "A",
    GroupLevel = "Site",
    Numerator = 5,
    Denominator = 10,
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  result <- TimeZScore(dfTimeline)

  # With only 1 Metric, Score should be 0
  expect_equal(result$Score, 0)
})


test_that("TimeZScore errors on invalid input", {
  # Missing required columns
  invalid_df <- data.frame(
    subject = 1:3,
    GroupID = c("A", "A", "B")
  )

  expect_error(TimeZScore(invalid_df))
})


test_that("TimeZScore errors when NMonth column is missing", {
  # DataFrame without NMonth column
  invalid_df <- data.frame(
    GroupID = c("A", "A", "B"),
    GroupLevel = "siteid",
    Numerator = c(1, 2, 1),
    Denominator = c(2, 3, 2)
  )

  expect_error(TimeZScore(invalid_df))
})


test_that("TimeZScore uses cumulative window correctly for month 2", {
  dfSubjects <- data.frame(
    SubjectID = c(1, 2, 3, 4),
    SiteID = c("A", "A", "B", "B")
  )
  dfNumerator <- data.frame(
    SubjectID = c(1, 1, 2, 3),
    EventDate = as.Date(c("2022-01-01", "2022-01-15", "2022-02-01", "2022-01-10"))
  )
  dfDenominator <- data.frame(
    SubjectID = c(1, 1, 2, 2, 3, 3, 4, 4),
    VisitDate = as.Date(c(
      "2022-01-01", "2022-01-20", "2022-01-01", "2022-02-01",
      "2022-01-01", "2022-01-15", "2022-01-01", "2022-02-01"
    ))
  )

  dfTimeline <- Timeline(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strGroupCol = "SiteID",
    strSubjectCol = "SubjectID",
    strNumeratorDateCol = "EventDate",
    strDenominatorDateCol = "VisitDate"
  )

  result <- TimeZScore(dfTimeline)

  # Verify month 1 z-scores (using only month 1 data)
  month_1 <- result %>% filter(NMonth == 1)
  ratio_month_1 <- month_1$Numerator / month_1$Denominator
  expected_score_month_1 <- (ratio_month_1 - mean(ratio_month_1)) / sd(ratio_month_1)
  expect_equal(month_1$Score, expected_score_month_1)

  # Verify month 2 z-scores (using months 1 AND 2 combined)
  month_1_2 <- result %>% filter(NMonth %in% c(1, 2))
  ratio_month_1_2 <- month_1_2$Numerator / month_1_2$Denominator
  mean_ratio_1_2 <- mean(ratio_month_1_2)
  sd_ratio_1_2 <- sd(ratio_month_1_2)

  month_2 <- result %>% filter(NMonth == 2)
  ratio_month_2 <- month_2$Numerator / month_2$Denominator
  expected_score_month_2 <- (ratio_month_2 - mean_ratio_1_2) / sd_ratio_1_2
  expect_equal(month_2$Score, expected_score_month_2)
})


test_that("TimeZScore with bAdjustForSize = TRUE adjusts for sample size", {
  dfTimeline <- data.frame(
    GroupID = c("A", "B", "C"),
    GroupLevel = "Site",
    Numerator = c(10, 20, 15),
    Denominator = c(100, 400, 225),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  result_no_adjust <- TimeZScore(dfTimeline, bAdjustForSize = FALSE)
  result_adjust <- TimeZScore(dfTimeline, bAdjustForSize = TRUE)

  # Both should have the same mean and sd
  expect_equal(result_no_adjust$metric_mean, result_adjust$metric_mean)
  expect_equal(result_no_adjust$metric_sd, result_adjust$metric_sd)

  # Scale should differ: without adjustment it's just sd, with adjustment it's sd/sqrt(Denominator)
  expect_equal(result_no_adjust$scale, result_no_adjust$metric_sd)
  expect_equal(result_adjust$scale, result_adjust$metric_sd / sqrt(result_adjust$Denominator))

  # Scores should be different when bAdjustForSize = TRUE
  # Sites with larger Denominator should have more extreme scores when adjusted
  expect_false(all(result_no_adjust$Score == result_adjust$Score))
})


test_that("TimeZScore bAdjustForSize produces different scale per site", {
  dfTimeline <- data.frame(
    GroupID = c("Small", "Large"),
    GroupLevel = "Site",
    Numerator = c(5, 50),
    Denominator = c(10, 1000),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  result <- TimeZScore(dfTimeline, bAdjustForSize = TRUE)

  # With adjustment, Large site should have smaller scale (sd / sqrt(1000) < sd / sqrt(10))
  small_site <- result[result$GroupID == "Small", ]
  large_site <- result[result$GroupID == "Large", ]

  expect_true(large_site$scale < small_site$scale)
})
