# Tests for TimeZScore_PredictBounds function

test_that("TimeZScore_PredictBounds returns correct column names", {
  dfTimeline <- data.frame(
    GroupID = c("A", "B", "A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20, 15, 25),
    Denominator = c(100, 200, 150, 250),
    DenominatorMonth = as.Date(c("2022-01-01", "2022-01-01", "2022-02-01", "2022-02-01")),
    NMonth = c(1, 1, 2, 2)
  )

  dfAnalyzed <- TimeZScore(dfTimeline)
  dfBounds <- TimeZScore_PredictBounds(dfAnalyzed)

  expect_equal(names(dfBounds), c("NMonth", "Threshold", "Metric"))
})


test_that("TimeZScore_PredictBounds includes 0 threshold for mean line", {
  dfTimeline <- data.frame(
    GroupID = c("A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20),
    Denominator = c(100, 200),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  dfAnalyzed <- TimeZScore(dfTimeline)
  dfBounds <- TimeZScore_PredictBounds(dfAnalyzed, vThreshold = c(-2, 2))

  # Should include -2, 0, 2
  expect_true(0 %in% dfBounds$Threshold)
  expect_setequal(unique(dfBounds$Threshold), c(-2, 0, 2))
})


test_that("TimeZScore_PredictBounds calculates correct Metric values", {
  dfTimeline <- data.frame(
    GroupID = c("A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20),
    Denominator = c(100, 200),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  dfAnalyzed <- TimeZScore(dfTimeline)
  dfBounds <- TimeZScore_PredictBounds(dfAnalyzed, vThreshold = c(-2, 2))

  # Get the mean and sd from analyzed data
  metric_mean <- unique(dfAnalyzed$metric_mean)
  metric_sd <- unique(dfAnalyzed$metric_sd)

  # Check that bounds are calculated correctly
  bound_at_2 <- dfBounds %>% dplyr::filter(Threshold == 2) %>% dplyr::pull(Metric)
  bound_at_neg2 <- dfBounds %>% dplyr::filter(Threshold == -2) %>% dplyr::pull(Metric)
  bound_at_0 <- dfBounds %>% dplyr::filter(Threshold == 0) %>% dplyr::pull(Metric)

  expect_equal(bound_at_2, metric_mean + 2 * metric_sd)
  expect_equal(bound_at_neg2, metric_mean - 2 * metric_sd)
  expect_equal(bound_at_0, metric_mean)
})


test_that("TimeZScore_PredictBounds has one row per NMonth per Threshold", {
  dfTimeline <- data.frame(
    GroupID = c("A", "B", "A", "B", "A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20, 15, 25, 20, 30),
    Denominator = c(100, 200, 150, 250, 200, 300),
    DenominatorMonth = as.Date(c(
      "2022-01-01", "2022-01-01",
      "2022-02-01", "2022-02-01",
      "2022-03-01", "2022-03-01"
    )),
    NMonth = c(1, 1, 2, 2, 3, 3)
  )

  dfAnalyzed <- TimeZScore(dfTimeline)
  dfBounds <- TimeZScore_PredictBounds(dfAnalyzed, vThreshold = c(-3, -2, 2, 3))

  n_months <- length(unique(dfAnalyzed$NMonth))
  n_thresholds <- 5 # -3, -2, 0, 2, 3

  expect_equal(nrow(dfBounds), n_months * n_thresholds)
})


test_that("TimeZScore_PredictBounds errors on missing columns", {
  dfInvalid <- data.frame(
    NMonth = 1,
    metric_mean = 0.1
    # Missing metric_sd
)

  expect_error(TimeZScore_PredictBounds(dfInvalid))
})


test_that("TimeZScore_PredictBounds works with clindata", {
  skip_if_not_installed("clindata")

  dfTimeline <-
    Timeline(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )

  dfAnalyzed <- TimeZScore(dfTimeline)
  dfBounds <- TimeZScore_PredictBounds(dfAnalyzed)

  expect_true(nrow(dfBounds) > 0)
  expect_equal(names(dfBounds), c("NMonth", "Threshold", "Metric"))
})
