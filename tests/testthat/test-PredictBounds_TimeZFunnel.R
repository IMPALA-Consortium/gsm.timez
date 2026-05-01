# Tests for PredictBounds_TimeZFunnel function

test_that("PredictBounds_TimeZFunnel returns correct column names", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B", "A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20, 15, 25),
    Denominator = c(100, 200, 150, 250),
    DenominatorMonth = as.Date(c("2022-01-01", "2022-01-01", "2022-02-01", "2022-02-01")),
    NMonth = c(1, 1, 2, 2)
  )

  dfAnalyzed <- Analyze_TimeZFunnel(dfTimeline)
  dfBounds <- PredictBounds_TimeZFunnel(dfAnalyzed)

  expected_cols <- c("NMonth", "Threshold", "Denominator", "LogDenominator", "Numerator", "Metric")
  expect_equal(names(dfBounds), expected_cols)
})


test_that("PredictBounds_TimeZFunnel includes 0 threshold for mean line", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B"),
    GroupLevel = "Site",
    Numerator = c(10, 20),
    Denominator = c(100, 200),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  dfAnalyzed <- Analyze_TimeZFunnel(dfTimeline)
  dfBounds <- PredictBounds_TimeZFunnel(dfAnalyzed, vThreshold = c(-2, 2))

  # Should include -2, 0, 2
  expect_true(0 %in% dfBounds$Threshold)
  expect_setequal(unique(dfBounds$Threshold), c(-2, 0, 2))
})


test_that("PredictBounds_TimeZFunnel has multiple rows per NMonth per Threshold", {
  skip_if_not_installed("gsm.core")

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

  dfAnalyzed <- Analyze_TimeZFunnel(dfTimeline)
  dfBounds <- PredictBounds_TimeZFunnel(dfAnalyzed, vThreshold = c(-2, 2))

  n_months <- length(unique(dfAnalyzed$NMonth))
  n_thresholds <- 3 # -2, 0, 2

  # Should have multiple rows per month per threshold (across denominator range)
  expect_gt(nrow(dfBounds), n_months * n_thresholds)

  # Each month should have all thresholds
  for (month in unique(dfBounds$NMonth)) {
    month_thresholds <- dfBounds %>%
      dplyr::filter(NMonth == month) %>%
      dplyr::pull(Threshold) %>%
      unique()
    expect_setequal(month_thresholds, c(-2, 0, 2))
  }
})


test_that("PredictBounds_TimeZFunnel errors on missing columns", {
  dfInvalid <- data.frame(
    NMonth = 1,
    GroupID = "A",
    GroupLevel = "Site",
    Numerator = 10
    # Missing Denominator and Metric
  )

  expect_error(PredictBounds_TimeZFunnel(dfInvalid))
})


test_that("PredictBounds_TimeZFunnel sets Metric to NA for sparse months", {
  skip_if_not_installed("gsm.core")

  # 6 sites in months 1-3, only 1 site in month 4 (~17% of peak -> sparse)
  dfTimeline <- data.frame(
    GroupID = c(
      "A", "B", "C", "D", "E", "F",
      "A", "B", "C", "D", "E", "F",
      "A", "B", "C", "D", "E", "F",
      "A"
    ),
    GroupLevel = "Site",
    Numerator = rep(10, 19),
    Denominator = rep(100, 19),
    DenominatorMonth = as.Date(c(
      rep("2022-01-01", 6), rep("2022-02-01", 6),
      rep("2022-03-01", 6), "2022-04-01"
    )),
    NMonth = c(rep(1, 6), rep(2, 6), rep(3, 6), 4)
  )

  dfAnalyzed <- Analyze_TimeZFunnel(dfTimeline)
  dfBounds <- PredictBounds_TimeZFunnel(dfAnalyzed, nMinSiteFraction = 0.2)

  expect_true(all(is.na(dfBounds$Metric[dfBounds$NMonth == 4])))
  expect_true(all(!is.na(dfBounds$Metric[dfBounds$NMonth < 4])))
})


test_that("PredictBounds_TimeZFunnel nMinSiteFraction = 0 disables NA behaviour", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c(
      "A", "B", "C", "D", "E",
      "A", "B", "C", "D", "E",
      "A", "B", "C", "D", "E",
      "A"
    ),
    GroupLevel = "Site",
    Numerator = rep(10, 16),
    Denominator = rep(100, 16),
    DenominatorMonth = as.Date(c(
      rep("2022-01-01", 5), rep("2022-02-01", 5),
      rep("2022-03-01", 5), "2022-04-01"
    )),
    NMonth = c(rep(1, 5), rep(2, 5), rep(3, 5), 4)
  )

  dfAnalyzed <- Analyze_TimeZFunnel(dfTimeline)
  dfBounds <- PredictBounds_TimeZFunnel(dfAnalyzed, nMinSiteFraction = 0)

  expect_true(all(!is.na(dfBounds$Metric)))
})


test_that("PredictBounds_TimeZFunnel output for single month matches direct gsm.core call", {
  skip_if_not_installed("gsm.core")

  dfTimeline <- data.frame(
    GroupID = c("A", "B", "C"),
    GroupLevel = "Site",
    Numerator = c(10, 20, 15),
    Denominator = c(100, 200, 150),
    DenominatorMonth = as.Date("2022-01-01"),
    NMonth = 1
  )

  dfAnalyzed <- Analyze_TimeZFunnel(dfTimeline)

  # Call via PredictBounds_TimeZFunnel
  dfBounds_wrapper <- PredictBounds_TimeZFunnel(dfAnalyzed, vThreshold = c(-2, 2))

  # Call gsm.core directly on the same data
  dfBounds_direct <- gsm.core::Analyze_NormalApprox_PredictBounds(
    dfAnalyzed,
    vThreshold = c(-2, 2),
    strType = "rate"
  )

  # Results should match (wrapper just adds NMonth column)
  expect_equal(
    dfBounds_wrapper %>% dplyr::select(-NMonth),
    dfBounds_direct
  )
})


test_that("PredictBounds_TimeZFunnel works with clindata", {
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

  dfAnalyzed <- Analyze_TimeZFunnel(dfTimeline)
  dfBounds <- PredictBounds_TimeZFunnel(dfAnalyzed)

  expect_true(nrow(dfBounds) > 0)
  expected_cols <- c("NMonth", "Threshold", "Denominator", "LogDenominator", "Numerator", "Metric")
  expect_equal(names(dfBounds), expected_cols)
})
