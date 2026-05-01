# Tests for Flag function

make_analyzed <- function(group_ids, nmonths, numerator = 10, denominator = 100) {
  n <- length(group_ids)
  dfTimeline <- data.frame(
    GroupID = group_ids,
    GroupLevel = "Site",
    Numerator = rep(numerator, n),
    Denominator = rep(denominator, n),
    DenominatorMonth = as.Date("2022-01-01") + (nmonths - 1) * 31,
    NMonth = nmonths
  )
  Analyze_TimeZFunnel(dfTimeline)
}


test_that("Flag returns correct column names including Flag", {
  dfAnalyzed <- make_analyzed(c("A", "B"), c(1, 1))
  result <- Flag(dfAnalyzed)
  expect_true("Flag" %in% names(result))
})


test_that("Flag stores vThreshold and vFlag as attributes", {
  dfAnalyzed <- make_analyzed(c("A", "B"), c(1, 1))
  result <- Flag(dfAnalyzed, vThreshold = c(-3, -2, 2, 3), vFlag = c(-2, -1, 0, 1, 2))
  expect_equal(attr(result, "vThreshold"), c(-3, -2, 2, 3))
  expect_equal(attr(result, "vFlag"), c(-2, -1, 0, 1, 2))
})


test_that("Flag sets Flag = NA for sparse months", {
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
  result <- Flag(dfAnalyzed, nMinSiteFraction = 0.2)

  expect_true(all(is.na(result$Flag[result$NMonth == 4])))
  expect_true(all(!is.na(result$Flag[result$NMonth < 4])))
})


test_that("Flag nMinSiteFraction = 0 disables NA masking", {
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
  result <- Flag(dfAnalyzed, nMinSiteFraction = 0)

  expect_true(all(!is.na(result$Flag)))
})


test_that("Flag works with clindata", {
  skip_if_not_installed("clindata")

  dfAnalyzed <-
    Timeline(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol = "invid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    ) %>%
    Analyze_TimeZFunnel()

  result <- Flag(dfAnalyzed)

  expect_true("Flag" %in% names(result))
  expect_true(nrow(result) > 0)
})
