# Tests for Visualize_Site function
#
# - Return type
#     - returns a ggplot object without dfBounds
#     - returns a ggplot object with dfBounds
# - dfBounds effect
#     - providing dfBounds adds exactly one extra layer
# - Flagged points layer
#     - renders flagged points when site has non-zero flags
# - Error handling
#     - errors when strSiteID is not found in the data
#     - errors when vFlag attribute is missing

create_funnel_flagged_data <- function() {
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
    Analyze_TimeZFunnel()

  list(
    dfFlagged = Flag(dfAnalyzed),
    dfBounds  = PredictBounds_TimeZFunnel(dfAnalyzed)
  )
}


test_that("Visualize_Site returns a ggplot object without dfBounds", {
  d <- create_funnel_flagged_data()
  result <- Visualize_Site(d$dfFlagged, strSiteID = "A")

  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("Visualize_Site returns a ggplot object with funnel dfBounds", {
  d <- create_funnel_flagged_data()
  result <- Visualize_Site(d$dfFlagged, dfBounds = d$dfBounds, strSiteID = "A")

  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("Visualize_Site adds exactly one extra layer when dfBounds is provided", {
  d <- create_funnel_flagged_data()
  result_without <- Visualize_Site(d$dfFlagged, strSiteID = "A")
  result_with <- Visualize_Site(d$dfFlagged, dfBounds = d$dfBounds, strSiteID = "A")

  expect_equal(length(result_with$layers), length(result_without$layers) + 1)
})


test_that("Visualize_Site errors on invalid strSiteID", {
  d <- create_funnel_flagged_data()
  expect_error(Visualize_Site(d$dfFlagged, strSiteID = "INVALID"), "not found")
})


test_that("Visualize_Site errors when vFlag attribute is missing", {
  d <- create_funnel_flagged_data()
  dfFlagged_no_attr <- d$dfFlagged
  attr(dfFlagged_no_attr, "vFlag") <- NULL

  expect_error(Visualize_Site(dfFlagged_no_attr, strSiteID = "A"), "vFlag")
})


test_that("Visualize_Site renders flagged points layer when site has non-zero flags", {
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

  # Very tight thresholds ensure non-zero Z-scores produce flagged points
  dfFlagged <- Timeline(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strGroupCol = "SiteID",
    strSubjectCol = "SubjectID",
    strNumeratorDateCol = "EventDate",
    strDenominatorDateCol = "VisitDate"
  ) %>%
    Analyze_TimeZFunnel() %>%
    Flag(vThreshold = c(-0.001, 0.001), vFlag = c(-1L, 0L, 1L))

  result <- Visualize_Site(dfFlagged, strSiteID = "A")
  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})
