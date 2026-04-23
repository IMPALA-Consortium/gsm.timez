# Tests for VisualizeSite function

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
    TimeZScoreFunnel()

  list(
    dfFlagged = Flag(dfAnalyzed),
    dfBounds  = TimeZScoreFunnel_PredictBounds(dfAnalyzed)
  )
}


test_that("VisualizeSite returns a ggplot object without dfBounds", {
  d <- create_funnel_flagged_data()
  result <- VisualizeSite(d$dfFlagged, strSiteID = "A")

  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("VisualizeSite returns a ggplot object with funnel dfBounds", {
  d <- create_funnel_flagged_data()
  result <- VisualizeSite(d$dfFlagged, dfBounds = d$dfBounds, strSiteID = "A")

  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("VisualizeSite adds exactly one extra layer when dfBounds is provided", {
  d <- create_funnel_flagged_data()
  result_without <- VisualizeSite(d$dfFlagged, strSiteID = "A")
  result_with    <- VisualizeSite(d$dfFlagged, dfBounds = d$dfBounds, strSiteID = "A")

  expect_equal(length(result_with$layers), length(result_without$layers) + 1)
})


test_that("VisualizeSite errors on invalid strSiteID", {
  d <- create_funnel_flagged_data()
  expect_error(VisualizeSite(d$dfFlagged, strSiteID = "INVALID"), "not found")
})
