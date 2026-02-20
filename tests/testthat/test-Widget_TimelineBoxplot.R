# Tests for Widget_TimelineBoxplot function

test_that("Widget_TimelineBoxplot returns a plotly object", {
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
    Flag()

  widget <- Widget_TimelineBoxplot(dfFlagged)

  expect_s3_class(widget, "plotly")
  expect_s3_class(widget, "htmlwidget")
})


test_that("Widget_TimelineBoxplot filters months correctly", {
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
    Flag()

  widget <- Widget_TimelineBoxplot(dfFlagged, nMonths = 1)

  expect_s3_class(widget, "plotly")
})


test_that("Widget_TimelineBoxplot wraps VisualizeBoxplot output", {
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
    Flag()

  gg_plot <- VisualizeBoxplot(dfFlagged)
  widget <- Widget_TimelineBoxplot(dfFlagged)

  expect_s3_class(gg_plot, "ggplot")
  expect_s3_class(widget, "plotly")
})
