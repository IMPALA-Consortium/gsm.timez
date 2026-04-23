# Tests for VisualizeFunnel function (heat map implementation)

# Helper to create test data using funnel scoring
create_flagged_funnel_data <- function() {
  skip_if_not_installed("gsm.core")

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
    TimeZScoreFunnel() %>%
    Flag()

  dfFlagged
}


test_that("VisualizeFunnel returns a ggplot object", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- VisualizeFunnel(dfFlagged)

  expect_s3_class(result, "ggplot")
})


test_that("VisualizeFunnel plot can be built without errors", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- VisualizeFunnel(dfFlagged)

  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("VisualizeFunnel contains GeomTile layer", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- VisualizeFunnel(dfFlagged)

  layer_classes <- sapply(result$layers, function(x) class(x$geom)[1])
  expect_true("GeomTile" %in% layer_classes)
})


test_that("VisualizeFunnel uses GroupID on x-axis and NMonth on y-axis", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- VisualizeFunnel(dfFlagged)

  expect_true(grepl("GroupID", deparse(result$mapping$x)))
  expect_true(grepl("NMonth", deparse(result$mapping$y)))
})


test_that("VisualizeFunnel uses Flag for fill color", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- VisualizeFunnel(dfFlagged)

  expect_true(grepl("Flag", deparse(result$mapping$fill)))
})


test_that("VisualizeFunnel converts Flag to factor with correct levels", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- VisualizeFunnel(dfFlagged)

  expect_s3_class(result$data$Flag, "factor")
  expect_equal(levels(result$data$Flag), c("-2", "-1", "0", "1", "2"))
})


test_that("VisualizeFunnel has reversed y-axis (time flows down)", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- VisualizeFunnel(dfFlagged)
  built <- ggplot2::ggplot_build(result)

  # Check that y-axis is reversed (trans should be "reverse")
  expect_equal(built$layout$panel_scales_y[[1]]$trans$name, "reverse")
})


test_that("VisualizeFunnel works with clindata", {
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

  dfAnalyzed <- TimeZScoreFunnel(dfTimeline)
  dfFlagged <- Flag(dfAnalyzed)

  result <- VisualizeFunnel(dfFlagged)

  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})
