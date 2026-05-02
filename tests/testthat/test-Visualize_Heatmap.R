# Tests for Visualize_Heatmap function
#
# - Return type
#     - returns a ggplot object
#     - plot can be built without errors
# - Plot structure
#     - uses GeomTile layer
#     - NMonth on x-axis, GroupID on y-axis
#     - Flag used as fill colour
#     - Flag converted to factor with correct levels (-2 to 2)
#     - y-axis is reversed (alphabetical top-to-bottom)
# - Integration with clindata
# - nSites argument
#     - nSites = N limits displayed sites and adds informative title
#     - nSites = NULL shows all sites with no title
#     - nSites > total shows all sites with correct title

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
    Analyze_TimeZFunnel() %>%
    Flag()

  dfFlagged
}


test_that("Visualize_Heatmap returns a ggplot object", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)

  expect_s3_class(result, "ggplot")
})


test_that("Visualize_Heatmap plot can be built without errors", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)

  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("Visualize_Heatmap contains GeomTile layer", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)

  layer_classes <- sapply(result$layers, function(x) class(x$geom)[1])
  expect_true("GeomTile" %in% layer_classes)
})


test_that("Visualize_Heatmap uses NMonth on x-axis and GroupID on y-axis", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)

  expect_true(grepl("NMonth", deparse(result$mapping$x)))
  expect_true(grepl("GroupID", deparse(result$mapping$y)))
})


test_that("Visualize_Heatmap uses Flag for fill color", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)

  expect_true(grepl("Flag", deparse(result$mapping$fill)))
})


test_that("Visualize_Heatmap converts Flag to factor with correct levels", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)

  expect_s3_class(result$data$Flag, "factor")
  expect_equal(levels(result$data$Flag), c("-2", "-1", "0", "1", "2"))
})


test_that("Visualize_Heatmap has reversed y-axis (time flows down)", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)
  built <- ggplot2::ggplot_build(result)

  y_limits <- built$layout$panel_scales_y[[1]]$limits
  expect_equal(y_limits, rev(sort(unique(dfFlagged$GroupID))))
})


test_that("Visualize_Heatmap works with clindata", {
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
  dfFlagged <- Flag(dfAnalyzed)

  result <- Visualize_Heatmap(dfFlagged)

  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("Visualize_Heatmap with nSites limits sites and adds correct title", {
  skip_if_not_installed("gsm.core")
  skip_if_not_installed("clindata")

  dfFlagged <- Timeline(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d")),
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  ) %>%
    Analyze_TimeZFunnel() %>%
    Flag()

  nTotal <- length(unique(dfFlagged$GroupID))
  result <- Visualize_Heatmap(dfFlagged, nSites = 2)

  expect_lte(length(unique(result$data$GroupID)), 2)
  expect_match(result$labels$title, sprintf("Showing top 2 of %d sites", nTotal))
})


test_that("Visualize_Heatmap with nSites = NULL shows all sites and no title", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data()
  result <- Visualize_Heatmap(dfFlagged)

  expect_equal(length(unique(result$data$GroupID)), length(unique(dfFlagged$GroupID)))
  expect_null(result$labels$title)
})


test_that("Visualize_Heatmap with nSites > total sites shows all sites with correct title", {
  skip_if_not_installed("gsm.core")

  dfFlagged <- create_flagged_funnel_data() # only 2 sites: A, B
  result <- Visualize_Heatmap(dfFlagged, nSites = 99)

  expect_equal(length(unique(result$data$GroupID)), 2)
  expect_match(result$labels$title, "Showing top 2 of 2 sites")
})
