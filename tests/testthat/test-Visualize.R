# Tests for Visualize function

# Helper to create test data
create_flagged_data <- function() {
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

  dfFlagged
}


test_that("Visualize returns a ggplot object", {
  dfFlagged <- create_flagged_data()
  result <- Visualize(dfFlagged)

  expect_s3_class(result, "ggplot")
})


test_that("Visualize plot can be built without errors", {
  dfFlagged <- create_flagged_data()
  result <- Visualize(dfFlagged)

  # ggplot_build will error if plot has invalid data/aesthetics
  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("Visualize contains expected layers", {
  dfFlagged <- create_flagged_data()
  result <- Visualize(dfFlagged)

  # Should have geom_line and geom_point layers
  layer_classes <- sapply(result$layers, function(x) class(x$geom)[1])
  expect_true("GeomLine" %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)
})


test_that("Visualize uses correct aesthetics", {
  dfFlagged <- create_flagged_data()
  result <- Visualize(dfFlagged)

  # Check x and y mappings (aesthetics use .data$ notation, so check as strings)
  expect_true(grepl("NMonth", deparse(result$mapping$x)))
  expect_true(grepl("Metric", deparse(result$mapping$y)))
  expect_true(grepl("GroupID", deparse(result$mapping$group)))
})


test_that("Visualize converts Flag to factor", {
  dfFlagged <- create_flagged_data()
  result <- Visualize(dfFlagged)

  # The data in the plot should have Flag as a factor
  expect_s3_class(result$data$Flag, "factor")
  expect_equal(levels(result$data$Flag), c("-2", "-1", "0", "1", "2"))
})
