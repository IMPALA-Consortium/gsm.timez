# Tests for VisualizeBoxplot function

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


test_that("VisualizeBoxplot returns a ggplot object", {
  dfFlagged <- create_flagged_data()
  result <- VisualizeBoxplot(dfFlagged)

  expect_s3_class(result, "ggplot")
})


test_that("VisualizeBoxplot plot can be built without errors", {
  dfFlagged <- create_flagged_data()
  result <- VisualizeBoxplot(dfFlagged)

  # ggplot_build will error if plot has invalid data/aesthetics
  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("VisualizeBoxplot contains expected layers", {
  dfFlagged <- create_flagged_data()
  result <- VisualizeBoxplot(dfFlagged)

  # Should have geom_point and geom_boxplot layers
  layer_classes <- sapply(result$layers, function(x) class(x$geom)[1])
  expect_true("GeomPoint" %in% layer_classes)
  expect_true("GeomBoxplot" %in% layer_classes)
})


test_that("VisualizeBoxplot uses correct aesthetics", {
  dfFlagged <- create_flagged_data()
  result <- VisualizeBoxplot(dfFlagged)

  # Check x and y mappings (aesthetics use .data$ notation, so check as strings)
  expect_true(grepl("NMonth", deparse(result$mapping$x)))
  expect_true(grepl("Metric", deparse(result$mapping$y)))
})


test_that("VisualizeBoxplot converts Flag and NMonth to factors", {
  dfFlagged <- create_flagged_data()
  result <- VisualizeBoxplot(dfFlagged)

  # Flag should be a factor
  expect_s3_class(result$data$Flag, "factor")
  expect_equal(levels(result$data$Flag), c("-2", "-1", "0", "1", "2"))

  # NMonth should be a factor
  expect_s3_class(result$data$NMonth, "factor")
})


test_that("VisualizeBoxplot filters by nMonths when provided", {
  dfFlagged <- create_flagged_data()

  # Get all months in the data
  all_months <- unique(dfFlagged$NMonth)

  # Filter to first month only
  result <- VisualizeBoxplot(dfFlagged, nMonths = 1)

  # Result data should only have month 1
  expect_equal(unique(as.numeric(as.character(result$data$NMonth))), 1)
})


test_that("VisualizeBoxplot shows all months when nMonths is NULL", {
  dfFlagged <- create_flagged_data()

  result <- VisualizeBoxplot(dfFlagged, nMonths = NULL)

  # Result should have same unique months as input
  result_months <- as.numeric(as.character(result$data$NMonth))
  expect_setequal(unique(result_months), unique(dfFlagged$NMonth))
})


test_that("VisualizeBoxplot filters by multiple nMonths values", {
  dfFlagged <- create_flagged_data()

  # Filter to months 1 and 2
  result <- VisualizeBoxplot(dfFlagged, nMonths = c(1, 2))

  # Result data should only have months 1 and 2
  result_months <- unique(as.numeric(as.character(result$data$NMonth)))
  expect_true(all(result_months %in% c(1, 2)))
})
