# Tests for Visualize_Funnel function
#
# - Return type
#     - returns a ggplot object
#     - plot can be built without errors
# - NMonth selection
#     - NMonth = NULL defaults to largest non-sparse month
#     - explicit valid NMonth is respected
# - Error handling
#     - errors when NMonth is sparse or absent
#     - errors when vFlag attribute is missing from dfFlagged

# Helper returning both dfFlagged and dfBounds
create_funnel_inputs <- function() {
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


test_that("Visualize_Funnel returns a ggplot object", {
  skip_if_not_installed("gsm.core")

  inputs <- create_funnel_inputs()
  result <- Visualize_Funnel(inputs$dfFlagged, inputs$dfBounds)

  expect_s3_class(result, "ggplot")
})


test_that("Visualize_Funnel plot can be built without errors", {
  skip_if_not_installed("gsm.core")

  inputs <- create_funnel_inputs()
  result <- Visualize_Funnel(inputs$dfFlagged, inputs$dfBounds)

  expect_no_error(ggplot2::ggplot_build(result))
})


test_that("Visualize_Funnel NMonth=NULL defaults to largest non-sparse month", {
  skip_if_not_installed("gsm.core")

  inputs <- create_funnel_inputs()
  result <- Visualize_Funnel(inputs$dfFlagged, inputs$dfBounds)

  valid_months <- inputs$dfFlagged %>%
    dplyr::filter(!is.na(.data$Flag)) %>%
    dplyr::pull(.data$NMonth) %>%
    unique()

  expect_true(grepl(paste0("Month: ", max(valid_months)), result$labels$title))
})


test_that("Visualize_Funnel respects an explicit valid NMonth", {
  skip_if_not_installed("gsm.core")

  inputs <- create_funnel_inputs()
  result <- Visualize_Funnel(inputs$dfFlagged, inputs$dfBounds, NMonth = 1)

  expect_true(grepl("Month: 1", result$labels$title))
})


test_that("Visualize_Funnel errors on a sparse or absent NMonth", {
  skip_if_not_installed("gsm.core")

  inputs <- create_funnel_inputs()

  expect_error(
    Visualize_Funnel(inputs$dfFlagged, inputs$dfBounds, NMonth = 99),
    "sparse or not present"
  )
})


test_that("Visualize_Funnel errors when vFlag attribute is missing", {
  skip_if_not_installed("gsm.core")

  inputs <- create_funnel_inputs()
  attr(inputs$dfFlagged, "vFlag") <- NULL

  expect_error(
    Visualize_Funnel(inputs$dfFlagged, inputs$dfBounds),
    "vFlag"
  )
})
