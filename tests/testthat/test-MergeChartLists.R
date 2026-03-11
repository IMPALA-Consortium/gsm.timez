# Tests for MergeChartLists function

test_that("MergeChartLists combines charts per metric", {
  list1 <- list(Metric_A = list(chart1 = "a"), Metric_B = list(chart1 = "b"))
  list2 <- list(Metric_A = list(chart2 = "c"), Metric_B = list(chart2 = "d"))

  result <- MergeChartLists(list1 = list1, list2 = list2)

  expect_equal(names(result), c("Metric_A", "Metric_B"))
  expect_equal(names(result$Metric_A), c("chart1", "chart2"))
  expect_equal(names(result$Metric_B), c("chart1", "chart2"))
})


test_that("MergeChartLists handles missing metrics", {
  list1 <- list(Metric_A = list(chart1 = "a"))
  list2 <- list(Metric_B = list(chart2 = "b"))

  result <- MergeChartLists(list1 = list1, list2 = list2)

  expect_equal(sort(names(result)), c("Metric_A", "Metric_B"))
})
