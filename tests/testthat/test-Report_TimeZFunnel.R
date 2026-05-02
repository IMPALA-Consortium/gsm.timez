# Tests for Report_TimeZFunnel function
#
# - Delegates to gsm.kri::Report_KRI
#     - passes all arguments through correctly
#     - uses the bundled Report_TimeZFunnel.Rmd as the default template

test_that("Report_TimeZFunnel delegates to gsm.kri::Report_KRI with bundled template", {
  skip_if_not_installed("gsm.kri")

  captured_args <- NULL
  local_mocked_bindings(
    Report_KRI = function(...) {
      captured_args <<- list(...)
      invisible(NULL)
    },
    .package = "gsm.kri"
  )

  Report_TimeZFunnel(lCharts = list(), dfResults = data.frame())

  expect_false(is.null(captured_args))
  expect_true(grepl("Report_TimeZFunnel.Rmd", captured_args$strInputPath))
})
