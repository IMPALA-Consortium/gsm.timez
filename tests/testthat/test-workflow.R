# Tests for YAML workflow execution

test_that("Timeline workflow executes successfully", {
  skip_if_not_installed("gsm.mapping")


  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_VISIT = clindata::rawplus_visdt
  )

  all_mapping_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/1_mappings", package = "gsm.mapping")
  )
  mapping_wf <- all_mapping_wf[c("AE", "SUBJ", "VISIT")]
  spec <- gsm.mapping::CombineSpecs(mapping_wf)

  expect_warning(
    lIngest <- gsm.mapping::Ingest(lRaw, spec),
    "Field `visit_dt`"
  )

  lMapped <- gsm.core::RunWorkflows(mapping_wf, lIngest)

  metrics_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/2_metrics", package = "gsm.timez")
  )

  expect_true("Timeline" %in% names(metrics_wf))

  timeline_wf <- metrics_wf["Timeline"]
  lResults <- gsm.core::RunWorkflows(timeline_wf, lMapped)


  expect_type(lResults, "list")
  expect_true("Analysis_Timeline" %in% names(lResults))


  dfTimeline <- lResults$Analysis_Timeline
  expect_s3_class(dfTimeline, "data.frame")
  expect_true(nrow(dfTimeline) > 0)
  expect_equal(
    names(dfTimeline),
    c("GroupID", "GroupLevel", "Numerator", "Denominator", "DenominatorMonth", "NMonth")
  )
})


test_that("TimeZScore workflow chains after Timeline", {
  skip_if_not_installed("gsm.mapping")

  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_VISIT = clindata::rawplus_visdt
  )

  all_mapping_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/1_mappings", package = "gsm.mapping")
  )
  mapping_wf <- all_mapping_wf[c("AE", "SUBJ", "VISIT")]
  spec <- gsm.mapping::CombineSpecs(mapping_wf)

  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lRaw, spec)
  })

  lMapped <- gsm.core::RunWorkflows(mapping_wf, lIngest)

  metrics_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/2_metrics", package = "gsm.timez")
  )

  lTimeline <- gsm.core::RunWorkflows(metrics_wf["Timeline"], lMapped)

  lData <- c(lMapped, lTimeline)

  lResults <- gsm.core::RunWorkflows(metrics_wf["TimeZScore"], lData)

  expect_type(lResults, "list")
  expect_true("Analysis_TimeZScore" %in% names(lResults))

  dfTimeZScore <- lResults$Analysis_TimeZScore
  expect_s3_class(dfTimeZScore, "data.frame")
  expect_true(nrow(dfTimeZScore) > 0)
  expect_equal(
    names(dfTimeZScore),
    c("GroupID", "GroupLevel", "Numerator", "Denominator", "DenominatorMonth", "NMonth", "Metric", "Score")
  )
})


test_that("workflow produces same results as direct function call", {
  skip_if_not_installed("gsm.mapping")

  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_VISIT = clindata::rawplus_visdt
  )

  all_mapping_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/1_mappings", package = "gsm.mapping")
  )
  mapping_wf <- all_mapping_wf[c("AE", "SUBJ", "VISIT")]
  spec <- gsm.mapping::CombineSpecs(mapping_wf)

  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lRaw, spec)
  })

  lMapped <- gsm.core::RunWorkflows(mapping_wf, lIngest)

  metrics_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/2_metrics", package = "gsm.timez")
  )
  lTimeline <- gsm.core::RunWorkflows(metrics_wf["Timeline"], lMapped)
  dfWorkflow <- lTimeline$Analysis_Timeline

  dfDirect <- Timeline(
    dfSubjects = lMapped$Mapped_SUBJ,
    dfNumerator = lMapped$Mapped_AE,
    dfDenominator = lMapped$Mapped_VISIT,
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  expect_equal(nrow(dfWorkflow), nrow(dfDirect))
  expect_equal(names(dfWorkflow), names(dfDirect))
  expect_equal(dfWorkflow$GroupID, dfDirect$GroupID)
  expect_equal(dfWorkflow$Numerator, dfDirect$Numerator)
  expect_equal(dfWorkflow$Denominator, dfDirect$Denominator)
})


test_that("Flag workflow chains after TimeZScore", {
  skip_if_not_installed("gsm.mapping")

  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_VISIT = clindata::rawplus_visdt
  )

  all_mapping_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/1_mappings", package = "gsm.mapping")
  )
  mapping_wf <- all_mapping_wf[c("AE", "SUBJ", "VISIT")]
  spec <- gsm.mapping::CombineSpecs(mapping_wf)

  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lRaw, spec)
  })

  lMapped <- gsm.core::RunWorkflows(mapping_wf, lIngest)

  metrics_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/2_metrics", package = "gsm.timez")
  )

  expect_true("Flag" %in% names(metrics_wf))

  lTimeline <- gsm.core::RunWorkflows(metrics_wf["Timeline"], lMapped)
  lData <- c(lMapped, lTimeline)
  lTimeZScore <- gsm.core::RunWorkflows(metrics_wf["TimeZScore"], lData)
  lData <- c(lData, lTimeZScore)
  lResults <- gsm.core::RunWorkflows(metrics_wf["Flag"], lData)

  expect_type(lResults, "list")
  expect_true("Analysis_Flag" %in% names(lResults))

  dfFlagged <- lResults$Analysis_Flag
  expect_s3_class(dfFlagged, "data.frame")
  expect_true(nrow(dfFlagged) > 0)
  expect_true("Flag" %in% names(dfFlagged))
  expect_true(all(dfFlagged$Flag %in% c(-2, -1, 0, 1, 2)))
})


test_that("Visualize workflow creates ggplot from flagged data", {
  skip_if_not_installed("gsm.mapping")

  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_VISIT = clindata::rawplus_visdt
  )

  all_mapping_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/1_mappings", package = "gsm.mapping")
  )
  mapping_wf <- all_mapping_wf[c("AE", "SUBJ", "VISIT")]
  spec <- gsm.mapping::CombineSpecs(mapping_wf)

  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lRaw, spec)
  })

  lMapped <- gsm.core::RunWorkflows(mapping_wf, lIngest)

  metrics_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/2_metrics", package = "gsm.timez")
  )

  lTimeline <- gsm.core::RunWorkflows(metrics_wf["Timeline"], lMapped)
  lData <- c(lMapped, lTimeline)
  lTimeZScore <- gsm.core::RunWorkflows(metrics_wf["TimeZScore"], lData)
  lData <- c(lData, lTimeZScore)
  lFlagged <- gsm.core::RunWorkflows(metrics_wf["Flag"], lData)
  lData <- c(lData, lFlagged)

  viz_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/3_visualization", package = "gsm.timez")
  )

  expect_true("Visualize" %in% names(viz_wf))

  lResults <- gsm.core::RunWorkflows(viz_wf["Visualize"], lData)

  expect_type(lResults, "list")
  expect_true("Visualization_Visualize" %in% names(lResults))

  chart <- lResults$Visualization_Visualize
  expect_s3_class(chart, "ggplot")
})
