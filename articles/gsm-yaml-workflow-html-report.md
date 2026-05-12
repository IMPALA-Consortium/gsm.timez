# GSM YAML Workflow: HTML Report

This vignette walks through the end-to-end GSM YAML workflow to generate
an HTML report for the timeline KRI using the funnel-based z-score
method. It follows the standard four-phase GSM pipeline: mapping raw
data into a standardised layer, running the metric analysis, assembling
the reporting data, and rendering the HTML output. The result is the
[Sample
Report](https://impala-consortium.github.io/gsm.timez/articles/Report_TimeZFunnel.md)
linked in the navbar.

## Prepare Source Data

``` r

lSource <- list(
  Source_STUDY = clindata::ctms_study,
  Source_SITE = clindata::ctms_site,
  Source_SUBJ = clindata::rawplus_dm,
  Source_AE = clindata::rawplus_ae %>% dplyr::select(-aetoxgr),
  # gsm.mapping expects a studyid column, but clindata::edc_data_pages uses
  # protocolname instead. DATAENT itself is not used by gsm.timez but is
  # required by gsm.mapping's AE workflow.
  Source_DATAENT = clindata::edc_data_pages %>%
    dplyr::mutate(studyid = protocolname),
  Source_VISIT = clindata::rawplus_visdt
)
```

## Step 1 — Map Raw Data

Use `Ingest()` to standardise column names based on the workflow specs,
then run the mapping workflows to produce the `Mapped_*` data layer.

``` r

mapping_wf <- gsm.core::MakeWorkflowList(
  strPath = system.file("workflow/1_mappings", package = "gsm.mapping"),
  strNames = c("SUBJ", "AE", "VISIT", "SITE", "STUDY", "COUNTRY")
)
mapping_spec <- gsm.mapping::CombineSpecs(mapping_wf)
lRaw <- gsm.mapping::Ingest(lSource, mapping_spec)
#> Warning: Field `visit_dt`: 19 unparsable Date(s) set to NA
lMapped <- gsm.core::RunWorkflows(mapping_wf, lRaw)
#> Warning: Not all specified columns in the spec are present in the data, missing columns
#> are: Raw_AE$aetoxgr
#> Warning: Not all specified columns in the spec are present in the data, missing columns
#> are: Raw_STUDY$db_lock_dt
```

``` r

names(lMapped)
#> [1] "Mapped_AE"      "Mapped_SUBJ"    "Mapped_VISIT"   "Mapped_DATAENT"
#> [5] "Mapped_COUNTRY" "Mapped_SITE"    "Mapped_STUDY"
```

## Step 2 — Run Analysis

Run the `kri_TimeZFunnel` metric workflow to produce the analysis
results.

``` r

metrics_wf <- gsm.core::MakeWorkflowList(
  strPath = system.file("workflow/2_metrics", package = "gsm.timez"),
  strNames = "kri_TimeZFunnel",
  bExact = TRUE
)
lAnalyzed <- gsm.core::RunWorkflows(metrics_wf, lMapped)
```

``` r

names(lAnalyzed)
#> [1] "Analysis_kri_TimeZFunnel"
```

## Step 3 — Prepare Reporting Data

The `3_reporting` workflows stack and reshape the analysis results into
the flat data frames expected by the report template.

``` r

reporting_wf <- gsm.core::MakeWorkflowList(
  strPath = system.file("workflow/3_reporting", package = "gsm.timez")
)
lReporting <- gsm.core::RunWorkflows(
  reporting_wf,
  c(lMapped, list(lAnalyzed = lAnalyzed, lWorkflows = metrics_wf))
)
```

``` r

names(lReporting)
#> [1] "Reporting_Bounds"            "Reporting_Results_allmonths"
#> [3] "Reporting_Groups"            "Reporting_Metrics"          
#> [5] "Reporting_Results"
```

## Step 4 — Generate HTML Report

The `4_modules` workflow creates all charts and renders the HTML report
using `gsm.timez`’s custom template
(`inst/report/Report_TimeZFunnel.Rmd`).

``` r

module_wf <- gsm.core::MakeWorkflowList(
  strPath = system.file("workflow/4_modules", package = "gsm.timez"),
  strNames = "Report_TimeZFunnel",
  bExact = TRUE
)
lReport <- gsm.core::RunWorkflows(module_wf, lReporting)
```

``` r

lReport$Module_Report_TimeZFunnel
#> [1] "/home/runner/work/gsm.timez/gsm.timez/vignettes/kri_report_AAAA0000000_Site_20260512.html"
```

The workflow generates `Report_TimeZFunnel.html`. See the [Sample
Report](https://impala-consortium.github.io/gsm.timez/articles/Report_TimeZFunnel.md)
for an example output.
