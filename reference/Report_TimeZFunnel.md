# Generate a custom KRI HTML report

Wrapper around
[`gsm.kri::Report_KRI()`](https://gilead-biostats.github.io/gsm.kri/reference/Report_KRI.html)
that uses the custom report template bundled with `gsm.timez`
(`inst/report/Report_TimeZFunnel.Rmd`).

## Usage

``` r
Report_TimeZFunnel(
  lCharts = NULL,
  dfResults = NULL,
  dfMetrics = NULL,
  dfGroups = NULL,
  strOutputDir = getwd(),
  strOutputFile = NULL,
  strInputPath = system.file("report", "Report_TimeZFunnel.Rmd", package = "gsm.timez")
)
```

## Arguments

- lCharts:

  A list of charts to include in the report.

- dfResults:

  \`data.frame\` A stacked summary of analysis pipeline output. Created
  by passing a list of results returned by \[Summarize()\] to
  \[BindResults()\]. Expected columns: \`GroupID\`, \`GroupLevel\`,
  \`Numerator\`, \`Denominator\`, \`Metric\`, \`Score\`, \`Flag\`,
  \`MetricID\`, \`StudyID\`, \`SnapshotDate\`.

- dfMetrics:

  \`data.frame\` Metric-specific metadata for use in charts and
  reporting. Created by passing an \`lWorkflow\` object to
  \[MakeMetric()\]. Expected columns: \`File\`, \`MetricID\`, \`Group\`,
  \`Abbreviation\`, \`Metric\`, \`Numerator\`, \`Denominator\`,
  \`Model\`, \`Score\`, and \`Threshold\`. For more details see the Data
  Model vignette: \`vignette("DataModel", package = "gsm.core")\`.

- dfGroups:

  \`data.frame\` Group-level metadata dictionary. Created by passing
  CTMS site and study data to \[MakeLongMeta()\]. Expected columns:
  \`GroupID\`, \`GroupLevel\`, \`Param\`, \`Value\`.

- strOutputDir:

  The output directory path for the generated report. If not provided,
  the report will be saved in the current working directory.

- strOutputFile:

  The output file name for the generated report. If not provided, the
  report will be named based on the study ID, Group Level and Date.

- strInputPath:

  \`string\` or \`fs_path\` Path to the template \`Rmd\` file.

## Value

Path to the generated HTML report (invisibly).
