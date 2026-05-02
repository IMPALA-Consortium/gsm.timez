# Generic Widget for Timeline Visualizations

Creates charts for each metric by applying a visualization function to
stacked results data. The visualization function is specified as a
string (e.g. "gsm.timez::Visualize_Heatmap") and resolved at runtime.

## Usage

``` r
Widget(
  dfResults,
  strVisualizeFun,
  strOutputLabel,
  strIcon = NULL,
  bInteractive = TRUE,
  dfBounds = NULL,
  ...
)
```

## Arguments

- dfResults:

  A stacked data frame with a `MetricID` column and all columns required
  by the visualization function.

- strVisualizeFun:

  Character string specifying the visualization function to apply,
  including namespace (e.g., "gsm.timez::Visualize_Heatmap").

- strOutputLabel:

  Character string for the chart tab label in the report.

- strIcon:

  Optional Font Awesome icon name. If provided, the icon is prepended to
  the tab label.

- bInteractive:

  Logical. If `TRUE` (default), creates fully interactive plotly charts.
  If `FALSE`, creates static plotly charts (no zoom, pan, hover) which
  render faster and produce smaller HTML output. Use `FALSE` for
  visualizations with many elements.

- dfBounds:

  Optional data frame with a `MetricID` column. When provided, it is
  filtered to the current MetricID and passed as the second positional
  argument to the visualization function, before `...`. Use this for
  functions like
  [`Visualize_Funnel()`](https://impala-consortium.github.io/gsm.timez/reference/Visualize_Funnel.md)
  that require a separate bounds data frame.

- ...:

  Additional arguments passed to the visualization function.

## Value

A named list keyed by MetricID. Each element is itself a named list
keyed by `strOutputLabel`, containing a plotly htmlwidget object.
