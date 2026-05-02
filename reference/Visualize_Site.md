# Visualize Single Site Trajectory with Flag Indicators

Creates a line plot highlighting a single site's cumulative metric
trajectory against all other sites, with colored dots marking flagged
months.

## Usage

``` r
Visualize_Site(dfFlagged, dfBounds = NULL, strSiteID)
```

## Arguments

- dfFlagged:

  A data frame output from
  [`Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)
  containing columns: `GroupID`, `NMonth`, `Metric`, `Flag`, and (when
  `dfBounds` is provided) `Denominator`. Must carry `vFlag` and
  `vThreshold` attributes (set by
  [`gsm.timez::Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)).

- dfBounds:

  Optional. A data frame with pre-calculated funnel bounds from
  [`PredictBounds_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/PredictBounds_TimeZFunnel.md).
  If provided, inner threshold lines are drawn for the selected site
  based on its denominator at each month. If `NULL` (default), no bounds
  are shown.

- strSiteID:

  Character string specifying the site ID to highlight. Must exist in
  `dfFlagged$GroupID`.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)
for assigning flag categories,
[`PredictBounds_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/PredictBounds_TimeZFunnel.md)
for calculating bounds,
[`Visualize_Heatmap()`](https://impala-consortium.github.io/gsm.timez/reference/Visualize_Heatmap.md)
for the heat map visualization.
