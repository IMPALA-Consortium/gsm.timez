# Visualize Funnel Plot for a Single Month

Creates a classic funnel plot showing Metric vs Denominator with curved
confidence bounds for a single month. Sites are plotted as points
colored by their Flag value, with flagged sites labeled.

## Usage

``` r
Visualize_Funnel(dfFlagged, dfBounds, NMonth = NULL)
```

## Arguments

- dfFlagged:

  A data frame output from
  [`Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)
  applied to
  [`Analyze_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/Analyze_TimeZFunnel.md)
  output. Must contain columns: `GroupID`, `NMonth`, `Denominator`,
  `Metric`, `Flag`. Must have `vFlag` attribute (set by
  [`gsm.timez::Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)).

- dfBounds:

  A data frame output from
  [`PredictBounds_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/PredictBounds_TimeZFunnel.md).
  Must contain columns: `NMonth`, `Threshold`, `Denominator`, `Metric`.

- NMonth:

  Optional integer specifying which month to plot. If `NULL` (default),
  uses the largest month with at least the minimum fraction of sites
  active (as specified in
  [`Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)
  and
  [`PredictBounds_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/PredictBounds_TimeZFunnel.md)
  via `nMinSiteFraction`). If provided, must meet the same criterion; an
  error is raised otherwise.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`Analyze_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/Analyze_TimeZFunnel.md)
for calculating funnel scores,
[`PredictBounds_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/PredictBounds_TimeZFunnel.md)
for calculating bounds,
[`Visualize_Heatmap()`](https://impala-consortium.github.io/gsm.timez/reference/Visualize_Heatmap.md)
for the heat map visualization across all months.
