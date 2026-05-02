# Visualize Flagged Months by Site

Creates a heat map of site flags over time.

## Usage

``` r
Visualize_Heatmap(dfFlagged, nSites = NULL)
```

## Arguments

- dfFlagged:

  A data frame output from
  [`Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)
  applied to
  [`Analyze_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/Analyze_TimeZFunnel.md)
  output. Must contain columns: `GroupID`, `NMonth`, and `Flag`. Must
  also carry `vFlag` attribute (set by
  [`Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)).

- nSites:

  Integer or `NULL`. Maximum number of sites to display. Sites are
  ranked by: (1) whether they were flagged in the most recent evaluated
  month, (2) total number of flagged months, (3) `GroupID`
  alphabetically. When `NULL` (the default) all sites are shown and no
  title is added to the plot.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`Analyze_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/Analyze_TimeZFunnel.md)
for calculating funnel scores,
[`Flag()`](https://impala-consortium.github.io/gsm.timez/reference/Flag.md)
for assigning flag categories.
