# Merge Multiple Chart Lists

Combines chart lists by merging charts within each MetricID. Used to
combine default charts from
[`gsm.kri::MakeCharts()`](https://gilead-biostats.github.io/gsm.kri/reference/MakeCharts.html)
with custom charts from
[`Widget()`](https://impala-consortium.github.io/gsm.timez/reference/Widget.md).

## Usage

``` r
Merge_ChartLists(...)
```

## Arguments

- ...:

  Two or more chart lists to merge. Each should be a list keyed by
  MetricID (the workflow `meta.ID` string), where each element is a list
  of charts. Arguments do not need to be named.

## Value

A list keyed by MetricID containing all charts from all input lists for
each metric. Metrics present in only some inputs are included unchanged.
