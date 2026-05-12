# Calculate Funnel Plot Scores for Timeline Data

Applies
[`gsm.core::Analyze_NormalApprox()`](https://gilead-biostats.github.io/gsm.core/reference/Analyze_NormalApprox.html)
to each month of `dfTimeline`, producing a funnel plot score for each
site-month combination.

## Usage

``` r
Analyze_TimeZFunnel(dfTimeline)
```

## Arguments

- dfTimeline:

  A data frame output from
  [`Timeline()`](https://impala-consortium.github.io/gsm.timez/reference/Timeline.md).
  Must contain columns: `GroupID`, `GroupLevel`, `Numerator`,
  `Denominator`, and `NMonth`.

## Value

All columns from
[`Timeline()`](https://impala-consortium.github.io/gsm.timez/reference/Timeline.md),
plus the following additional columns:

- `Metric`: The ratio of Numerator to Denominator (Numerator /
  Denominator). Computed before calling
  [`gsm.core::Analyze_NormalApprox`](https://gilead-biostats.github.io/gsm.core/reference/Analyze_NormalApprox.html).

- `OverallMetric`, `Factor`, `Score`: See
  [`gsm.core::Analyze_NormalApprox()`](https://gilead-biostats.github.io/gsm.core/reference/Analyze_NormalApprox.html)
  for definitions.

## See also

[`gsm.core::Analyze_NormalApprox()`](https://gilead-biostats.github.io/gsm.core/reference/Analyze_NormalApprox.html),
[`Timeline()`](https://impala-consortium.github.io/gsm.timez/reference/Timeline.md)
