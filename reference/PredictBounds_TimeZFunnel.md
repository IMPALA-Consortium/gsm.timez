# Calculate Predicted Bounds for Analyze_TimeZFunnel Visualization

Applies
[`gsm.core::Analyze_NormalApprox_PredictBounds()`](https://gilead-biostats.github.io/gsm.core/reference/Analyze_NormalApprox_PredictBounds.html)
to each month of `dfAnalyzed`, producing funnel plot bounds for each
month.

## Usage

``` r
PredictBounds_TimeZFunnel(
  dfAnalyzed,
  vThreshold = c(-3, -2, 2, 3),
  nMinSiteFraction = 0.2
)
```

## Arguments

- dfAnalyzed:

  A data frame output from
  [`Analyze_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/Analyze_TimeZFunnel.md).
  Must contain columns: `NMonth`, `GroupID`, `GroupLevel`, `Numerator`,
  `Denominator`, and `Metric`.

- vThreshold:

  Numeric vector of threshold values for bounds. Default is
  `c(-3, -2, 2, 3)`. A threshold of 0 (for the mean line) is
  automatically included by
  [`gsm.core::Analyze_NormalApprox_PredictBounds`](https://gilead-biostats.github.io/gsm.core/reference/Analyze_NormalApprox_PredictBounds.html).

- nMinSiteFraction:

  Minimum fraction of peak site count required to return non-`NA` bounds
  for a month. Months where the number of distinct `GroupID`s is below
  this fraction of the maximum are returned with `Metric = NA`. Default
  is `0.2` (20%).

## Value

A data frame with columns:

- `NMonth`: Time point (month number).

- `Threshold`: Threshold value (e.g., -3, -2, 0, 2, 3).

- `Denominator`: Sample size / exposure value.

- `LogDenominator`: Log of Denominator.

- `Numerator`: Predicted numerator at this threshold.

- `Metric`: Predicted Metric value at this threshold.

Multiple rows are returned per month per threshold, covering the range
of Denominator values observed in the data.

## See also

[`Analyze_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/Analyze_TimeZFunnel.md)
for calculating funnel scores,
[`gsm.core::Analyze_NormalApprox_PredictBounds`](https://gilead-biostats.github.io/gsm.core/reference/Analyze_NormalApprox_PredictBounds.html)
for the underlying bounds calculation.
