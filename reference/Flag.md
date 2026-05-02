# Flag

Wrapper around
[`gsm.core::Flag()`](https://gilead-biostats.github.io/gsm.core/reference/Flag.html).
Stores `vThreshold` and `vFlag` as attributes for use by downstream
visualization functions.

Months where the number of distinct sites falls below `nMinSiteFraction`
of the peak site count are considered statistically unreliable and have
their `Flag` set to `NA`, consistent with the `NA` bounds returned by
[`PredictBounds_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/PredictBounds_TimeZFunnel.md)
for the same months.

## Usage

``` r
Flag(
  dfAnalyzed,
  vThreshold = c(-3, -2, 2, 3),
  vFlag = c(-2, -1, 0, 1, 2),
  nMinSiteFraction = 0.2,
  ...
)
```

## Arguments

- dfAnalyzed:

  `data.frame` where flags should be added. Must contain columns
  `NMonth` and `GroupID` for sparse-month detection.

- vThreshold:

  `numeric` Vector of threshold values in ascending order. Default:
  `c(-3, -2, 2, 3)`.

- vFlag:

  `numeric` Vector of flag values. Must have length equal to
  `length(vThreshold) + 1`. Default: `c(-2, -1, 0, 1, 2)`.

- nMinSiteFraction:

  Minimum fraction of peak site count required to return a non-`NA` flag
  for a month. Months where the number of distinct `GroupID`s is below
  this fraction of the maximum are returned with `Flag = NA`. Default is
  `0.2` (20\\ Set to `0` to disable sparse-month masking.

- ...:

  Additional arguments passed to
  [`gsm.core::Flag()`](https://gilead-biostats.github.io/gsm.core/reference/Flag.html).

## Value

All columns from `dfAnalyzed`, plus the following additions:

- `Flag`: Integer from `vFlag` indicating how far `Score` falls from
  centre: `0` within bounds, negative below average, positive above
  average. `NA` for sparse months (see `nMinSiteFraction`).

Also carries attributes `vThreshold` and `vFlag` for downstream use.

## See also

[`gsm.core::Flag()`](https://gilead-biostats.github.io/gsm.core/reference/Flag.html),
[`PredictBounds_TimeZFunnel()`](https://impala-consortium.github.io/gsm.timez/reference/PredictBounds_TimeZFunnel.md)
