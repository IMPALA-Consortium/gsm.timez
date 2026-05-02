# Get Sparse Months

Get Sparse Months

## Usage

``` r
GetSparseMonths(df, nMinSiteFraction)
```

## Arguments

- df:

  A data frame containing at minimum columns `NMonth` (month index) and
  `GroupID` (site identifier).

- nMinSiteFraction:

  Minimum fraction of peak site count; months below this fraction are
  marked sparse.

## Value

A data frame with columns `NMonth` and `sparse` (logical).
