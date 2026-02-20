# Calculate Cumulative Z-Scores for Timeline Data

This function takes the output from [`Timeline`](timeline.md) and
calculates cumulative z-scores for each row using an expanding window
indexed by month. The z-score compares each site's ratio against the
cumulative study-wide distribution of ratios up to that month.

## Usage

``` r
TimeZScore(dfTimeline)
```

## Arguments

- dfTimeline:

  A data frame output from [`Timeline`](timeline.md). Must contain
  columns: `GroupID`, `GroupLevel`, `Numerator`, `Denominator`, and
  `NMonth`.

## Value

The input data frame with two additional columns:

- `Metric`: The ratio of Numerator to Denominator (Numerator /
  Denominator).

- `Score`: The z-score calculated using an expanding window. For month
  N, the z-score is calculated using all ratios from months 1 through N
  across all groups. If fewer than 2 ratios exist in the cumulative
  window, Score is 0.

## Details

The z-score is calculated as: \$\$z = \frac{Metric -
mean(Metrics)}{sd(Metrics)}\$\$

Where `Metrics` includes all Metric values from all groups where
`NMonth <= current_NMonth`.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
dfSubjects <- data.frame(
  SubjectID = c(1, 2, 3, 4),
  SiteID = c("A", "A", "B", "B")
)
dfNumerator <- data.frame(
  SubjectID = c(1, 1, 2, 3, 4, 4, 4),
  EventDate = as.Date(c(
    "2022-01-01", "2022-01-15", "2022-02-01",
    "2022-01-10", "2022-01-05", "2022-01-20", "2022-02-01"
  ))
)
dfDenominator <- data.frame(
  SubjectID = c(1, 1, 2, 2, 3, 3, 4, 4),
  VisitDate = as.Date(c(
    "2022-01-01", "2022-01-20", "2022-01-01", "2022-02-01",
    "2022-01-01", "2022-01-15", "2022-01-01", "2022-02-01"
  ))
)

dfTimeline <- Timeline(
  dfSubjects = dfSubjects,
  dfNumerator = dfNumerator,
  dfDenominator = dfDenominator,
  strGroupCol = "SiteID",
  strSubjectCol = "SubjectID",
  strNumeratorDateCol = "EventDate",
  strDenominatorDateCol = "VisitDate"
)

TimeZScore(dfTimeline)
#> # A tibble: 4 × 8
#>   GroupID GroupLevel Numerator Denominator DenominatorMonth NMonth Metric  Score
#>   <chr>   <chr>          <int>       <int> <date>            <int>  <dbl>  <dbl>
#> 1 A       SiteID             2           3 2022-01-01            1  0.667  0.707
#> 2 A       SiteID             3           4 2022-02-01            2  0.75   0.227
#> 3 B       SiteID             1           3 2022-01-01            1  0.333 -0.707
#> 4 B       SiteID             4           4 2022-02-01            2  1      1.13 
```
