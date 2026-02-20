# Generate Site-Level Cumulative Event Timeline

This function generates a site-level (group-level) timeline by joining
numerator events (e.g., adverse events, required reports) to the
corresponding denominator periods (e.g., visits, time points). It
calculates cumulative metrics aggregated at the site/group level by
month.

## Usage

``` r
Timeline(
  dfSubjects,
  dfNumerator,
  dfDenominator,
  strGroupCol,
  strGroupLevel = NULL,
  strSubjectCol,
  strNumeratorCol = NULL,
  strDenominatorCol = NULL,
  strNumeratorDateCol,
  strDenominatorDateCol
)
```

## Arguments

- dfSubjects:

  A data frame containing unique subject IDs and grouping variables.

- dfNumerator:

  A data frame containing event records (the numerator). This must
  contain a subject ID column and a date column.

- dfDenominator:

  A data frame containing time point records (the denominator, e.g.,
  visits). This must contain a subject ID column and a date column.

- strGroupCol:

  The name of the column in `dfSubjects` used for grouping subjects
  (e.g., "SiteID", "Treatment").

- strGroupLevel:

  Optional character string specifying a subset level within
  `strGroupCol` to be analyzed. If `NULL`, the full column is used.

- strSubjectCol:

  The name of the unique subject ID column in `dfSubjects`.

- strNumeratorCol:

  The name of the subject ID column in `dfNumerator`. Defaults to the
  value of `strSubjectCol`.

- strDenominatorCol:

  The name of the subject ID column in `dfDenominator`. Defaults to the
  value of `strSubjectCol`.

- strNumeratorDateCol:

  The name of the date column in `dfNumerator` (must be of class "Date",
  "POSIXct", or "POSIXlt").

- strDenominatorDateCol:

  The name of the date column in `dfDenominator` (must be of class
  "Date", "POSIXct", or "POSIXlt").

## Value

A tibble with site-level monthly aggregations containing:

- GroupID:

  The site/group identifier from `strGroupCol`.

- GroupLevel:

  The grouping level name.

- Numerator:

  Total cumulative numerator events (e.g., AEs) across all subjects at
  the site up to that month.

- Denominator:

  Total cumulative visits across all subjects at the site up to that
  month.

- DenominatorMonth:

  The month (Date, truncated to first of month).

- NMonth:

  Sequential month number for the group (1, 2, 3, ...).

## Examples

``` r
library(dplyr)

# Example data
dfSubjects <- data.frame(
  SubjectID = c(1, 2, 3),
  SiteID = c("A", "A", "B")
)
dfNumerator <- data.frame(
  SubjectID = c(1, 1, 2, 3),
  EventDate = as.Date(c("2022-01-01", "2022-01-15", "2022-02-01", "2022-02-15"))
)
dfDenominator <- data.frame(
  SubjectID = c(1, 1, 2, 2, 3),
  VisitDate = as.Date(c("2022-01-01", "2022-01-20", "2022-01-15", "2022-02-01", "2022-02-01"))
)

# Generate site-level timeline
Timeline(
  dfSubjects = dfSubjects,
  dfNumerator = dfNumerator,
  dfDenominator = dfDenominator,
  strGroupCol = "SiteID",
  strSubjectCol = "SubjectID",
  strNumeratorDateCol = "EventDate",
  strDenominatorDateCol = "VisitDate"
)
#> # A tibble: 3 × 6
#>   GroupID GroupLevel Numerator Denominator DenominatorMonth NMonth
#>   <chr>   <chr>          <int>       <int> <date>            <int>
#> 1 A       SiteID             2           3 2022-01-01            1
#> 2 A       SiteID             3           4 2022-02-01            2
#> 3 B       SiteID             0           1 2022-02-01            1
```
