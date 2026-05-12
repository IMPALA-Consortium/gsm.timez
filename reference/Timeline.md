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
  (e.g., "invid", "country").

- strGroupLevel:

  Optional character string used as a label for the grouping level,
  stored in the `GroupLevel` output column. If `NULL`, defaults to the
  value of `strGroupCol`.

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
