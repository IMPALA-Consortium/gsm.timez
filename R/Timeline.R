#' Generate Site-Level Cumulative Event Timeline
#'
#' This function generates a site-level (group-level) timeline by joining
#' numerator events (e.g., adverse events, required reports) to the corresponding
#' denominator periods (e.g., visits, time points). It calculates cumulative
#' metrics aggregated at the site/group level by month.
#'
#' @param dfSubjects A data frame containing unique subject IDs and grouping
#'   variables.
#' @param dfNumerator A data frame containing event records (the numerator).
#'   This must contain a subject ID column and a date column.
#' @param dfDenominator A data frame containing time point records (the
#'   denominator, e.g., visits). This must contain a subject ID column and a
#'   date column.
#' @param strGroupCol The name of the column in \code{dfSubjects} used for
#'   grouping subjects (e.g., "invid", "country").
#' @param strGroupLevel Optional character string used as a label for the
#'   grouping level, stored in the \code{GroupLevel} output column. If
#'   \code{NULL}, defaults to the value of \code{strGroupCol}.
#' @param strSubjectCol The name of the unique subject ID column in
#'   \code{dfSubjects}.
#' @param strNumeratorCol The name of the subject ID column in
#'   \code{dfNumerator}. Defaults to the value of \code{strSubjectCol}.
#' @param strDenominatorCol The name of the subject ID column in
#'   \code{dfDenominator}. Defaults to the value of \code{strSubjectCol}.
#' @param strNumeratorDateCol The name of the date column in \code{dfNumerator}
#'   (must be of class "Date", "POSIXct", or "POSIXlt").
#' @param strDenominatorDateCol The name of the date column in
#'   \code{dfDenominator} (must be of class "Date", "POSIXct", or "POSIXlt").
#'
#' @return A tibble with site-level monthly aggregations containing:
#'   \describe{
#'     \item{GroupID}{The site/group identifier from \code{strGroupCol}.}
#'     \item{GroupLevel}{The grouping level name.}
#'     \item{Numerator}{Total cumulative numerator events (e.g., AEs) across
#'       all subjects at the site up to that month.}
#'     \item{Denominator}{Total cumulative visits across all subjects at the
#'       site up to that month.}
#'     \item{DenominatorMonth}{The month (Date, truncated to first of month).}
#'     \item{NMonth}{Sequential month number for the group (1, 2, 3, ...).}
#'   }
#'
#' @export
Timeline <- function(
    dfSubjects,
    dfNumerator,
    dfDenominator,
    strGroupCol,
    strGroupLevel = NULL,
    strSubjectCol,
    strNumeratorCol = NULL,
    strDenominatorCol = NULL,
    strNumeratorDateCol,
    strDenominatorDateCol) {
  strGroupLevel <- if (is.null(strGroupLevel)) strGroupCol else strGroupLevel

  # dfSubjects
  #  - strSubjectCol
  #  - strGroupCol
  #  - strGroupLevel = NULL

  dfSubjectsCols <- c(strSubjectCol, strGroupCol)
  # checking all columns exist
  stopifnot(all(dfSubjectsCols %in% names(dfSubjects)))

  subjects <-
    dfSubjects %>%
    select(all_of(dfSubjectsCols)) %>%
    rename(
      subject = {{ strSubjectCol }},
      group = {{ strGroupCol }}
    ) %>%
    filter(!is.na(.data$subject)) %>%
    mutate(group_level = strGroupLevel)


  # dfDenominator
  #  - strDenominatorCol = NULL
  #  - strDenominatorDateCol

  strDenominatorCol <- if (is.null(strDenominatorCol)) strSubjectCol else strDenominatorCol

  dfDenominatorCols <- c(strDenominatorCol, strDenominatorDateCol)
  # checkin all columns exist
  stopifnot(all(dfDenominatorCols %in% names(dfDenominator)))
  # checking date column is of type Date or similar (timestamp)
  stopifnot(inherits(
    dfDenominator[[strDenominatorDateCol]],
    c("Date", "POSIXct", "POSIXlt")
  ))

  denominator <-
    dfDenominator %>%
    select(all_of(dfDenominatorCols)) %>%
    rename(
      subject = {{ strDenominatorCol }},
      date = {{ strDenominatorDateCol }}
    ) %>%
    filter(.data$subject %in% subjects$subject &
      !is.na(.data$date)) %>% # keeping only subjects in dfSubjects and with non-NA dates
    inner_join(subjects, by = "subject") # adding group


  # dfNumerator
  #  - strNumeratorCol = NULL
  #  - strNumeratorDateCol

  strNumeratorCol <- if (is.null(strNumeratorCol)) strSubjectCol else strNumeratorCol

  dfNumeratorCols <- c(strNumeratorCol, strNumeratorDateCol)
  # checkin all columns exist
  stopifnot(all(dfNumeratorCols %in% names(dfNumerator)))
  # checking date column is of type Date or similar (timestamp)
  stopifnot(inherits(
    dfNumerator[[strNumeratorDateCol]],
    c("Date", "POSIXct", "POSIXlt")
  ))

  numerator <-
    dfNumerator %>%
    select(all_of(dfNumeratorCols)) %>%
    rename(
      subject = {{ strNumeratorCol }},
      date = {{ strNumeratorDateCol }}
    ) %>%
    filter(.data$subject %in% denominator$subject &
      !is.na(.data$date)) %>% # keeping only subjects in denominator and with non-NA dates
    inner_join(subjects, by = "subject") # adding group


  # Building timeline

  prep_denominator <-
    denominator %>%
    group_by(.data$group, .data$subject) %>%
    arrange(.data$date, .by_group = TRUE) %>%
    # previous_date is not needed, just adding it for sanity checks
    # later on.
    mutate(previous_denominator_date = lag(.data$date)) %>%
    mutate(
      type = 1, # Add a type flag: 1 for visit start
      .id = row_number() # row number in the denominator table
    ) %>%
    ungroup()

  prep_numerator <-
    numerator %>%
    mutate(
      type = 0, # add a type flag: 0 for the actual event
      .id = NA_integer_ # placeholder to match .id column above
    )

  union_numerator_denominator <- bind_rows(prep_numerator, prep_denominator)

  # Assigning groups

  assign_group_to_numerator_denominator <-
    union_numerator_denominator %>%
    arrange(
      .data$group_level,
      .data$group,
      .data$subject,
      .data$date,
      .data$type
    ) %>%
    group_by(.data$group_level, .data$group, .data$subject) %>%
    # Refers to the denominator group both the denominator and the numerator
    # belong to
    mutate(denominator = cumsum(.data$type)) %>%
    ungroup()

  # numerator with their respective denominator group
  numerator_group <-
    assign_group_to_numerator_denominator %>%
    filter(.data$type == 0) %>%
    # The numerator matches the denominator group *after* the denominator
    # date, so we need the denominator group *before* the one assigned.
    # In other words - numerators that happened after the previous
    # denominator up until the current denominator, belong to the current
    # denominator.
    mutate(denominator = .data$denominator + 1) %>% # Adjust the denominator group
    # index to match the
    # denominator group it
    # belongs to
    select(-all_of(c(".id", "type", "previous_denominator_date"))) %>%
    rename(numerator_date = "date")


  # denominator with their respective group
  denominator_group <-
    assign_group_to_numerator_denominator %>%
    filter(.data$type == 1) %>%
    select(-all_of(c(".id", "type"))) %>%
    rename(denominator_date = "date")

  # joining denominator_group with numerator_group
  numerator_denominator_joined <-
    denominator_group %>%
    left_join(numerator_group,
      by = c("subject", "group", "group_level", "denominator")
    )


  # check numerator falls into the right denominator interval
  numerator_denominator_joined <-
    numerator_denominator_joined %>%
    mutate(check_numerator = ifelse(is.na(.data$numerator_date), TRUE,
      ifelse(is.na(.data$previous_denominator_date),
        # If previous_denominator_date is
        # NA, check only if
        # numerator_date <= denominator_date
        .data$numerator_date <= .data$denominator_date,
        # If previous_denominator_date is
        # NOT NA, check the both bounds of the
        # interval:
        .data$numerator_date > .data$previous_denominator_date &
          .data$numerator_date <= .data$denominator_date
      )
    ))

  stopifnot(all(numerator_denominator_joined$check_numerator))
  numerator_denominator_joined <-
    numerator_denominator_joined %>%
    select(-c("check_numerator", "previous_denominator_date"))


  timeline <-
    numerator_denominator_joined %>%
    arrange(
      .data$subject,
      .data$group,
      .data$group_level,
      .data$denominator_date,
      .data$denominator
    ) %>%
    group_by(
      .data$subject,
      .data$group,
      .data$group_level,
      .data$denominator_date,
      .data$denominator
    ) %>%
    summarise(
      events = sum(!is.na(.data$numerator_date)),
      .groups = "drop"
    ) %>%
    group_by(.data$subject, .data$group, .data$group_level) %>%
    mutate(numerator = cumsum(.data$events)) %>%
    ungroup()

  ##
  ## strDomain: original
  ## Previously returned here for strDomain == "original"
  ##

  timeline_month_gap <-
    timeline %>%
    group_by(.data$subject,
      .data$group,
      .data$group_level,
      denominator_month = trunc(.data$denominator_date, "months")
    ) %>%
    summarise(
      denominator = max(.data$denominator),
      visits = n(),
      numerator = max(.data$numerator),
      events = sum(.data$events),
      .groups = "drop"
    ) %>%
    ungroup()

  ##
  ## strDomain: month_gap
  ## Previously returned here for strDomain == "month_gap"
  ##


  timeline_month <-
    timeline_month_gap %>%
    group_by(.data$subject, .data$group, .data$group_level) %>%
    tidyr::complete(denominator_month = seq(min(.data$denominator_month),
      max(.data$denominator_month),
      by = "months"
    )) %>%
    tidyr::fill(denominator, numerator, .direction = "down") %>%
    mutate(
      events = tidyr::replace_na(.data$events, 0),
      visits = tidyr::replace_na(.data$visits, 0)
    ) %>%
    ungroup()

  ##
  ## strDomain: month
  ## Previously returned here for strDomain == "month"
  ##

  # Site-level aggregation

  # Step 1: Get first month for each subject (for enrollment curve)
  first_month_per_subject <-
    timeline_month %>%
    group_by(.data$group, .data$group_level, .data$subject) %>%
    summarise(first_month = min(.data$denominator_month), .groups = "drop")

  # Step 2: Aggregate at site-month level
  timeline_site <-
    timeline_month %>%
    group_by(.data$group, .data$group_level, .data$denominator_month) %>%
    summarise(
      visits = sum(.data$visits),
      events = sum(.data$events),
      num_patients = n_distinct(.data$subject),
      .groups = "drop"
    ) %>%
    group_by(.data$group, .data$group_level) %>%
    arrange(.data$denominator_month) %>%
    mutate(
      n_month = row_number(),
      denominator = cumsum(.data$visits), # cumulative visits
      numerator = cumsum(.data$events) # cumulative AEs
    ) %>%
    ungroup()

  # Step 3: Calculate cum_num_patients (enrollment curve)
  new_enrollments <-
    first_month_per_subject %>%
    group_by(.data$group, .data$group_level, .data$first_month) %>%
    summarise(new_patients = n(), .groups = "drop")

  timeline_site <-
    timeline_site %>%
    left_join(new_enrollments,
      by = c("group", "group_level", "denominator_month" = "first_month")
    ) %>%
    mutate(new_patients = tidyr::replace_na(.data$new_patients, 0)) %>%
    group_by(.data$group, .data$group_level) %>%
    arrange(.data$denominator_month) %>%
    mutate(cum_num_patients = cumsum(.data$new_patients)) %>%
    select(-"new_patients") %>%
    ungroup() %>%
    select(
      "group", "group_level", "numerator", "denominator",
      "denominator_month", "n_month"
    ) %>%
    # excluding num_patients and cum_num_patients from output
    # can be added if needed in the future.
    #  "denominator_month", "n_month", "num_patients", "cum_num_patients") %>%
    arrange(.data$group, .data$denominator) %>%
    # Rename to GSM standard PascalCase column names
    rename(
      GroupID = "group",
      GroupLevel = "group_level",
      Numerator = "numerator",
      Denominator = "denominator",
      DenominatorMonth = "denominator_month",
      NMonth = "n_month"
    )

  return(timeline_site)
}
