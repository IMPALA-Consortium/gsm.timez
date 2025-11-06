#' Generate Cumulative Event Timeline
#'
#' This function generates a subject-level timeline by joining numerator events
#' (e.g., adverse events, required reports) to the corresponding denominator periods
#' (e.g., visits, time points). It calculates the cumulative number of numerator
#' events that occurred up to each denominator date for each subject.
#'
#' The function relies on tidy-evaluation principles to handle column names passed
#' as strings and supports three different aggregation domains for the output.
#'
#' @param dfSubjects A data frame containing unique subject IDs and grouping 
#'                   variables.
#' @param dfNumerator A data frame containing event records (the numerator).
#'                    This must contain a subject ID column and a date column.
#' @param dfDenominator A data frame containing time point records (the
#'                      denominator, e.g., visits). This must contain a subject
#'                      ID column and a date column.
#' @param strGroupCol The name of the column in \code{dfSubjects} used for 
#'                    grouping subjects (e.g., "SiteID", "Treatment").
#' @param strGroupLevel Optional character string specifying a subset level
#'                      within \code{strGroupCol} to be analyzed. If
#'                      \code{NULL}, the full column is used.
#' @param strSubjectCol The name of the unique subject ID column in
#'                      \code{dfSubjects}.
#' @param strNumeratorCol The name of the subject ID column in 
#'                        \code{dfNumerator}. Defaults to the value of
#'                        \code{strSubjectCol}.
#' @param strDenominatorCol The name of the subject ID column in
#'                          \code{dfDenominator}. Defaults to the value of
#'                          \code{strSubjectCol}.
#' @param strNumeratorDateCol The name of the date column in \code{dfNumerator}
#'                            (must be of class "Date", "POSIXct", or "POSIXlt").
#' @param strDenominatorDateCol The name of the date column in \code{dfDenominator}
#'                              (must be of class "Date", "POSIXct", or "POSIXlt").
#' @param domain Character string specifying the desired time aggregation for the output:
#'   \itemize{
#'     \item \code{"original"} (default): Returns the timeline at the granularity of
#'       \code{dfDenominatorDateCol}.
#'     \item \code{"month_gap"}: Aggregates the timeline to the month of the
#'       denominator date.
#'     \item \code{"month"}: Expands the \code{"month_gap"} results to fill in all
#'       intermediate months between the subject's first and last denominator date,
#'       carrying the previous cumulative values forward.
#'   }
#'
#' @return A tibble containing the subject, group, date, cumulative denominator
#'   count, and cumulative numerator count, according to the specified \code{domain}.

#' @export
timeline <- function(
  dfSubjects,
  dfNumerator,
  dfDenominator,
  strGroupCol,
  strGroupLevel = NULL,
  strSubjectCol,
  strNumeratorCol = NULL,
  strDenominatorCol = NULL,
  strNumeratorDateCol,
  strDenominatorDateCol,
  domain= "original"
  ) {

  if (!domain %in% c("original", "month_gap", "month")) {
    stop("Invalid 'domain' value. Must be 'original', 'month_gap' or 'month'.")
  }


  # dfSubjects
  #  - strSubjectCol
  #  - strGroupCol
  #  - strGroupLevel = NULL
  strGroupLevel <- if (is.null(strGroupLevel)) strGroupCol else strGroupLevel

  dfSubjectsCols <- c(strSubjectCol, strGroupCol)
  # checking all columns exist
  stopifnot( all( dfSubjectsCols %in% names(dfSubjects) ) )

  subjects <-
    dfSubjects %>%
    select( all_of( dfSubjectsCols ) ) %>%
    rename(subject = {{ strSubjectCol }},
           group   = {{ strGroupCol }}
          ) %>%
    filter(!is.na(.data$subject)) %>%
    mutate(group_level = strGroupLevel)


  # dfDenominator
  #  - strDenominatorCol = NULL
  #  - strDenominatorDateCol
    
  strDenominatorCol <- if (is.null(strDenominatorCol)) strSubjectCol else strDenominatorCol

  dfDenominatorCols <- c(strDenominatorCol, strDenominatorDateCol)
  # checkin all columns exist
  stopifnot( all( dfDenominatorCols %in% names(dfDenominator) ) )
  # checking date column is of type Date or similar (timestamp)
  stopifnot(inherits( dfDenominator[[ strDenominatorDateCol ]],
                      c("Date", "POSIXct", "POSIXlt")
                    )
           )

  denominator <-
    dfDenominator %>%
    select( all_of( dfDenominatorCols ) ) %>%
    rename( subject = {{ strDenominatorCol }},
            date    = {{ strDenominatorDateCol }}
          ) %>%
    filter( .data$subject %in% subjects$subject &
            !is.na(.data$date)
          ) %>% # keeping only subjects in dfSubjects and with non-NA dates
    inner_join(subjects, by= "subject") # adding group


  # dfNumerator
  #  - strNumeratorCol = NULL
  #  - strNumeratorDateCol

  strNumeratorCol <- if (is.null(strNumeratorCol)) strSubjectCol else strNumeratorCol
    
  dfNumeratorCols <- c(strNumeratorCol, strNumeratorDateCol)
  # checkin all columns exist
  stopifnot( all( dfNumeratorCols %in% names(dfNumerator) ) )
  # checking date column is of type Date or similar (timestamp)
  stopifnot(inherits(dfNumerator[[ strNumeratorDateCol ]],
                     c("Date", "POSIXct", "POSIXlt")
                    )
           )

  numerator <-
    dfNumerator %>%
    select( all_of( dfNumeratorCols ) ) %>%
    rename( subject = {{ strNumeratorCol }},
            date    = {{ strNumeratorDateCol }}
          ) %>%
    filter( .data$subject %in% denominator$subject &
            !is.na(.data$date)
          ) %>% # keeping only subjects in denominator and with non-NA dates
    inner_join(subjects, by= "subject") # adding group


  # Building timeline

  prep_denominator <-
    denominator %>%
    group_by(.data$group, .data$subject) %>%
    arrange(.data$date, .by_group=TRUE) %>%
    # previous_date is not needed, just adding it for sanity checks
    # later on.
    mutate(previous_denominator_date = lag(.data$date)) %>%
    mutate( type = 1, # Add a type flag: 1 for visit start
            .id  = row_number() # row number in the denominator table
          ) %>%
    ungroup

  prep_numerator <-
    numerator %>%
    mutate( type = 0, # add a type flag: 0 for the actual event
            .id  = NA_integer_ # placeholder to match .id column above
          )

  union_numerator_denominator <- bind_rows(prep_numerator, prep_denominator)

  # Assigning groups

  assign_group_to_numerator_denominator <-
    union_numerator_denominator %>%
    arrange(.data$group_level,
            .data$group,
            .data$subject,
            .data$date,
            .data$type) %>%
    group_by(.data$group_level, .data$group, .data$subject) %>%
    # Refers to the denominator group both the denominator and the numerator
    # belong to
    mutate(denominator = cumsum(.data$type)) %>%
    ungroup
    
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
    mutate(check_numerator= ifelse(is.na(.data$numerator_date), TRUE,
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
                            )
          )

  stopifnot(all(numerator_denominator_joined$check_numerator))
  numerator_denominator_joined <- 
    numerator_denominator_joined %>%
    select(-c("check_numerator", "previous_denominator_date"))


  timeline <-
    numerator_denominator_joined %>%
    arrange(.data$subject,
            .data$group,
            .data$group_level,
            .data$denominator_date,
            .data$denominator
           ) %>%
    group_by(.data$subject,
             .data$group,
             .data$group_level,
             .data$denominator_date,
             .data$denominator
            ) %>%
    summarise( numerator = sum(!is.na(.data$numerator_date)),
               .groups   = "drop") %>%
    group_by(.data$subject, .data$group, .data$group_level) %>%
    mutate(numerator = cumsum(.data$numerator)) %>%
    ungroup

  ##
  ## domain: original
  ##
  if (domain == "original") return( timeline )

  timeline_month_gap <-
    timeline %>%
    group_by(.data$subject,
             .data$group,
             .data$group_level,
             denominator_month = trunc(.data$denominator_date, "months")
            ) %>%
    summarise( denominator = max(.data$denominator),
               numerator   = max(.data$numerator),
               .groups = "drop"
             ) %>%
    ungroup

  ##
  ## domain: month_gap
  ##
  if (domain == "month_gap") return( timeline_month_gap )


  min_max_denominator_dates_per_subject <-
    timeline %>%
    group_by(.data$subject, .data$group, .data$group_level) %>%
    summarise( min_denominator_date = trunc(min(.data$denominator_date), "months"),
               max_denominator_date = trunc(max(.data$denominator_date), "months"),
               .groups = "drop"
             ) %>%
    ungroup
    
  min_year_month <- min(min_max_denominator_dates_per_subject$min_denominator_date)
  max_year_month <- max(min_max_denominator_dates_per_subject$max_denominator_date)

  year_month_backbone <- data.frame(year_month= seq(min_year_month, max_year_month, by="months"))

  by <- join_by(between(y$year_month, x$min_denominator_date, x$max_denominator_date))
  timeline_month_backbone <-
    min_max_denominator_dates_per_subject %>%
    left_join(year_month_backbone, by) %>%
    select(-c("min_denominator_date", "max_denominator_date"))


  timeline_month <-
    timeline_month_backbone %>%
    left_join(timeline_month_gap,
              by = join_by( "subject",
                            "group",
                            "group_level",
                            "year_month" == "denominator_month"
                          )
             ) %>%
    group_by(.data$subject, .data$group, .data$group_level) %>%
    arrange(.data$year_month) %>%
    mutate(denominator = purrr::accumulate(.data$denominator, ~ if (is.na(.y)) .x else .y),
           numerator   = purrr::accumulate(.data$numerator  , ~ if (is.na(.y)) .x else .y)
          ) %>%
    ungroup() %>%
    rename(denominator_month = "year_month")

  ##
  ## domain: month
  ##
  return(timeline_month)
}