# Check column names for
# - domain="original"
# - domain="month_gap"
# - domain="month"

# domain=original
test_that("Column names match: domain=original", {
  df <-
    timeline(
      dfSubjects    = clindata::rawplus_dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt= as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol   = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )
  expect_equal(names(df),
               c("subject", "group", "group_level", "denominator_date",
                 "denominator", "numerator")
              )
})

# domain=month_gap
test_that("Column names match: domain=month_gap", {
  df <-
    timeline(
      dfSubjects    = clindata::rawplus_dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt= as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol   = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = "visit_dt",
      domain = "month_gap"
    )
  expect_equal(names(df),
               c("subject", "group", "group_level", "denominator_month",
                 "denominator", "numerator")
              )
})

# domain=month
test_that("Column names match: domain=month", {
  df <-
    timeline(
      dfSubjects    = clindata::rawplus_dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt= as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol   = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = "visit_dt",
      domain = "month"
    )
  expect_equal( names(df),
                c("subject", "group", "group_level", "denominator_month",
                  "denominator", "numerator")
              )
})


test_that("timeline from all domains contain the same subset of subjects from
           dfSubjects with non-na dates in dfDenominator", {

  dm     <- clindata::rawplus_dm
  visits <- clindata::rawplus_visdt %>%
            mutate(visit_dt= as.Date(visit_dt, "%Y-%m-%d"))

  original <-
    timeline(
      dfSubjects    = dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = visits,
      strGroupCol   = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )
  
  month_gap <-
    timeline(
      dfSubjects    = dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = visits,
      strGroupCol   = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = "visit_dt",
      domain = "month_gap"
    )
  
  month <-
    timeline(
      dfSubjects    = dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = visits,
      strGroupCol   = "siteid",
      strSubjectCol = "subjid",
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = "visit_dt",
      domain = "month"
    )

  # checking dfs from all domains contain the same subjects
  expect_setequal(original$subject,  month_gap$subject)
  expect_setequal(month_gap$subject, month$subject)

  # timeline contains the same set of subjects as dfDenominator with non-na dates
  expect_setequal(original$subject,
                  visits %>% filter(!is.na(visit_dt)) %>% pull(subjid)
                 )
})


##
## timeline consistency with dfDenominator
##

# domain=original
test_that("timeline is consistent with dfDenominator: domain=original", {
  dfSubjects <- clindata::rawplus_dm
  strSubjectCol <- "subjid"

  dfDenominator <- clindata::rawplus_visdt %>%
                   mutate(visit_dt= as.Date(visit_dt, "%Y-%m-%d"))
  strDenominatorDateCol <- "visit_dt"

  df <-
    timeline(
      dfSubjects    = clindata::rawplus_dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = dfDenominator,
      strGroupCol   = "siteid",
      strSubjectCol = strSubjectCol,
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = strDenominatorDateCol,
      domain = "original"
    )

  expect_equal( nrow(df),
                # only rows with subjects that are present in dfSubjects and
                # whose strDenominatorDateCol is not NA
                dfDenominator %>% 
                filter( .data[[strSubjectCol]] %in% dfSubjects[[strSubjectCol]] &
                        !is.na(.data[[strDenominatorDateCol]])
                      ) %>%
                nrow
              )
})

# domain=month_gap
test_that("timeline is consistent with dfDenominator: domain=month_gap", {
  dfSubjects <- clindata::rawplus_dm
  strSubjectCol <- "subjid"

  dfDenominator <- clindata::rawplus_visdt %>%
                   mutate(visit_dt= as.Date(visit_dt, "%Y-%m-%d"))
  strDenominatorDateCol <- "visit_dt"

  strGroupCol <- "siteid"

  df <-
    timeline(
      dfSubjects    = clindata::rawplus_dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = dfDenominator,
      strGroupCol   = strGroupCol,
      strSubjectCol = strSubjectCol,
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = strDenominatorDateCol,
      domain = "month_gap"
    )

  expect_equal( nrow(df),
                # only rows with subjects that are present in dfSubjects and
                # whose strDenominatorDateCol is not NA
                dfDenominator %>% 
                filter( .data[[strSubjectCol]] %in% dfSubjects[[strSubjectCol]] &
                        !is.na(.data[[strDenominatorDateCol]])
                      ) %>%
                mutate("{strDenominatorDateCol}" := trunc(.data[[strDenominatorDateCol]], "months")) %>%
                select(all_of(c(strSubjectCol, strGroupCol, strDenominatorDateCol))) %>%
                distinct %>%
                nrow
              )
})

# domain=month
test_that("timeline is consistent with dfDenominator: domain=month", {
  dfSubjects <- clindata::rawplus_dm
  strSubjectCol <- "subjid"

  dfDenominator <- clindata::rawplus_visdt %>%
                   mutate(visit_dt= as.Date(visit_dt, "%Y-%m-%d"))
  strDenominatorDateCol <- "visit_dt"

  strGroupCol <- "siteid"

  df <-
    timeline(
      dfSubjects    = clindata::rawplus_dm,
      dfNumerator   = clindata::rawplus_ae,
      dfDenominator = dfDenominator,
      strGroupCol   = strGroupCol,
      strSubjectCol = strSubjectCol,
      strNumeratorDateCol   = "aest_dt",
      strDenominatorDateCol = strDenominatorDateCol,
      domain = "month"
    )

  # generating date sequence (from min to max date)
  min_trunc_date <- trunc(min(dfDenominator[[strDenominatorDateCol]], na.rm=TRUE), "months")
  max_trunc_date <- trunc(max(dfDenominator[[strDenominatorDateCol]], na.rm=TRUE), "months")

  date_sequence_backbone <- data.frame(year_month= seq(min_trunc_date,
                                                       max_trunc_date,
                                                       by="months")
                                      )

  # joining dfDenominator with the date sequence
  fulldatesequence_dfDenominator <-
    dfDenominator %>% 
    filter( .data[[strSubjectCol]] %in% dfSubjects[[strSubjectCol]] &
            !is.na(.data[[strDenominatorDateCol]])
          ) %>%
    mutate("{strDenominatorDateCol}" := trunc(.data[[strDenominatorDateCol]], "months")) %>%
    select(all_of(c(strSubjectCol, strGroupCol, strDenominatorDateCol))) %>%
    group_by(across(all_of(c(strSubjectCol,strGroupCol)))
            ) %>%
    summarise( min_date = trunc(min(.data[[strDenominatorDateCol]]), "months"),
               max_date = trunc(max(.data[[strDenominatorDateCol]]), "months"),
               .groups = "drop"
             ) %>%
    left_join( date_sequence_backbone,
               by= join_by(between(y$year_month,
                                   x$min_date,
                                   x$max_date)
                          )
             ) %>%
    select(-c(min_date, max_date))


  expect_equal(nrow(df), nrow(fulldatesequence_dfDenominator))
})