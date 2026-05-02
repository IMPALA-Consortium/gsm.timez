# Tests for Timeline function
#
# - Output structure
#     - column names match expected schema
# - Row uniqueness
#     - one row per site-month combination
# - Site coverage
#     - all sites with visit data are represented
# - Denominator (cumulative visits)
#     - monotonically non-decreasing within each site
#     - aggregates cumulative visits correctly across subjects
# - NMonth numbering
#     - sequential month index per site
# - Numerator (cumulative events)
#     - aggregates cumulative events correctly across subjects
# - Edge cases
#     - subjects with no events (Numerator = 0)

test_that("Column names match expected site-level output", {
  df <-
    Timeline(
      dfSubjects = clindata::rawplus_dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d")),
      strGroupCol = "invid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )
  expect_equal(
    names(df),
    c(
      "GroupID", "GroupLevel", "Numerator", "Denominator",
      "DenominatorMonth", "NMonth"
    )
  )
})


test_that("Timeline returns one row per site-month combination", {
  dm <- clindata::rawplus_dm
  visits <- clindata::rawplus_visdt %>%
    mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d"))

  df <-
    Timeline(
      dfSubjects = dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = visits,
      strGroupCol = "invid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )

  # Check that each row is unique by GroupID + DenominatorMonth
  expect_equal(
    nrow(df),
    nrow(distinct(df, GroupID, DenominatorMonth))
  )
})


test_that("Timeline contains all sites from subjects with visit data", {
  dm <- clindata::rawplus_dm
  visits <- clindata::rawplus_visdt %>%
    mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d"))

  df <-
    Timeline(
      dfSubjects = dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = visits,
      strGroupCol = "invid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )

  # Expected sites: sites from dm that have subjects with non-NA visit dates
  subjects_with_visits <- visits %>%
    filter(!is.na(visit_dt)) %>%
    distinct(subjid) %>%
    pull(subjid)

  expected_sites <- dm %>%
    filter(subjid %in% subjects_with_visits) %>%
    distinct(invid) %>%
    pull(invid)

  expect_setequal(unique(df$GroupID), expected_sites)
})


test_that("denominator (cumulative visits) is monotonically non-decreasing per site", {
  dm <- clindata::rawplus_dm
  visits <- clindata::rawplus_visdt %>%
    mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d"))

  df <-
    Timeline(
      dfSubjects = dm,
      dfNumerator = clindata::rawplus_ae,
      dfDenominator = visits,
      strGroupCol = "invid",
      strSubjectCol = "subjid",
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )

  # Check that Denominator (cumulative visits) never decreases within each site
  check_monotonic <- df %>%
    group_by(GroupID) %>%
    arrange(DenominatorMonth) %>%
    mutate(is_nondecreasing = Denominator >= lag(Denominator, default = 0)) %>%
    pull(is_nondecreasing)

  expect_true(all(check_monotonic))
})


test_that("denominator aggregates cumulative visits correctly", {
  # Simple test: 2 subjects at same site, each with cumulative visit counts
  dfSubjects <- data.frame(
    SubjectID = c("S1", "S2"),
    SiteID = c("A", "A")
  )
  dfNumerator <- data.frame(
    SubjectID = character(0),
    EventDate = as.Date(character(0))
  )
  dfDenominator <- data.frame(
    SubjectID = c("S1", "S1", "S2"),
    VisitDate = as.Date(c("2022-01-01", "2022-01-15", "2022-01-10"))
  )

  df <-
    Timeline(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strGroupCol = "SiteID",
      strSubjectCol = "SubjectID",
      strNumeratorDateCol = "EventDate",
      strDenominatorDateCol = "VisitDate"
    )

  # Site A in Jan 2022:
  # - S1 has 2 visits by end of Jan (cumulative = 2)
  # - S2 has 1 visit by end of Jan (cumulative = 1)
  # - Total site Denominator (cumulative visits) = 2 + 1 = 3
  expect_equal(df$Denominator[df$GroupID == "A"], 3)
})


test_that("n_month is sequential month number per site", {
  dfSubjects <- data.frame(
    SubjectID = c("S1", "S2"),
    SiteID = c("A", "A")
  )
  dfNumerator <- data.frame(
    SubjectID = character(0),
    EventDate = as.Date(character(0))
  )
  # S1 has visits in Jan, Feb, Mar; S2 has visits in Jan and Mar
  dfDenominator <- data.frame(
    SubjectID = c("S1", "S1", "S1", "S2", "S2"),
    VisitDate = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01", "2022-01-15", "2022-03-15"))
  )

  df <-
    Timeline(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strGroupCol = "SiteID",
      strSubjectCol = "SubjectID",
      strNumeratorDateCol = "EventDate",
      strDenominatorDateCol = "VisitDate"
    )

  # Site A should have NMonth = 1, 2, 3 for the three months
  site_a <- df %>%
    filter(GroupID == "A") %>%
    arrange(DenominatorMonth)
  expect_equal(site_a$NMonth, c(1, 2, 3))
})


test_that("numerator aggregates cumulative events correctly", {
  dfSubjects <- data.frame(
    SubjectID = c("S1", "S2"),
    SiteID = c("A", "A")
  )
  dfNumerator <- data.frame(
    SubjectID = c("S1", "S1", "S2"),
    EventDate = as.Date(c("2022-01-01", "2022-01-05", "2022-01-10"))
  )
  dfDenominator <- data.frame(
    SubjectID = c("S1", "S2"),
    VisitDate = as.Date(c("2022-01-15", "2022-01-15"))
  )

  df <-
    Timeline(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strGroupCol = "SiteID",
      strSubjectCol = "SubjectID",
      strNumeratorDateCol = "EventDate",
      strDenominatorDateCol = "VisitDate"
    )

  # Site A in Jan 2022:
  # - S1 has 2 events
  # - S2 has 1 event
  # - Total site Numerator = 2 + 1 = 3
  expect_equal(df$Numerator[df$GroupID == "A"], 3)
})


test_that("Timeline handles subjects with no events correctly", {
  dfSubjects <- data.frame(
    SubjectID = c("S1", "S2"),
    SiteID = c("A", "A")
  )
  dfNumerator <- data.frame(
    SubjectID = character(0),
    EventDate = as.Date(character(0))
  )
  dfDenominator <- data.frame(
    SubjectID = c("S1", "S2"),
    VisitDate = as.Date(c("2022-01-01", "2022-01-01"))
  )

  df <-
    Timeline(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strGroupCol = "SiteID",
      strSubjectCol = "SubjectID",
      strNumeratorDateCol = "EventDate",
      strDenominatorDateCol = "VisitDate"
    )

  expect_equal(df$Numerator[df$GroupID == "A"], 0)
  expect_equal(df$Denominator[df$GroupID == "A"], 2) # 2 subjects, 1 visit each
})


# =============================================================================
# COMMENTED TESTS: These tests are for columns that are not currently included
# in Timeline() output. Re-enable if num_patients and cum_num_patients columns
# are added back to the output.
# =============================================================================

# test_that("num_patients counts distinct subjects per site-month", {
#   # Create simple test data
#   # S1 and S2 at Site A, S3 at Site B
#   # S1: visits in Jan and Feb
#   # S2: visits in Jan and Feb (so they appear in both months)
#   # S3: visit in Feb only
#   dfSubjects <- data.frame(
#     SubjectID = c("S1", "S2", "S3"),
#     SiteID = c("A", "A", "B")
#   )
#   dfNumerator <- data.frame(
#     SubjectID = c("S1", "S2", "S3"),
#     EventDate = as.Date(c("2022-01-15", "2022-01-20", "2022-02-01"))
#   )
#   dfDenominator <- data.frame(
#     SubjectID = c("S1", "S1", "S2", "S2", "S3"),
#     VisitDate = as.Date(c("2022-01-01", "2022-02-01", "2022-01-15", "2022-02-15", "2022-02-01"))
#   )
#
#   df <-
#     Timeline(
#       dfSubjects = dfSubjects,
#       dfNumerator = dfNumerator,
#       dfDenominator = dfDenominator,
#       strGroupCol = "SiteID",
#       strSubjectCol = "SubjectID",
#       strNumeratorDateCol = "EventDate",
#       strDenominatorDateCol = "VisitDate"
#     )
#
#   # Site A in Jan 2022: S1 and S2 both have visits
#   site_a_jan <- df %>% filter(group == "A" & denominator_month == as.Date("2022-01-01"))
#   expect_equal(site_a_jan$num_patients, 2)
#
#   # Site A in Feb 2022: S1 and S2 both have visits
#   site_a_feb <- df %>% filter(group == "A" & denominator_month == as.Date("2022-02-01"))
#   expect_equal(site_a_feb$num_patients, 2)
#
#   # Site B: only S3
#   site_b <- df %>% filter(group == "B")
#   expect_true(all(site_b$num_patients == 1))
# })


# test_that("cum_num_patients is monotonically non-decreasing per site", {
#   dm     <- clindata::rawplus_dm
#   visits <- clindata::rawplus_visdt %>%
#             mutate(visit_dt = as.Date(visit_dt, "%Y-%m-%d"))
#
#   df <-
#     Timeline(
#       dfSubjects    = dm,
#       dfNumerator   = clindata::rawplus_ae,
#       dfDenominator = visits,
#       strGroupCol   = "siteid",
#       strSubjectCol = "subjid",
#       strNumeratorDateCol   = "aest_dt",
#       strDenominatorDateCol = "visit_dt"
#     )
#
#   # Check that cum_num_patients never decreases within each site
#   check_monotonic <- df %>%
#     group_by(group) %>%
#     arrange(denominator_month) %>%
#     mutate(is_nondecreasing = cum_num_patients >= lag(cum_num_patients, default = 0)) %>%
#     pull(is_nondecreasing)
#
#   expect_true(all(check_monotonic))
# })


# test_that("cum_num_patients tracks enrollment curve correctly", {
#   # Create data where patients enroll in different months
#   dfSubjects <- data.frame(
#     SubjectID = c("S1", "S2", "S3"),
#     SiteID = c("A", "A", "A")
#   )
#   dfNumerator <- data.frame(
#     SubjectID = character(0),
#     EventDate = as.Date(character(0))
#   )
#   # S1 first visit in Jan, S2 first visit in Jan, S3 first visit in Feb
#   dfDenominator <- data.frame(
#     SubjectID = c("S1", "S2", "S3", "S1", "S2"),
#     VisitDate = as.Date(c("2022-01-01", "2022-01-15", "2022-02-01", "2022-02-15", "2022-02-20"))
#   )
#
#   df <-
#     Timeline(
#       dfSubjects = dfSubjects,
#       dfNumerator = dfNumerator,
#       dfDenominator = dfDenominator,
#       strGroupCol = "SiteID",
#       strSubjectCol = "SubjectID",
#       strNumeratorDateCol = "EventDate",
#       strDenominatorDateCol = "VisitDate"
#     )
#
#   # Jan 2022: S1 and S2 enrolled -> cum_num_patients = 2
#   jan_row <- df %>% filter(denominator_month == as.Date("2022-01-01"))
#   expect_equal(jan_row$cum_num_patients, 2)
#
#   # Feb 2022: S3 newly enrolled -> cum_num_patients = 3
#   feb_row <- df %>% filter(denominator_month == as.Date("2022-02-01"))
#   expect_equal(feb_row$cum_num_patients, 3)
# })
