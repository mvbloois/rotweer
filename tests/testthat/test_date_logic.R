library(testthat)
library(lubridate)

# Resolve paths relative to the project root regardless of the working
# directory from which the test runner is invoked.
proj_root <- Sys.getenv("ROTWEER_ROOT", unset = getwd())
source(file.path(proj_root, "R", "utils.R"))

# ── compute_last_day ──────────────────────────────────────────────────────────

test_that("compute_last_day returns max_date when it is the last day of the month", {
  last_of_march <- ymd("2024-03-31")
  expect_equal(compute_last_day(last_of_march), last_of_march)
})

test_that("compute_last_day returns last day of previous month for mid-month dates", {
  mid_march <- ymd("2024-03-15")
  # Previous month end: Feb 29 (2024 is a leap year)
  expect_equal(compute_last_day(mid_march), ymd("2024-02-29"))
})

test_that("compute_last_day handles leap-year Feb 29 as a valid month-end", {
  feb29 <- ymd("2024-02-29")
  expect_equal(compute_last_day(feb29), feb29)
})

test_that("compute_last_day returns correct prior month-end in non-leap year", {
  mid_march_non_leap <- ymd("2023-03-10")
  expect_equal(compute_last_day(mid_march_non_leap), ymd("2023-02-28"))
})

# ── compute_first_day ─────────────────────────────────────────────────────────

test_that("compute_first_day returns the first day of the month 12 months before last_day", {
  last_day <- ymd("2024-03-31")
  # last_day %m-% years(1) = 2023-03-31 -> floor to month = 2023-03-01
  expect_equal(compute_first_day(last_day), ymd("2023-03-01"))
})

test_that("compute_first_day handles month-end arithmetic correctly", {
  last_day <- ymd("2024-01-31")
  # 2024-01-31 %m-% years(1) = 2023-01-31 -> floor = 2023-01-01
  expect_equal(compute_first_day(last_day), ymd("2023-01-01"))
})

# ── compute_base_years ────────────────────────────────────────────────────────

test_that("compute_base_years returns a 30-year integer sequence", {
  last_day   <- ymd("2024-03-31")
  base_years <- compute_base_years(last_day)
  expect_length(base_years, 30L)
})

test_that("compute_base_years ends 2 years before last_day year", {
  last_day <- ymd("2024-03-31")
  expect_equal(max(compute_base_years(last_day)), 2022L)
})

test_that("compute_base_years starts 31 years before last_day year", {
  last_day <- ymd("2024-03-31")
  expect_equal(min(compute_base_years(last_day)), 1993L)
})
