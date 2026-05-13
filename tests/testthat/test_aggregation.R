library(testthat)
library(dplyr)
library(lubridate)

proj_root <- Sys.getenv("ROTWEER_ROOT", unset = getwd())
source(file.path(proj_root, "R", "utils.R"))

# ── Monthly aggregation ───────────────────────────────────────────────────────

test_that("monthly rainfall totals are computed correctly", {
  df <- tibble::tibble(
    date = seq.Date(ymd("2024-01-01"), ymd("2024-01-31"), by = "day"),
    rain = rep(2.0, 31)
  )
  result <- df |>
    group_by(yr = year(date), mth = month(date)) |>
    summarise(sum_rain = sum(rain), .groups = "drop")
  expect_equal(result$sum_rain, 62.0)
})

test_that("monthly sunshine totals are computed correctly", {
  df <- tibble::tibble(
    date = seq.Date(ymd("2024-02-01"), ymd("2024-02-29"), by = "day"),
    sun  = rep(3.0, 29)
  )
  result <- df |>
    group_by(yr = year(date), mth = month(date)) |>
    summarise(sum_sun = sum(sun), .groups = "drop")
  expect_equal(result$sum_sun, 87.0)
})

test_that("baseline mean aggregation averages correctly across years", {
  # Two years of January data, 1 mm/day each year
  df <- tibble::tibble(
    date = c(
      seq.Date(ymd("2020-01-01"), ymd("2020-01-31"), by = "day"),
      seq.Date(ymd("2021-01-01"), ymd("2021-01-31"), by = "day")
    ),
    rain = rep(1.0, 62)
  )
  result <- df |>
    group_by(yr = year(date), mth = month(date)) |>
    summarise(sum_rain = sum(rain), .groups = "drop") |>
    group_by(mth) |>
    summarise(mean_mth_rain = mean(sum_rain), .groups = "drop")
  expect_equal(result$mean_mth_rain, 31.0)
})

# ── validate_columns ──────────────────────────────────────────────────────────

test_that("validate_columns returns df invisibly when all columns are present", {
  df  <- data.frame(a = 1, b = 2, c = 3)
  out <- validate_columns(df, c("a", "b"))
  expect_identical(out, df)
})

test_that("validate_columns stops with informative message when columns are missing", {
  df <- data.frame(a = 1, b = 2)
  expect_error(validate_columns(df, c("a", "c"), context = "test"), "c")
})

test_that("validate_columns includes the context label in the error message", {
  df <- data.frame(x = 1)
  expect_error(validate_columns(df, c("y"), context = "myctx"), "myctx")
})

# ── validate_non_empty ────────────────────────────────────────────────────────

test_that("validate_non_empty returns df invisibly when df is non-empty", {
  df  <- data.frame(x = 1:3)
  out <- validate_non_empty(df)
  expect_identical(out, df)
})

test_that("validate_non_empty stops with informative message for empty data frame", {
  df <- data.frame(x = integer(0))
  expect_error(validate_non_empty(df, context = "empty_test"), "empty")
})

# ── validate_range ────────────────────────────────────────────────────────────

test_that("validate_range issues no warning when all values are in range", {
  df <- data.frame(temp = c(-10, 0, 20, 35))
  expect_no_warning(validate_range(df, "temp", -30, 45))
})

test_that("validate_range warns when values fall outside the specified range", {
  df <- data.frame(temp = c(-5, 50))   # 50 > 45
  expect_warning(validate_range(df, "temp", -30, 45), "1 value")
})

test_that("validate_range ignores NA values", {
  df <- data.frame(temp = c(NA_real_, 20))
  expect_no_warning(validate_range(df, "temp", -30, 45))
})

# ── validate_output_file ─────────────────────────────────────────────────────

test_that("validate_output_file stops for non-existent file", {
  expect_error(validate_output_file("/tmp/definitely_does_not_exist.png"), "not created")
})

test_that("validate_output_file stops for zero-byte file", {
  tmp <- tempfile(fileext = ".png")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(validate_output_file(tmp), "empty")
})

test_that("validate_output_file returns path invisibly for a non-empty file", {
  tmp <- tempfile(fileext = ".png")
  writeLines("fake png content", tmp)
  on.exit(unlink(tmp))
  out <- validate_output_file(tmp)
  expect_equal(out, tmp)
})
