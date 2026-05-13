# R/utils.R — Shared logic for date-window computation and data validation.
# Sourced by rotweer.R and the test suite.  Only depends on base R and
# lubridate so it can be loaded in isolation during testing.

library(lubridate)

# ── Date window helpers ────────────────────────────────────────────────────────

# Determine the last day to include in the chart.
# If max_date is the final calendar day of its month the data for that month
# are complete and max_date is returned unchanged.  Otherwise the last day of
# the *previous* month is returned so that only complete months appear in the
# visualisation.
compute_last_day <- function(max_date) {
  month_end <- ceiling_date(max_date, unit = "month") - days(1)
  if (max_date == month_end) max_date else floor_date(max_date, unit = "month") - days(1)
}

# Compute the first day of the 13-month display window.
# Returns the first calendar day of the month that falls approximately one
# year before last_day.
compute_first_day <- function(last_day) {
  floor_date(last_day %m-% years(1), unit = "month")
}

# Select the 30-year historical baseline period.
# Returns an integer vector of 30 calendar years ending two full years before
# last_day.  The two-year lag ensures the baseline never overlaps the display
# window.
compute_base_years <- function(last_day) {
  seq(year(last_day) - 31L, year(last_day) - 2L, 1L)
}

# ── Data validation helpers ────────────────────────────────────────────────────

# Stop with a clear message if expected columns are absent from a data frame.
validate_columns <- function(df, required_cols, context = "") {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0L) {
    prefix <- if (nchar(context) > 0L) paste0("[", context, "] ") else ""
    stop(prefix, "Required columns missing: ", paste(missing_cols, collapse = ", "))
  }
  invisible(df)
}

# Stop with a clear message if a data frame has zero rows.
validate_non_empty <- function(df, context = "") {
  if (nrow(df) == 0L) {
    prefix <- if (nchar(context) > 0L) paste0("[", context, "] ") else ""
    stop(prefix, "Dataset is unexpectedly empty.")
  }
  invisible(df)
}

# Warn when values fall outside a plausible meteorological range.
validate_range <- function(df, col, min_val, max_val, context = "") {
  vals  <- df[[col]]
  n_out <- sum(!is.na(vals) & (vals < min_val | vals > max_val))
  if (n_out > 0L) {
    prefix <- if (nchar(context) > 0L) paste0("[", context, "] ") else ""
    warning(prefix, n_out, " value(s) in '", col,
            "' outside plausible range [", min_val, ", ", max_val, "].")
  }
  invisible(df)
}

# ── Output validation ──────────────────────────────────────────────────────────

# Stop with a clear message if an expected output file is missing or empty.
validate_output_file <- function(path) {
  if (!file.exists(path)) {
    stop("Expected output file was not created: ", path)
  }
  if (file.info(path)$size == 0L) {
    stop("Output file is empty (0 bytes): ", path)
  }
  message("Output OK: ", path, " (", file.info(path)$size, " bytes)")
  invisible(path)
}
