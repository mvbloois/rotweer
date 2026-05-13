# rotweer.R — Rotterdam Weather in Perspective
#
# Downloads daily weather data for KNMI station 344 (Rotterdam), derives a
# 13-month display window and a 30-year historical baseline, and produces three
# stacked charts (temperature, rainfall, sunshine) saved as PNG files.
#
# Run locally:  Rscript rotweer.R          (from the project root)
# Run tests:    Rscript tests/run_tests.R  (from the project root)

# ── 0. Dependencies ────────────────────────────────────────────────────────────

library(tidyverse)
library(janitor)
library(patchwork)
library(slider)
library(showtext)

source("R/utils.R")

# ── 1. Configuration ───────────────────────────────────────────────────────────

# Data source
STATION_ID <- "344"   # KNMI station: Rotterdam
DATA_URL   <- paste0(
  "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/",
  "etmgeg_", STATION_ID, ".zip"
)
DATA_ZIP   <- "data/etmgeg_344.zip"
DATA_SKIP  <- 50L     # Header rows to skip in the KNMI CSV export

# The KNMI file uses a fixed comma-delimited format with these relevant columns:
#   YYYYMMDD  – date
#   TG        – daily mean temperature (0.1 °C)
#   RH        – daily precipitation (0.1 mm;  -1 = <0.05 mm)
#   SQ        – sunshine duration    (0.1 h;   -1 = <0.05 h)
KNMI_REQUIRED_COLS <- c("yyyymmdd", "tg", "rh", "sq")

# Colours
C_AVERAGE    <- "#d5dcdc"
C_HOT        <- "#CE0000"
C_COLD       <- "#000063"
C_RAIN       <- "#1e7df0"
C_SUN        <- "#e1ad01"
C_LINES      <- "#548c8c"
C_TEXT       <- "#ededed"
C_BACKGROUND <- "#2f4f4f"

# Output
PLOT_DIR    <- "plots"
PLOT_WIDTH  <- 10
PLOT_HEIGHT <- 10
PLOT_DPI    <- 300

# ── 2. Data ingestion ──────────────────────────────────────────────────────────

message("Downloading KNMI data from ", DATA_URL, " ...")
tryCatch(
  download.file(DATA_URL, DATA_ZIP, quiet = TRUE),
  error = function(e) stop("Download failed: ", conditionMessage(e))
)

if (!file.exists(DATA_ZIP)) {
  stop("Downloaded file not found at: ", DATA_ZIP)
}
message("Download complete (", file.info(DATA_ZIP)$size, " bytes).")

message("Reading data ...")
rt_raw <- read_delim(DATA_ZIP, delim = ",", skip = DATA_SKIP,
                     show_col_types = FALSE) |>
  clean_names()

validate_columns(rt_raw, KNMI_REQUIRED_COLS, context = "KNMI raw")
validate_non_empty(rt_raw, context = "KNMI raw")
message("Read ", nrow(rt_raw), " raw rows.")

# ── 3. Date window ─────────────────────────────────────────────────────────────

# last_day  : the latest complete month-end in the data.  If the download
#             contains a partial month only months up to the previous full
#             month-end are visualised.
# first_day : start of the ~13-month display window (one year back).
# base_years: 30-year baseline for historical comparison (ends 2 years before
#             last_day to avoid overlap with the display window).

max_date   <- ymd(max(rt_raw$yyyymmdd))
last_day   <- compute_last_day(max_date)
first_day  <- compute_first_day(last_day)
base_years <- compute_base_years(last_day)

message(
  "Display window : ", first_day, " to ", last_day, "\n",
  "Baseline period: ", min(base_years), " to ", max(base_years),
  " (", length(base_years), " years)"
)

# x-axis tick positions and labels (one tick per month, centred at mid-month)
axis_breaks <- seq.Date(from = first_day, to = last_day, by = "month")
axis_day    <- axis_breaks + days(14)
axis_month  <- format(axis_breaks, "%m-%Y")

# ── 4. ggplot theme ────────────────────────────────────────────────────────────

font_add_google(name = "Titillium Web", family = "titillium")
showtext_opts(dpi = PLOT_DPI)
showtext_auto(enable = TRUE)

theme_set(theme_minimal())
theme_update(
  text                = element_text(family = "titillium", colour = C_TEXT),
  axis.line           = element_blank(),
  panel.grid.major    = element_line(colour = C_LINES),
  panel.grid.minor    = element_blank(),
  panel.grid.major.x  = element_blank(),
  axis.text.x         = element_text(colour = C_TEXT),
  axis.text.y         = element_text(colour = C_TEXT),
  plot.title          = element_text(hjust = 0.5),
  plot.title.position = "plot",
  plot.background     = element_rect(fill = C_BACKGROUND, colour = C_BACKGROUND)
)

# ── 5. Data transformation ─────────────────────────────────────────────────────

rt_base <- rt_raw |>
  filter(yyyymmdd > 19900000) |>
  select(
    date = yyyymmdd,
    temp = tg,
    rain = rh,
    sun  = sq
  ) |>
  mutate(
    date = ymd(date),
    temp = parse_number(temp) / 10,    # 0.1 °C  → °C
    rain = parse_integer(rain) / 10,   # 0.1 mm  → mm
    sun  = parse_integer(sun)  / 10,   # 0.1 h   → h
    doy  = format(date, "%m-%d")       # day-of-year key for baseline joins
  ) |>
  mutate(
    mov_30d_mean_temp = slide_dbl(temp, mean, .before = 14, .after = 14,
                                  .complete = TRUE)
  ) |>
  drop_na() |>
  filter(date <= last_day)

validate_non_empty(rt_base, context = "rt_base")
validate_range(rt_base, "temp",  -30,  45, context = "temperature (°C)")
validate_range(rt_base, "rain",    0, 200, context = "rainfall (mm)")
validate_range(rt_base, "sun",     0,  20, context = "sunshine (h)")

# ── 6. Chart data preparation ──────────────────────────────────────────────────

## 6a. Daily temperature ────────────────────────────────────────────────────────
# Compare each day in the display window against the baseline 30-day moving
# mean for the same calendar day-of-year.

rt_temp_lastyear <- rt_base |>
  filter(date >= first_day) |>
  select(date, doy, temp)

rt_temp_ref <- rt_base |>
  filter(year(date) %in% base_years) |>
  group_by(doy) |>
  summarise(mov_temp = mean(mov_30d_mean_temp), .groups = "drop")

rt_temp <- rt_temp_ref |>
  inner_join(rt_temp_lastyear, by = "doy") |>
  mutate(
    hotness = if_else(temp > mov_temp, "hot", "cold"),
    doy     = fct_reorder(doy, date)
  )

validate_non_empty(rt_temp, context = "rt_temp join")

## 6b. Monthly rainfall and sunshine ───────────────────────────────────────────
# Build a single table shared by both the rainfall and sunshine charts.
# Each row is one calendar month in the display window with its actual totals
# and the long-term baseline mean for that calendar month.

baseline_monthly <- rt_base |>
  filter(year(date) %in% base_years) |>
  group_by(yr = year(date), mth = month(date)) |>
  summarise(sum_rain = sum(rain), sum_sun = sum(sun), .groups = "drop") |>
  group_by(mth) |>
  summarise(
    mean_mth_rain = mean(sum_rain),
    mean_mth_sun  = mean(sum_sun),
    .groups = "drop"
  )

window_monthly <- rt_base |>
  filter(date >= first_day, date <= last_day) |>
  group_by(yr = year(date), mth = month(date)) |>
  summarise(sum_rain = sum(rain), sum_sun = sum(sun), .groups = "drop") |>
  mutate(row = row_number())

tbl_monthly <- inner_join(baseline_monthly, window_monthly, by = "mth") |>
  mutate(x_label = format(as.Date(paste(yr, mth, 1L, sep = "-")), "%m-%Y"))

validate_non_empty(tbl_monthly, context = "tbl_monthly join")

# ── 7. Plots ───────────────────────────────────────────────────────────────────

## 7a. Daily temperature ────────────────────────────────────────────────────────

plt_temp <- rt_temp |>
  ggplot() +
  geom_segment(
    aes(x = date, xend = date, y = temp, yend = mov_temp, colour = hotness),
    linewidth   = 1.5,
    show.legend = FALSE
  ) +
  geom_line(aes(x = date, y = mov_temp, group = 1),
            colour = C_AVERAGE, linewidth = 1) +
  scale_x_date(limits = c(first_day, last_day),
               breaks  = axis_day,
               labels  = axis_month) +
  scale_y_continuous(labels = scales::number_format(suffix = " \u00b0C")) +
  scale_colour_manual(values = c("hot" = C_HOT, "cold" = C_COLD)) +
  labs(x = NULL, y = NULL, title = "Daily mean temperature")

## 7b. Monthly rainfall and sunshine ───────────────────────────────────────────
# Both charts share the same layout; only the data column, colour and axis
# label differ.  A helper function avoids repeating the geom/scale/labs code.

make_monthly_chart <- function(data, y_col, mean_col, fill_color, y_suffix, title) {
  ggplot(data, aes(x = fct_reorder(x_label, row), y = .data[[y_col]])) +
    geom_col(fill = fill_color) +
    geom_segment(
      aes(
        x    = row - 0.2, xend = row + 0.2,
        y    = .data[[mean_col]], yend = .data[[mean_col]]
      ),
      colour    = C_AVERAGE,
      linewidth = 1.5
    ) +
    geom_segment(
      aes(
        x    = row, xend = row,
        y    = .data[[mean_col]], yend = .data[[y_col]]
      ),
      linetype = 3,
      colour   = C_AVERAGE
    ) +
    scale_y_continuous(
      labels = scales::number_format(suffix = paste0(" ", y_suffix))
    ) +
    labs(x = NULL, y = NULL, title = title)
}

plt_rain <- make_monthly_chart(
  tbl_monthly,
  y_col      = "sum_rain",
  mean_col   = "mean_mth_rain",
  fill_color = C_RAIN,
  y_suffix   = "mm",
  title      = "Total monthly amount of rainfall"
)

plt_sun <- make_monthly_chart(
  tbl_monthly,
  y_col      = "sum_sun",
  mean_col   = "mean_mth_sun",
  fill_color = C_SUN,
  y_suffix   = "h",
  title      = "Total monthly amount of sunshine"
)

## 7c. Composite plot ───────────────────────────────────────────────────────────

plt_total <- plt_temp / plt_rain / plt_sun +
  plot_annotation(
    title = paste0(
      "Yearly Rotterdam weather summary up to ", last_day,
      " (compared to ", min(base_years), "\u2013", max(base_years), ")"
    ),
    caption = "Source: KNMI",
    theme   = theme(plot.title = element_text(size = 20))
  )

# ── 8. Export ──────────────────────────────────────────────────────────────────

dated_plot  <- file.path(PLOT_DIR, paste0("rotweer_", last_day, ".png"))
latest_plot <- file.path(PLOT_DIR, "rotweer_latest.png")

message("Saving plots ...")
ggsave(dated_plot,  plt_total, width = PLOT_WIDTH, height = PLOT_HEIGHT,
       dpi = PLOT_DPI, device = "png")
ggsave(latest_plot, plt_total, width = PLOT_WIDTH, height = PLOT_HEIGHT,
       dpi = PLOT_DPI, device = "png")

validate_output_file(dated_plot)
validate_output_file(latest_plot)

message("Done.")
