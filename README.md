# rotweer

The weather in Rotterdam in perspective.

Fun project to hone my R skills. Inspired by the data visualisations by
[Neil Kaye](https://twitter.com/neilrkaye).

> I do not know anything about meteorological data. This is just for fun.

![Latest plot](https://raw.githubusercontent.com/mvbloois/rotweer/main/plots/rotweer_latest.png)

---

## What it does

`rotweer.R` downloads the official daily weather data for **KNMI station 344
(Rotterdam)** and produces a three-panel chart showing:

| Panel | Metric | Unit |
|-------|--------|------|
| Top   | Daily mean temperature | °C |
| Middle | Total monthly rainfall | mm |
| Bottom | Total monthly sunshine duration | h |

Each panel compares the most recent ~13 months against a **30-year historical
baseline** (ending two full years before the latest data point, to avoid
overlapping the display window).

---

## Data source

Data are downloaded from the
[KNMI Climate Data Portal](https://www.knmi.nl/nederland-nu/klimatologie/daggegevens).

* **Station**: 344 – Rotterdam
* **File format**: ZIP-compressed comma-delimited text with a 50-line header
* **Relevant columns**: `YYYYMMDD`, `TG` (temperature, 0.1 °C), `RH`
  (precipitation, 0.1 mm), `SQ` (sunshine, 0.1 h)
* Values of `-1` indicate a trace measurement (< 0.05 in the respective unit)
  and are treated as zero after unit conversion.

---

## Baseline methodology

1. The script computes `last_day`: the most recent complete month-end in the
   downloaded data.  If KNMI has published only a partial current month, the
   display window ends at the previous full month-end.
2. `first_day` is the first day of the calendar month approximately one year
   before `last_day`, giving a ~13-month display window.
3. The baseline covers the 30 calendar years ending two years before
   `last_day` (e.g. if `last_day` is 2024-04-30, the baseline is 1993–2022).
4. Temperature comparison uses a 30-day centred moving mean (±14 days) of the
   baseline daily temperatures, grouped by calendar day-of-year.
5. Rainfall and sunshine comparisons use the mean of monthly totals across all
   30 baseline years for each calendar month.

---

## Update cadence

A **GitHub Actions** workflow (`main.yml`) runs automatically at **05:00 UTC
on the 8th of every month** – shortly after KNMI publishes the previous
month's complete data.  The workflow:

1. Downloads fresh KNMI data.
2. Runs the unit-test suite (`tests/run_tests.R`).  The script will not run if
   any test fails.
3. Executes `rotweer.R` to regenerate the charts.
4. Commits and pushes only the updated files in `data/` and `plots/`.

The workflow can also be triggered manually via **Actions → Monthly update
Rotweer → Run workflow**.

---

## Generated files

| Path | What it is | Why it is committed |
|------|------------|---------------------|
| `data/etmgeg_344.zip` | Latest KNMI data download | Reproducible re-runs without re-downloading |
| `plots/rotweer_YYYY-MM-DD.png` | Dated chart snapshot | Permanent record of each monthly update |
| `plots/rotweer_latest.png` | Always the most recent chart | Stable URL for the README badge |

---

## Development & Troubleshooting

### Run locally

```bash
# From the project root
Rscript rotweer.R
```

Required R packages (see `DESCRIPTION`): `tidyverse`, `lubridate`, `janitor`,
`slider`, `showtext`, `patchwork`.

Required system libraries (Ubuntu/Debian):
```bash
sudo apt-get install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev
```

### Run tests

```bash
Rscript tests/run_tests.R
```

### Common failure causes

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| `Download failed` | Network issue or KNMI URL change | Check connectivity; verify URL in `DATA_URL` constant |
| `Required columns missing` | KNMI changed their CSV column names | Update `KNMI_REQUIRED_COLS` and column rename in `select()` |
| `Dataset is unexpectedly empty` | Date filter too restrictive or join found no matching days-of-year | Check `last_day` and `base_years` values in the log output |
| `Expected output file was not created` | `ggsave` silently failed (e.g. missing font) | Check for ggplot/showtext errors above in the log |
| Font download fails in CI | `font_add_google` needs internet access | Ensure the runner has outbound HTTPS access |
