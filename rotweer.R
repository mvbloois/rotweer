library(tidyverse)
library(lubridate)
library(janitor)
library(patchwork)
library(slider)
library(showtext)


# Download and read file
download.file(
  "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_344.zip",
  "./data/etmgeg_344.zip"
)

rt_raw <-
  read_delim("data/etmgeg_344.zip", delim = ",", skip = 50) %>%
  clean_names()

max_date <- ymd(max(rt_raw$yyyymmdd))

last_day <- if_else(max_date == ceiling_date(max_date, unit = "month") - days(1), max_date, floor_date(max_date, unit = "month") - days(1))

# fonts
font_add_google(name = "Titillium Web", family = "titillium")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# colours

c_average <- "#d5dcdc"
c_hot <- "#CE0000"
c_cold <- "#000063"
c_rain <- "#1e7df0"
c_sun <- "#e1ad01"
c_lines <- "#548c8c"
c_text <- "#ededed"
c_background <- "#2f4f4f"

## ggplot theme
theme_set(theme_minimal())
          
theme_update(text = element_text(family = "titillium",
                                 colour = c_text),
             axis.line = element_blank(),
             panel.grid.major = element_line(colour = c_lines),
             panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             axis.text.x = element_text(colour = c_text),
             axis.text.y = element_text(colour = c_text),
             plot.title = element_text(hjust = 0.5),
             plot.title.position = "plot",
             plot.background = element_rect(fill = c_background,
                                            colour = c_background)
  )

# Base data.frame
rt_base <- rt_raw %>%
  filter(yyyymmdd > 19900000) %>%
  select(
    date = yyyymmdd,
    temp = tg,
    rain = rh,
    sun = sq
  ) %>%
  mutate(
    date = lubridate::ymd(date),
    temp = parse_number(temp),
    rain = parse_integer(rain),
    sun = parse_integer(sun),
    temp = temp / 10,
    rain = rain / 10,
    sun = sun / 10,
    doy = format(date, "%m-%d")
  ) %>%
  mutate(mov_30d_mean_temp = slide_dbl(temp,
                                       mean,
                                       .before = 29,
                                       .complete = TRUE)) %>%
  drop_na() %>% 
  filter(date <= last_day)

axis_day <- seq.Date(from = last_day - years(1) - months(1) + days(1),
                     to = last_day,
                     by = "month")
axis_month <- axis_day %>% 
  format("%m-%Y")
axis_day <- axis_day + days(14)

# Daily data
rt_temp_lastyear <- rt_base %>%
  filter(date >= max(date) - years(1) - months(1) + days(1)) %>%
  select(date, doy, temp)

rt_temp_ref <- rt_base %>%
  filter(year(date) %in% seq(1991, 2020, 1)) %>%
  select(doy, mov_30d_mean_temp, temp) %>%
  # 30-day moving mean for rain
  group_by(doy) %>%
  summarise(mov_temp = mean(mov_30d_mean_temp),
            .groups = "drop")

rt_temp <- rt_temp_ref %>%
  inner_join(rt_temp_lastyear, by = "doy") %>%
  mutate(hotness = ifelse(temp > mov_temp, "hot", "cold"),
         doy = fct_reorder(doy, date))

plt_1 <- rt_temp %>%
  ggplot() +
  geom_segment(
    aes(
      x = date,
      xend = date,
      y = temp,
      yend = mov_temp,
      colour = hotness
    ),
    size = 1.5,
    show.legend = FALSE
  ) +
  geom_line(aes(x = date, y = mov_temp, group = 1),
            colour = c_average,
            size = 1) +
  scale_x_date(breaks = axis_day, labels = axis_month) +
  scale_y_continuous(labels = scales::number_format(suffix = " °C")) +
  scale_colour_manual(values = c("hot" = c_hot, "cold" = c_cold)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Daily mean temperature"
  )


# Monthly data
tbl_2 <- inner_join(
  rt_base %>%
    group_by(yr = year(date), mth = month(date)) %>%
    summarise(sum_rain = sum(rain),
              sum_sun = sum(sun),
              .groups = "drop") %>%
    filter(yr %in% seq(1991, 2020, 1)) %>%
    group_by(mth) %>%
    summarise(mean_mth_rain = mean(sum_rain),
              mean_mth_sun = mean(sum_sun),
              .groups = "drop")
,
  rt_base %>%
    filter(date <= last_day) %>%
    filter(date >= max(date) - years(1) - months(1) + days(1)) %>%
    group_by(yr = year(date), mth = month(date)) %>%
    summarise(sum_rain = sum(rain),
              sum_sun = sum(sun),
              .groups = "drop") %>%
    mutate(row = row_number())
  ,
  by = "mth"
) %>% 
  mutate(x_label = format(as.Date(paste(yr, mth, 1, sep = "-")), "%m-%Y"))

plt_2 <- tbl_2 %>%
  ggplot(aes(x = fct_reorder(x_label, row), y = sum_rain)) +
  geom_col(fill = c_rain) +
  geom_segment(
    aes(
      x = row - 0.2,
      xend = row + 0.2,
      y = mean_mth_rain,
      yend = mean_mth_rain
    ),
    colour = c_average,
    size = 1.5
  ) +
  geom_segment(
    aes(
      x = row,
      xend = row,
      y = mean_mth_rain,
      yend = sum_rain
    ),
    linetype = 3,
    colour = c_average
  ) +
  scale_y_continuous(labels = scales::number_format(suffix = " mm")) +
  labs(
    x = NULL,
    y = NULL,
    title = "Total monthly amount of rainfall"
  ) 


plt_3 <- tbl_2 %>%
  ggplot(aes(x = fct_reorder(x_label, row), y = sum_sun)) +
  geom_col(fill = c_sun) +
  geom_segment(
    aes(
      x = row - 0.2,
      xend = row + 0.2,
      y = mean_mth_sun,
      yend = mean_mth_sun
    ),
    colour = c_average,
    size = 1.5
  ) +
  geom_segment(
    aes(
      x = row,
      xend = row,
      y = mean_mth_sun,
      yend = sum_sun
    ),
    linetype = 3,
    colour = c_average
  ) +
  scale_y_continuous(labels = scales::number_format(suffix = " h")) +
  labs(
    x = NULL,
    y = NULL,
    title = "Total monthly amount of sunshine"
    ) 


plt_total <- plt_1 / plt_2 / plt_3 +
  plot_annotation(
    title = paste("Yearly Rotterdam weather summary up to", last_day,
                  "(compared to 1991-2020)"),
    caption = "Source: KNMI",
    theme = theme(plot.title = element_text(size = 20))
  )

ggsave(paste0("plots/rotweer_", last_day, ".png"),
       plt_total,
       width = 10, height = 10, dpi = 300,
       device = "png")
