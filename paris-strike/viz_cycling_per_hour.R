library(ggplot2)

View(cycling_per_hour_during_strike)
viz_hourly <- function(d){
  ggplot(d, aes(x = time, y = count_daily_avg)) +
    geom_col(aes(fill = 'Nb of cyclist')) +
    geom_col(aes(y = count_daily_avg_yoy, fill = 'Nb of cyclist: Last year'), alpha = 0.95) +
    scale_fill_brewer(palette = 'Dark2') +
    labs(fill="Series", y = 'Nb of cyclist') +
    scale_x_discrete(
      breaks = c("02:00:00","05:00:00","08:00:00","11:00:00","14:00:00","17:00:00","20:00:00","23:00:00"),
      labels = c("02:00","05:00","08:00","11:00","14:00","17:00","20:00","23:00")
    ) +
    theme(
      axis.text.x = element_text(angle=90),
      legend.position = "bottom"
      )
}

p <- viz_hourly(dplyr::filter(cycling_per_hour_during_strike)) +
  facet_wrap(~as.factor(wday), labeller = label_wrap_gen(multi_line=FALSE))

p
ggsave("paris-strike/plots/cycling_per_hour.png", width = 40, height = 22.5, units = "cm")


p2 <- viz_hourly(cycling_per_hour_during_strike_weekend_vs_week) +
  facet_wrap(~as.factor(label))

p2
ggsave("paris-strike/plots/cycling_per_hour_weekend_vs_week.png", width = 40, height = 22.5, units = "cm")

# overal numbers per weekend,weekdays
cycling_per_hour_during_strike %>%
  dplyr::group_by(is_weekend, time) %>%
  dplyr::summarise(
    count_daily_avg = sum(count_daily_avg) / n_distinct(wday),
    count_daily_avg_yoy = sum(count_daily_avg_yoy) / n_distinct(wday),
    diff_yoy = round(count_daily_avg / count_daily_avg_yoy - 1, 2)
  ) %>% View()

# overal numbers per weekend,weekdays
cycling_per_hour_during_strike %>%
  dplyr::group_by(wday, time) %>%
  dplyr::summarise(
    count_daily_avg = sum(count_daily_avg) / n_distinct(wday),
    count_daily_avg_yoy = sum(count_daily_avg_yoy) / n_distinct(wday),
    diff_yoy = round(count_daily_avg / count_daily_avg_yoy - 1, 2)
  ) %>% View()
