library(ggplot2)

View(cycling_per_hour_during_strike)

p <- 
  ggplot(cycling_per_hour_during_strike, aes(x = time, y = count_daily_avg)) +
  geom_col() +
  geom_col(aes(y = count_daily_avg_yoy, color = 'yoy')) +
  facet_wrap(~as.factor(wday))


ggsave("paris-strike/plots/cycling_per_hour.png", width = 40, height = 22.5, units = "cm")

# overal numbers
cycling_during_strike_per_day %>%
  dplyr::filter(is_during_strike == 1) %>%
  dplyr::summarise(
      count_daily = sum(count_daily, na.rm = T), 
      count_mom = sum(count_mom, na.rm = T), 
      count_yoy = sum(count_yoy, na.rm = T),
      diff_mom = round(count_daily / count_mom - 1, 2),
      diff_yoy = round(count_daily / count_yoy - 1, 2)
    ) 
# overal numbers per weekend,weekdays
cycling_during_strike_per_day %>%
  dplyr::filter(is_during_strike == 1) %>%
  dplyr::group_by(is_weekend) %>%
  dplyr::summarise(
    count_daily = sum(count_daily, na.rm = T), 
    count_mom = sum(count_mom, na.rm = T), 
    count_yoy = sum(count_yoy, na.rm = T),
    diff_mom = round(count_daily / count_mom - 1, 2),
    diff_yoy = round(count_daily / count_yoy - 1, 2)
  ) 
