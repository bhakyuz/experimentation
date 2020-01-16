library(ggplot2)

View(cycling_during_strike_per_day)

p <- 
  ggplot(cycling_during_strike_per_day, aes(x = date, y = count_daily)) +
  geom_col(aes(y=is_weekend * max(count_daily)), color="cornsilk3", fill ="cornsilk3", width = 1, alpha = 0.5, size = 0) +
  geom_vline(xintercept = lubridate::date(strike_start), color = 'cornsilk4') +
  geom_line(aes(y = count_daily, colour = "Nb of cyclist")) +
  geom_line(aes(y = count_mom, colour = "Nb of cyclist: Last month")) +
  geom_line(aes(y = count_yoy, colour = "Nb of cyclist: Last year")) +
  geom_text(aes(y = count_daily, colour = "Nb of cyclist", label = count_daily, hjust = 0, vjust = 0),  check_overlap = T) +
  # geom_text(aes(y = count_daily, colour = "Nb of cyclist: Last year", label = diff_label_yoy, hjust = 0, vjust = 0)) +
  annotate("text", x = lubridate::date(strike_start), y = Inf, label = "Beginning of strike", vjust = 2, hjust = -0.05, size = 4) +
  # scale_fill_grey() +
  scale_colour_brewer(palette = 'Dark2') +
  labs(colour="Series", y = 'Nb of cyclist')
p

ggsave("paris-strike/plots/cycling_per_day.png", width = 40, height = 22.5, units = "cm")

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
