setwd(here::here())
source(file = "paris-strike/get_cycling_data.R")

strike_start <- lubridate::ymd_hms("2019-12-05 04:00:00")
strike_end <- lubridate::now() # still goes on

cycling_during_strike <- cycling_daily %>%
  dplyr::filter(
    date >= strike_start - lubridate::ddays(5), # see data starting from a bit earlier
    date <= strike_end
    ) %>%
  dplyr::mutate(
    is_during_strike = if_else(date >= strike_start - lubridate::ddays(1) & date <= strike_end, 1, 0),
    date_yoy = date - lubridate::dyears(1),
    date_mom = date - lubridate::dweeks(4),
  )
  
cycling_during_strike <- cycling_during_strike %>%
  dplyr::left_join(
    dplyr::select(cycling_daily, location_id, date, count_mom = count_daily), 
    by = c("location_id", "date_mom" = "date")) %>%
  dplyr::left_join(
    dplyr::select(cycling_daily, location_id, date, count_yoy = count_daily), 
    by = c("location_id", "date_yoy" = "date")) 

cycling_during_strike_per_day <- cycling_during_strike %>% 
  dplyr::group_by(date, is_weekend, is_during_strike) %>%
  summarise(
    count_daily = sum(count_daily, na.rm = T), 
    count_mom = sum(count_mom, na.rm = T), 
    count_yoy = sum(count_yoy, na.rm = T),
    diff_mom = round(count_daily / count_mom - 1, 2),
    diff_yoy = round(count_daily / count_yoy - 1, 2)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    diff_label_mom = paste0('%',round(diff_mom * 100)),
    diff_label_yoy = paste0('%',round(diff_yoy * 100))
  )
head(cycling_during_strike_per_day)

cycling_per_hour_during_strike <- cycling %>%
  dplyr::filter(
    date >= strike_start - lubridate::ddays(1),
    date <= strike_end - lubridate::ddays(1)
  ) %>%
  dplyr::group_by(
    wday,
    time,
    is_weekend
  ) %>%
  dplyr::summarise(
    count_daily_avg = sum(count_hourly) / n_distinct(date)
  ) %>%
  dplyr::ungroup()

cycling_per_hour_during_strike_yoy <- cycling %>%
  dplyr::filter(
    date >= strike_start - lubridate::ddays(1) - lubridate::dyears(1) ,
    date <= strike_end - lubridate::ddays(1) - lubridate::dyears(1) 
  ) %>%
  dplyr::group_by(
    wday,
    time,
    is_weekend
  ) %>%
  dplyr::summarise(
    count_daily_avg = sum(count_hourly) / n_distinct(date)
  ) %>%
  dplyr::ungroup()

cycling_per_hour_during_strike <-
  cycling_per_hour_during_strike %>% 
  dplyr::left_join(
    dplyr::select(cycling_per_hour_during_strike_yoy, wday, time, is_weekend, count_daily_avg_yoy = count_daily_avg), 
    by = c('wday','time', 'is_weekend')
  ) 

cycling_per_hour_during_strike_weekend_vs_week <-
  cycling_per_hour_during_strike %>% 
  dplyr::group_by(
    time,
    is_weekend
  ) %>%
  dplyr::summarise(
    count_daily_avg = sum(count_daily_avg) / n_distinct(wday),
    count_daily_avg_yoy = sum(count_daily_avg_yoy) / n_distinct(wday)
  ) %>%
  dplyr::mutate(
    label = factor(is_weekend, levels = 0:1, labels = c('Weekdays', 'Weekend'))
  ) %>%
  dplyr::ungroup()
