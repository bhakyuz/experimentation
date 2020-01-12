setwd(here::here())
library(ggplot2)
library(grid)
source(file = "paris-strike/get_cycling_data.R")

strike_start <- lubridate::ymd_hms("2019-12-05 04:00:00")
strike_end <- lubridate::now() # still goes on

cycling_during_strike <- cycling_daily %>%
  dplyr::filter(
    date >= strike_start - lubridate::ddays(5), # see data starting from a bit earlier
    date <= strike_end
    ) %>%
  dplyr::mutate(
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
  dplyr::group_by(date, is_weekend) %>%
  summarise(
    count_daily = sum(count_daily, na.rm = T), 
    count_mom = sum(count_mom, na.rm = T), 
    count_yoy = sum(count_yoy, na.rm = T)
    ) %>%
  dplyr::ungroup()


ggplot(cycling_during_strike_per_day, aes(x = date, y = count_daily)) +
  # change bg color when weekend
#  geom_col(aes(y=is_weekend * max(cycling_during_strike_per_day$count_daily)), fill="peachpuff2") +
  geom_col(aes(y=is_weekend * max(count_daily)), color="peachpuff2", fill ="peachpuff2", width = 1) +
  geom_vline(xintercept = lubridate::date(strike_start), color = 'lightseagreen') +
  geom_line() +
  annotate("text", x = lubridate::date(strike_start), y = Inf, label = "Beginning of strike", vjust = 2, hjust = -0.1, size = 4)
#  geom_tile(
  #    aes(fill = as.factor(is_weekend), y = 200000)
  #  ) 

ggplot(cycling_during_strike_per_day, aes(x = date, y = count_daily)) +
  geom_col(aes(y=is_weekend * max(count_daily)), fill="peachpuff2", linetype = 2)

weeks <- cycling_during_strike_per_day %>%
  mutate(week = lubridate::floor_date(date, 'week',  week_start = 1)) %>%
  group_by(week, is_weekend) %>%
  summarise(start = min(date), end = max(date)) %>%
  ungroup()

ggplot(cycling_during_strike_per_day, aes(x = date, y = count_daily)) +
  geom_tile(aes(y = is_weekend * 20000, fill=as.factor(is_weekend)), colour="peachpuff2")

ggplot(cycling_during_strike_per_day) +
  geom_rect(
    aes(xmin = start, xmax = end, fill = as.factor(is_weekend)), 
    ymin = -Inf, ymax = Inf, alpha = 0.2, 
  )

ggplot(cycling_during_strike_per_day) +
  geom_tile(
    aes(x = date, y = is_weekend * 20000)
  )

ggplot(cycling_during_strike_per_day) +
  geom_tile(
    aes(xmin = start, xmax = end, fill = as.factor(is_weekend)), 
    ymin = -Inf, ymax = Inf, alpha = 0.2, 
  )
# dplyr::group_by(cycling_during_strike_per_day, is_weekend, )
  
#  geom_smooth(method = 'loess')
#  geom_line(linejoin = 'round', lineend ='round')
