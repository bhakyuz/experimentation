setwd(here::here())
library(ggplot2)
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
  dplyr::group_by(date) %>%
  summarise(
    count_daily = sum(count_daily, na.rm = T), 
    count_mom = sum(count_mom, na.rm = T), 
    count_yoy = sum(count_yoy, na.rm = T)
    )


ggplot(cycling_during_strike_per_day, aes(x = date, y = count_daily)) +
  geom_area()
