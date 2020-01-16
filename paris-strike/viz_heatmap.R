library(ggplot2)
library(ggmap)

height <- max(cycling_during_strike_per_counter_location$location_latitude) - min(cycling_during_strike_per_counter_location$location_latitude)
width <- max(cycling_during_strike_per_counter_location$location_longitude) - min(cycling_during_strike_per_counter_location$location_longitude)
bb <- c(bottom  = min(cycling_during_strike_per_counter_location$location_latitude)  - 0.1 * height, 
                 top     = max(cycling_during_strike_per_counter_location$location_latitude)  + 0.1 * height,
                 left    = min(cycling_during_strike_per_counter_location$location_longitude) - 0.1 * width,
                 right   = max(cycling_during_strike_per_counter_location$location_longitude) + 0.1 * width)

cycling_repeated <- cycling_during_strike_per_counter_location %>%
  dplyr::mutate(
    x = list(1:round(count_daily_avg/300))
  ) %>%
  tidyr::unnest(x)

cycling_fairly_dist <-cycling_during_strike_per_counter_location %>%
  dplyr::mutate(
    location_latitude = round(location_latitude, 2),
    location_longitude = round(location_longitude, 2)
  ) %>%
  dplyr::group_by(location_latitude, location_longitude) %>%
  dplyr::summarise(
    count_daily_avg = max(count_daily_avg)
    ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x = list(1:ceiling(count_daily_avg/200))
    ) %>%
  tidyr::unnest(x)

m <- get_stamenmap(bb, maptype = 'toner', zoom = 12)
ggmap(m) 

ggmap(m) +
  stat_density2d(data = cycling_fairly_dist, 
                 aes(x = location_longitude, y = location_latitude, fill = ..level..), size = 0.01, alpha = 0.05,
                 bins = 100, geom = "polygon") +
  scale_fill_distiller(palette = 'Spectral', -1) +
  geom_point(data = cycling_during_strike_per_counter_location, 
           aes(x = location_longitude, y = location_latitude, 
               size = round(cycling_during_strike_per_counter_location$count_daily_avg/1000),
               shape = 24
               ), color ='black', fill = 'paleturquoise1'
           ) +
  scale_shape_identity() +
  theme(legend.position = "none")

ggsave("paris-strike/plots/counters_on_map.png", width = 40, height = 22.5, units = "cm")

cycling_during_strike_per_counter_location %>%
  dplyr::arrange(dplyr::desc(count_daily_avg)) 
