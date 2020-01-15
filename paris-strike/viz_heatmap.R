library(ggplot2)
library(ggmap)

height <- max(cycling_during_strike_per_counter_location$location_latitude) - min(cycling_during_strike_per_counter_location$location_latitude)
width <- max(cycling_during_strike_per_counter_location$location_longitude) - min(cycling_during_strike_per_counter_location$location_longitude)
bb <- c(bottom  = min(cycling_during_strike_per_counter_location$location_latitude)  - 0.1 * height, 
                 top     = max(cycling_during_strike_per_counter_location$location_latitude)  + 0.1 * height,
                 left    = min(cycling_during_strike_per_counter_location$location_longitude) - 0.1 * width,
                 right   = max(cycling_during_strike_per_counter_location$location_longitude) + 0.1 * width)

cycling_repeated <-cycling_during_strike_per_counter_location %>%
  dplyr::mutate(
    x = list(1:round(count_daily_avg/300))
  ) %>%
  tidyr::unnest(x)

m <- get_stamenmap(bb, maptype = 'toner', zoom = 13)
ggmap(m) 

ggmap(m) +
  stat_density2d(data = cycling_repeated, 
                 aes(x = location_longitude, y = location_latitude, fill = ..level..), size = 0.01, alpha = 0.04,
                 bins = 100, geom = "polygon") +
  scale_fill_distiller(palette = 'Spectral', -1) +
  geom_point(data = cycling_during_strike_per_counter_location, 
           aes(x = location_longitude, y = location_latitude, 
               color = round(cycling_during_strike_per_counter_location$count_daily_avg/1000), 
               size = count_daily_avg*10000)
           ) +
  scale_alpha()
#  scale_color_distiller(palette = 'OrRd', direction = -1)
