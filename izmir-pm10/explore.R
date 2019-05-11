# install if not already installed in order to read xls
library(readxl)
# install if not already installed in order to reshape data
library(tidyr)
# install if not already installed in order to play with timestamps
library(lubridate)
# install if not already installed in order to make it simpler to manipulate
library(dplyr)
# install if not already installed in order to make it simpler to manipulate
library(ggplot2)


raw_data <- readxl::read_xls("datane.xls", sheet = 1, 
                             col_types = c("date", "numeric", "numeric", "numeric"))

names(raw_data) <- c("time", "izmir_alsancak", "izmir_bayrakli", "izmir_bornova")
str(raw_data)

# instead of having 3 different columns for pm10, keep it in just once 
# and create new colum for location
normalized_raw_data  <- tidyr::gather(raw_data, 
                                      key = location, 
                                      value = "pm10", 
                                      izmir_alsancak:izmir_bornova,
                                      na.rm = T)
str(normalized_raw_data)

data_detailed <- normalized_raw_data %>%
  dplyr::mutate(
    year = lubridate::year(time),
    month = lubridate::month(time, label = T),
    date = lubridate::date(time),
    hour = lubridate::hour(time),
    wday = lubridate::wday(time, label= T, week_start = 1, abbr = F)
  )

str(data_detailed)

# see the global distribution of pm10
ggplot(data_detailed, aes(x = pm10)) +
  geom_histogram(binwidth = 5)

# see the distribution of pm10 per location
ggplot(data_detailed, aes(x = pm10, fill = location)) +
  geom_histogram(binwidth = 5, alpha = 0.9)

# see the distribution of pm10 per location seperately
ggplot(data_detailed, aes(x = pm10, fill = location)) +
  geom_histogram(binwidth = 5, alpha = 0.9) +
  facet_wrap(~location, ncol = 1)

# see the density of pm10 per month seperately
ggplot(data_detailed, aes(x = pm10, fill = location)) +
  geom_density(alpha = 0.9, size = 0) +
  facet_wrap(~month, ncol = 1)

# see the density of pm10 per weekday seperately
ggplot(data_detailed, aes(x = pm10)) +
  geom_density(alpha = 0.9) +
  facet_wrap(~wday, ncol = 1)

# is it changing depending on the hour of the day
ggplot(data_detailed, aes(x = hour, y = pm10)) +
  geom_jitter(alpha = 0.5, height = 0.1, width = 0.3, size = 1) +
  geom_smooth(method = "auto") +
  ylim(0, 400)
