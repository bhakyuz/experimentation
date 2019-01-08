source('get_data.R')
source('prepare.R')


# plot density of all numeric columns
training_numeric_histogram <- data.train.complete %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density()

# ggsave("images/training_numeric_histogram.png", plot = training_numeric_histogram,  width = 40, height = 30, units = "cm")

# plot density of all date columns
training_date_histogram <- data.train.complete %>%
  select_if(is.Date) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density()

# ggsave("images/training_date_histogram.png", plot = training_date_histogram,  width = 40, height = 30, units = "cm")

training_categorical_histogram <- data.train.complete %>%
  select_if(is.factor) %>%
  mutate_if(is.factor, as.character) %>%
  gather(na.rm = T) %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_bar()

# ggsave("images/training_categorical_histogram.png", plot = training_categorical_histogram,  width = 40, height = 30, units = "cm")

# take loong
training_pairs <- data.train %>%
  select_if(is.numeric) %>%
  GGally::ggpairs()

# ggsave("images/training_pairs.png", plot = training_pairs,  width = 40, height = 30, units = "cm")
