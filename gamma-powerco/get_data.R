library(readr)
library(dplyr)
library(tidyr)
library(caret)

data.train.raw <- read_csv("data/ml_case_training_data.csv", col_types = cols())
data.train.output <- read_csv("data/ml_case_training_output.csv", col_types = cols())
data.train.historical <- read_csv("data/ml_case_training_hist_data.csv", col_types = cols())
data.test <- read_csv("data/ml_case_test_data.csv", col_types = cols())
data.test.historical <- read_csv("data/ml_case_test_hist_data.csv", col_types = cols())

combine_data <- function(raw, output, historical){
  if(!missing(output)) {
    final <- raw %>% 
      left_join(output, by = "id")
  } else final <- raw 
  return(final)
}


data.train <- combine_data(raw = data.train.raw, output = data.train.output)

# remove(data.train.raw, data.train.output, data.train.historical)
