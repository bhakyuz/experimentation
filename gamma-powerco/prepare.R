source('get_data.R')
library(lubridate)
library(caret)

prepare <- function(data, data.historical){
  
  # find avg per quarter, id 
  data.historical.quarterly <- data.historical %>% 
    mutate_if(is.numeric, funs(ifelse(. == 0, NA, .)) ) %>%
    mutate(quarter = paste(year(price_date), (quarter(price_date) - 1) * 3 + 1, 1, sep = "-"),
           quarter = ymd(quarter)) %>%
    group_by(id, quarter) %>%
    summarise_if(is.numeric, mean, na.rm = T) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(ifelse(is.nan(.), NA, .)) )
  
  # long table into wide table eg one row per id
  data.historical.per.id <- data.historical.quarterly %>%
    gather(key, value, -id, -quarter) %>% 
    mutate(key = paste(key, quarter) %>% make.names()) %>%
    select(id, key, value) %>%
    spread(key, value)
  
  data.historical.per.id <- data.historical.per.id %>% 
    rowwise() %>%
    mutate(
      price_fix_change = mean(c(
        price_p1_fix.2015.04.01 / price_p1_fix.2015.01.01,
        price_p1_fix.2015.07.01 / price_p1_fix.2015.04.01,
        price_p1_fix.2015.10.01 / price_p1_fix.2015.07.01,
        price_p2_fix.2015.04.01 / price_p2_fix.2015.01.01,
        price_p2_fix.2015.07.01 / price_p2_fix.2015.04.01,
        price_p2_fix.2015.10.01 / price_p2_fix.2015.07.01,
        price_p3_fix.2015.04.01 / price_p3_fix.2015.01.01,
        price_p3_fix.2015.07.01 / price_p3_fix.2015.04.01,
        price_p3_fix.2015.10.01 / price_p3_fix.2015.07.01),
        na.rm = T
      ) - 1,
      price_var_change = mean(c(
        price_p1_var.2015.04.01 / price_p1_var.2015.01.01,
        price_p1_var.2015.07.01 / price_p1_var.2015.04.01,
        price_p1_var.2015.10.01 / price_p1_var.2015.07.01,
        price_p2_var.2015.04.01 / price_p2_var.2015.01.01,
        price_p2_var.2015.07.01 / price_p2_var.2015.04.01,
        price_p2_var.2015.10.01 / price_p2_var.2015.07.01,
        price_p3_var.2015.04.01 / price_p3_var.2015.01.01,
        price_p3_var.2015.07.01 / price_p3_var.2015.04.01,
        price_p3_var.2015.10.01 / price_p3_var.2015.07.01), 
        na.rm = T
        ) - 1,
      # data might not be so correct # change 5800% in few cases
      price_fix_change = ifelse(price_fix_change < -0.25, -0.25, price_fix_change), 
      price_fix_change = ifelse(price_fix_change > 0.25, 0.25, price_fix_change), 
      price_var_change = ifelse(price_var_change < -0.25, -0.25, price_var_change), 
      price_var_change = ifelse(price_var_change > 0.25, 0.25, price_var_change)
      ) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)) ) 

  final <- data %>%
    mutate(
      activity_new = as.factor(activity_new),
      campaign_disc_ele = as.factor(campaign_disc_ele),
      channel_sales = as.factor(channel_sales),
      has_gas = ifelse(has_gas == "f", F, T),
      origin_up = as.factor(origin_up)
    ) %>% 
    left_join(data.historical.per.id, by = "id") %>%
    mutate_if(is.factor, 
              funs(ifelse(is.na(.), "missing", as.character(.))) ) %>%
    mutate(
      activity_new = as.factor(activity_new),
      campaign_disc_ele = as.factor(campaign_disc_ele),
      channel_sales = as.factor(channel_sales),
      has_gas = as.factor(has_gas),
      origin_up = as.factor(origin_up)
      ) %>%
    mutate(
      cons_12m = ifelse(cons_12m < 0, 0, cons_12m),
      cons_gas_12m = ifelse(cons_gas_12m < 0, 0, cons_gas_12m),
      cons_last_month = ifelse(cons_last_month < 0, 0, cons_last_month),
      forecast_cons_12m = ifelse(forecast_cons_12m < 0, 0, forecast_cons_12m),
      forecast_cons_year = ifelse(forecast_cons_year < 0, 0, forecast_cons_year),
      forecast_meter_rent_12m = ifelse(forecast_meter_rent_12m < 0, 0, forecast_meter_rent_12m),
      imp_cons = ifelse(imp_cons < 0, 0, imp_cons)
    ) %>%
    # handling NAs
    mutate(
      date_first_activ = ifelse(is.na(date_first_activ), 
                                ymd("2016-01-01") - 365 * num_years_antig,
                                date_first_activ
                                ),
      date_first_activ = lubridate::as_date(date_first_activ)
    ) %>% 
    mutate_if(~is.Date(.), 
              funs(coalesce(., mean(., na.rm = T)))) %>% 
    mutate_if(~is.numeric(.), 
              funs(coalesce(.*1.0, mean(., na.rm = T))))
  
  if ("churn" %in% names(final)) {
    final <- final %>%
      mutate(
        churn = as.logical(churn)
      )
  }
  
  # based on NA exploration
  vars_to_drop <- c("forecast_base_bill_ele", "forecast_base_bill_year", "forecast_bill_12m", 
                    "forecast_cons", "campaign_disc_ele")
  vars_to_keep <- names(final)[! names(final) %in% vars_to_drop]
  
  final <- subset(final, select = vars_to_keep)
  return(final)
}

data.train.complete <- prepare(
  data = data.train, 
  data.historical = data.train.historical)

data.test.complete <- prepare(
  data = data.test, 
  data.historical = data.test.historical)


set.seed(126)
data.train.idx <- caret::createDataPartition(y = data.train.complete$churn, 
                                             list = F, p = 0.6)
data.train.train <- data.train.complete[data.train.idx, ]
data.train.test <- data.train.complete[-data.train.idx, ]
data.train.test1 <- data.train.test[1:round(nrow(data.train.test)/2), ]
data.train.test2 <- data.train.test[-c(1:round(nrow(data.train.test)/2)), ]

remove(data.train.idx)
remove(data.test.historical, data.train, 
       data.train.output, data.train.historical, data.train.raw)
