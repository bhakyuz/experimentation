# source('get_data.R')
source('prepare.R')

# explore historical data
data.train.historical.per_id <- data.train.historical %>% 
  group_by(id) %>% 
  summarise(
    n_distinct = n_distinct(price_date), 
    n = n(),
    price_p1_distinct = n_distinct(price_p1_var),
    price_p2_distinct = n_distinct(price_p2_var),
    price_p3_distinct = n_distinct(price_p3_var),
    price_p1_fix_distinct = n_distinct(price_p1_fix),
    price_p2_fix_distinct = n_distinct(price_p2_fix),
    price_p3_fix_distinct = n_distinct(price_p3_fix)
  )  %>% 
  ungroup() %>% rowwise() %>%
  mutate(
    price_distinct = max(price_p1_distinct, price_p2_distinct, price_p3_distinct,
                         price_p1_fix_distinct, price_p2_fix_distinct, price_p3_fix_distinct)
    )

# missing values in historical rates
data.train.historical %>% complete.cases() %>% sum() / nrow(data.train.historical) * 100
# 0.8% missing, should be omitted
# what percentage of the user have actually how many unique price
table(data.train.historical.per_id$price_distinct) / nrow(data.train.historical.per_id) * 100
# only 4% of the clients has more 5 unique prices.
# so find the avg. price on quarter based

# plot density of all numeric columns to see the distribution
# so many 0 values does it mean they are NA
training_historical_numeric_histogram <- data.train.historical %>%
  select_if(is.numeric) %>%
  gather() %>%
  mutate(key = factor(key, levels = unique(key))) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()

# ggsave("images/training_historical_numeric_histogram.png", plot = training_historical_numeric_histogram, width = 40, height = 30, units = "cm")

# which percentage on each var there are zeroes
data.train.historical %>%
  select_if(is.numeric) %>%
  mutate_all(funs(ifelse(. == 0, NA, .)) ) %>% 
  summarise_all(funs(sum(is.na(.)) / length(.) * 100) )
# price_p2_var 45% is 0 
# price_p3_var price_p2_fix price_p3_fix 58 % is 0
# so treat 0 as NAs

# TODO might be interesting to check whether the change in price in 2015, 
# (increase or decrease) has an impact on churn 

# which variables to drop
mostly_na <- sapply(data.train.complete, function(x) sum(is.na(x)) / length(x) )
vars_to_drop <- mostly_na[mostly_na > 0.4]
names(vars_to_drop)
data_frame(var = names(mostly_na), type = sapply(data.train.complete, class), na_rate = mostly_na) %>% View()
# data_frame(var = names(mostly_na), type = sapply(data.train.complete, class), na_rate = mostly_na) %>% write_csv(path = "na_rates.csv")
# complete cases 
data.train.complete %>% complete.cases() %>% sum() / nrow(data.train.complete)
data.test.complete %>% complete.cases() %>% sum() / nrow(data.test.complete)

correlations <- data.train.train %>%
  mutate_if(is.logical, as.numeric) %>%
  select_if(is.numeric) %>%
  na.omit() %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  tibble::rownames_to_column(var = "var1") %>%
  gather(var2, value, -var1)


correlations %>% 
  filter(var1 != var2) %>%
  # filter(value > 0.5 | value < -0.5 ) %>%
  arrange(var1, desc(value)) %>%
  write_csv(path = "correlations.csv")

data.train.complete %>% 
  select_if(is.factor) %>% sapply(levels)

longer <- data.train.complete %>%
  mutate(churn = as.numeric(churn)) %>%
  mutate_if(~is.factor(.)|is.Date(.)|is.logical(.), as.numeric ) %>%
  select_if(~is.numeric(.x)|is.character(.) ) %>%
  gather(key, value, -id, -churn) %>%
  na.omit()


varnames <- longer$key %>% unique()

# do not run unless necessary
if (1 == 0 ) {
  for (i in 1:length(varnames)) {
    # for (i in 1:1) {
    cur_var <- varnames[i]
    print(cur_var)
    
    filtered <- longer %>% 
      filter(key  == cur_var) %>%
      left_join(select(longer, -churn), by = "id") %>%
      filter(key.x != key.y)
    
    to_p <- filtered %>%
      filter(key.x == cur_var)
    # %>% filter(key.y == "cons_gas_12m") 
    p <- to_p %>% na.omit() %>%
      arrange(churn) %>%
      mutate(churn = as.factor(churn)) %>%
      ggplot(aes(x = value.y, y = value.x , color = churn)) + 
      facet_wrap( vars(key.x , key.y), scales = "free") + 
      geom_jitter(alpha = 0.2, width = 0.2, height = 0.2)
    
    ggsave(paste("images/pairwise_", cur_var,".png", sep = ""), plot = p,  width = 40, height = 30, units = "cm")
    
  }
  
}

# Data for prez
nrow(data.train.complete)
# who left
table(data.train.complete$churn)/sum(table(data.train.complete$churn))
# what is the loss
churn_clients <- data.train.complete %>% filter(churn == 1) 
loss_margin <- data.train.complete %>% filter(churn == 1) %>% pull(net_margin) 

yearly_net_margin <- sum(data.train.complete$net_margin / data.train.complete$num_years_antig)

yearly_loss_margin <- sum(churn_clients$net_margin / churn_clients$num_years_antig)
sum(loss_margin)
sum(loss_margin) / length(loss_margin)
yearly_loss_margin / nrow(churn_clients)

# gross expected margin
gross_margin <- data.train.complete %>% pull(net_margin) %>% sum()
sum(loss_margin) / gross_margin
# which origin has more churn
data.train.complete %>% group_by(origin_up) %>% 
  summarise(churn = sum(churn), n = n()) %>% mutate(ratio = churn / n) %>% View()

