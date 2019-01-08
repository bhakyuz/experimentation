source('prepare.R')
library(e1071)
library(ROCR)

data.train.train <- data.train.train %>% 
  mutate(churn = as.numeric(churn)) %>%
  select(-contains("p2"), -contains("p3")) %>%
  select_if(~is.numeric(.)|is.Date(.)) 
# trying with less variable will have an effect ?
#  select(margin_net_pow_ele, margin_gross_pow_ele, date_activ, cons_12m,
#         forecast_meter_rent_12m, date_modif_prod, net_margin, forecast_cons_year,
#         date_end, price_var_change, cons_last_month, pow_max, price_fix_change,
#         churn)

data.train.complete <- data.train.complete %>% 
  mutate(churn = as.numeric(churn)) %>%
  select(-contains("p2"), -contains("p3")) %>%
  select_if(~is.numeric(.)|is.Date(.)) 

g <- 0.35
svm <- svm(as.numeric(churn) ~ . , data = data.train.train, 
           kernel = "radial", gamma = g)
# pr <- ROCR::prediction(fitted.results, data.train.train$churn)
pr <- ROCR::prediction(svm$fitted, data.train.train$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- ROCR::performance(pr, measure = "auc")
auc@y.values

# summary(svm$fitted)

svm2 <- svm(as.factor(churn) ~ . , data = data.train.train, 
           kernel = "radial", gamma = g, probability = T)

# test on training test
data.train.test1.res <- predict(svm, data.train.test1)
# data.train.test1.res_class <- predict(svm2, data.train.test1)
pr <- ROCR::prediction(data.train.test1.res, data.train.test1$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- ROCR::performance(pr, measure = "auc")
auc@y.values

# data.train.test1.res_class <- predict(svm2, data.train.test1)
pr <- ROCR::prediction(data.train.test2.res, data.train.test2$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- ROCR::performance(pr, measure = "auc")
auc@y.values

confusionMatrix(as.factor(data.train.test2.class), 
                reference =  as.factor(as.numeric(data.train.test2$churn)))

data.train.test2.class <- predict(svm2, data.train.test2, probability = T)

probs.train <- as.data.frame(attr(data.train.test2.class, "probabilities"))

# data.train.test1.res_class <- predict(svm2, data.train.test1)
pr <- ROCR::prediction(data.train.test.res, data.train.test$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- ROCR::performance(pr, measure = "auc")
auc@y.values


train_output <- data_frame(id = data.train.test2$id,
                           real = as.numeric(data.train.test2$churn),
                          Churn_prediction = data.train.test2.class,
                          Churn_probability = probs.train$`1`) %>%
  arrange(desc(Churn_probability)) %>% 
  mutate(Churn_prediction = Churn_prediction %>% as.character() %>% as.numeric())


train_output %>% filter(real == 1) %>% pull(Churn_probability) %>% summary()
# calculate brier
(train_output$real - train_output$Churn_probability)^2 %>% sum() / 
  nrow(train_output)


# train model with all training set
svm <- svm(as.numeric(churn) ~ . , data = data.train.complete, 
           kernel = "radial", gamma = g)

svm2 <- svm(as.factor(churn) ~ . , data = data.train.complete, 
            kernel = "radial", gamma = g, probability = T)



data.test.res.score <- predict(svm, data.test.complete)
data.test.res.class <- predict(svm2, data.test.complete, probability = T)

probs <- as.data.frame(attr(data.test.res.class, "probabilities"))

summary(data.test.res.score)
test_output <- data_frame(id = data.test.complete$id,
                          Churn_prediction = data.test.res.class,
                          Churn_probability = probs$`1`) %>%
  arrange(desc(Churn_probability))

write_csv(test_output, "ml_case_test_output.csv")

# look at the results
data.test.complete.final <- left_join(test_output, data.test.complete) %>% 
  mutate(
    Churn_prediction = Churn_prediction %>% as.character() %>% as.numeric()
         ) 

churns <- data.test.complete.final %>%
  filter(Churn_prediction == 1) 

# rev lost
sum(churns$forecast_cons_12m * churns$forecast_price_energy_p1 + 
      churns$forecast_price_pow_p1) *0.2
# lost net revenue
sum(churns$net_margin) #this is what company gains as net
# yearly total net margin for churns
sum(churns$net_margin / churns$num_years_antig)
# net margin + forecast_meter_rent_12m gives revenues as consumption * prices + fix price
sum(churns$forecast_meter_rent_12m)
# if they stay, loss in the revenue of the company
sum(churns$forecast_cons_12m * churns$forecast_price_energy_p1 + 
      churns$forecast_price_pow_p1)  * 0.2
