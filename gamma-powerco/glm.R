source('prepare.R')
library(ROCR)

data.train.train <- data.train.train %>% 
  mutate(churn = as.numeric(churn)) %>%
  select(-contains("p2"), -contains("p3")) %>%
  select_if(~is.numeric(.)|is.Date(.))

model <- glm(churn ~ ., data = data.train.train, 
             family = "binomial", na.action = na.omit)

p <- predict(model, newdata = data.train.train, type = 'response')
summary(model)

varImp(model, scale = F) %>% View()
# anova(model, test="Chisq")

fitted.results <- ifelse(model$fitted.values > 0.15, 1, 0)
confusionMatrix(as.factor(fitted.results), reference =  as.factor(model$y))

pr <- ROCR::prediction(fitted.results, model$y)
pr <- ROCR::prediction(model$fitted.values, model$y)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- ROCR::performance(pr, measure = "auc")
auc@y.values
# postResample(pred = data.train.test.clean.pred,  obs = data.train.test.clean$SalePrice)

# data.test.clean.pred <- predict(lm.train, newdata = data.test.clean)

# there are negative values ??
# data.test.clean.pred[data.test.clean.pred < 0] <- 0

results <-  data.frame(Id = data.test.clean$Id, SalePrice = data.test.clean.pred) %>% 
  filter(!is.na(SalePrice))
