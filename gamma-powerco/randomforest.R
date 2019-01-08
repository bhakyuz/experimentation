source('prepare.R')
library(randomForest)
library(ROCR)

# Can not handle categorical predictors with more than 53 categories.
data.train.train <- data.train.train %>% select(-id, -activity_new)
data.train.test <- data.train.test %>% select(-id, -activity_new)

rf <- randomForest(as.factor(churn) ~ . , data = data.train.train, 
                   importance = T,
                   sampsize = c(600, 600)
                   # sampsize = c(600, 600) # sens .84 spec .4
                   )

rf
confusionMatrix(data = rf$predicted, reference = as.factor(data.train.train$churn))

View(rf$importance)
varImpPlot(rf, type = 2)
res <- predict(rf, data.train.test)

pr <- ROCR::prediction(as.numeric(as.logical(rf$predicted)), as.numeric(as.logical(rf$y)))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- ROCR::performance(pr, measure = "auc")
auc@y.values

