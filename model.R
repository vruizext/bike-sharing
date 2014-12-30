library(data.table)
library(caret)
library(randomForest)
library(doMC)
registerDoMC(cores = 2)

source("predict.model.R")

rmsle <- function(pv, av) {
	(sum((log(av + 1) - log(pv + 1))^2) / length(pv))^0.5
}

train <- readRDS("data/train.rds")
counts <- train[,.(casual, registered, count)]

#predictors.reg <- train[, .(year, month, hour, workingday, atemp, humidity, windspeed)]

predictors <- train[, .(weekday, year, month, hour, workingday, atemp, humidity, windspeed)]

test.rows <- which(train$day == 18 | train$day == 19)

test.rows <- which(train$day == 1| train$day == 7 | train$day == 13 | train$day == 19)

predictors.test <- predictors[test.rows]
counts.test <- counts[test.rows]
predictors.train <- predictors[-test.rows]
counts.train <- counts[-test.rows]


model <- count.model(predictors.train, counts.train, 100, 6, 6)

model$cas$finalModel
model$reg$finalModel
#model$tot$finalModel

saveRDS(model, "model.rds")


tot.test <- count.predict(model, predictors.test)
rmsle(tot.test, counts.test$count)

cas.test <- predict(model$cas, predictors.test)
rmsle(cas.test, counts.test$casual)

reg.test <- predict(model$reg, predictors.test)
rmsle(reg.test, counts.test$registered)

tot.test <- floor(reg.test + cas.test)
rmsle(tot.test, counts.test$count)

test <- readRDS("data/test.rds")
hours <- test$datetime
test <- test[, .(weekday, year, month, hour, workingday, atemp, humidity, windspeed)]

model <- readRDS("model_50x8.rds")
tot.test <- count.predict(model, test)
predictions.write(hours, tot.test)

cas.test <- predict(model$cas, test)
reg.test <- predict(model$reg, test)
tot.test <- cas.test + reg.test


