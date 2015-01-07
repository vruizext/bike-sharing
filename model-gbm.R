source("predict.model.R")
train <- readRDS("data/train.rds")
test <- readRDS("data/test.rds")
train.counts <- readRDS("data/train.counts.rds")
test.counts <- readRDS("data/test.counts.rds")
counts.cas <- log10(train.counts$casual + 1)
counts.reg <- log10(train.counts$registered + 1)

cols <- c("weekday", "year", "month", "hour", "workingday", "temp", "humidity", "weather", "season", "windspeed")
predictors <- train[, cols, with = F]
predictors.test <- test[, cols, with = F]

model.gbm.reg <- count.model.gbm(predictors, counts.reg, 1000, 4, c(0.1, 0.05, 0.01))
model.gbm.cas <- count.model.gbm(predictors, counts.cas, 1000, 4, c(0.1, 0.05, 0.01))
model.gbm.reg
model.gbm.cas

cas.test <- predict(model.gbm$cas, predictors.test)
reg.test <- predict(model.gbm$reg, predictors.test)
tot.test <- round(10^(reg.test) - 1 + 10^(cas.test) - 1)

rmsle(test.counts$casual, 10^cas.test - 1)
rmsle(test.counts$registered, 10^reg.test - 1)
rmsle(test.counts$count, tot.test)

predictions.write(test$datetime, tot.test, "results-gbm.csv")




