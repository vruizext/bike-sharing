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

model.rf.cas <- count.model.rf(predictors, counts.cas, 100, 5:9)
model.rf.reg <- count.model.rf(predictors, counts.reg, 100, 5:9)
model.rf.cas$finalModel
model.rf.reg$finalModel

cas.test <- predict(model.rf.cas, predictors.test)
reg.test <- predict(model.rf.reg, predictors.test)
tot.test <- round(10^reg.test + 10^cas.test - 2)

rmsle(test.counts$casual, 10^cas.test - 1) #
rmsle(test.counts$registered, 10^reg.test - 1) #
rmsle(tot.test, counts.test$count)

predictions.write(test$datetime, tot.test, "results-rf.csv")
