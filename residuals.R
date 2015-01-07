### residual analysis
cols <- c("year", "month", "hour", "hour.c", "weekday", "workingday", "temp.c", "temp", "humidity", "windspeed", "weather", "season")
predictors.reg <- train[, cols, with = F]
cols <- c("weekday", "year", "month", "hour", "workingday", "temp", "humidity", "weather", "season", "windspeed")
predictors.cas <- train[, cols, with = F]

reg.train <- predict(model.rf.reg, predictors.reg)
cas.train <- predict(model.gbm.cas, predictors.cas)
tot.train <- (10^(reg.train) + 10^(cas.train) - 2)
rmsle(10^cas.train - 1, train.counts$casual)
rmsle(10^reg.train - 1, train.counts$registered)
rmsle(round(tot.train), train.counts$count)

#squared log residual
sle <- (log(tot.train + 1) - log(train.counts$count + 1))^2
sle.cas <- (log(10^cas.train) - log(train.counts$casual + 1))^2
sle.reg <- (log(10^reg.train) - log(train.counts$registered + 1))^2

idx.2011 <- which(train$year == "2011")
idx.2012 <- which(train$year == "2012")
rmsle(train.counts$casual[idx.2011], (10^cas.train[idx.2011] - 1))
rmsle(train.counts$casual[idx.2012], (10^cas.train[idx.2012] - 1))
#histogram of residuals
hist(sle.cas, breaks = 100)
hist(sle.reg, breaks = 100)

#points with higher error
idx.cas <- which(sle.cas > 0.5)
idx.reg <- which(sle.reg > 0.214)

qplot(counts$count, sle)
qplot(10^cas.train[idx.2011] - 1, sle.cas[idx.2011])
qplot(10^cas.train[idx.2012] - 1, sle.cas[idx.2012])
qplot(exp(counts$registered) - 1, sle.reg)
qplot(test.counts$casual, sle.cas)
qplot(counts$registered, sle.reg)

qplot(train$weekday[idx.cas], sle.cas[idx.cas], geom= c("boxplot"))
qplot(train$weekday, sle.cas, geom= c("boxplot"))
qplot(train$hour, sle.reg, geom= c("boxplot"))
qplot(train$hour, sle.cas, geom= c("boxplot"))
qplot(train$hour[idx.cas], sle.cas[idx.cas], geom= c("boxplot"))
qplot(train$hour, sle.cas, geom= c("boxplot"))
qplot(train$hourc, sle.reg, geom= c("boxplot"))
qplot(test$hour.c, sle.cas, geom= c("boxplot"))
qplot(train$month, sle.reg, geom= c("boxplot"))
qplot(train$month[idx.cas], sle.cas[idx.cas], geom= c("boxplot"))
qplot(train$year, sle.reg, geom= c("boxplot"))
qplot(test$year, sle.cas, geom= c("boxplot"))
qplot(train$season, sle.reg, geom= c("boxplot"))
qplot(train$season[idx.cas], sle.cas[idx.cas], geom= c("boxplot"))
qplot(train$workingday, sle.reg, geom= c("boxplot"))
qplot(train$workingday, sle.cas, geom= c("boxplot"))
qplot(train$holiday, sle.reg, geom= c("boxplot"))
qplot(train$holiday, sle.cas, geom= c("boxplot"))
qplot(test$weather, sle.reg, geom= c("boxplot"))
qplot(test$weather, sle.cas, geom= c("boxplot"))
qplot(train$humidity, sle.reg)
qplot(train$humidity[idx.cas], sle.cas[idx.cas])
qplot(train$windspeed, sle.reg)
qplot(train$windspeed, sle.cas)
qplot(train$temp, sle.cas)
qplot(train$atemp, sle.reg)
qplot(train$hour.c, sle.cas)
qplot(train$humidity, train$windspeed, colour = sle, fill = 10 * sle)

