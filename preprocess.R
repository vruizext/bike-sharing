library(data.table)
train <- read.csv("data/raw/train.csv", header = T)

train <- data.table(train)

names(train)

# Data Fields
#
# datetime - hourly date + timestamp
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter
# labels specified by kaggle are wrong, use these instead
# 1 = winter, 2 = spring, 3 = summer, 4 = fall
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# count - number of total rentals

time <- strptime(train$datetime, format="%Y-%m-%d %H:%M:%S")
train[, datetime:=as.Date(datetime)]
train[, year:=year(datetime)]
train[, month:= factor(time$mon + 1)]
train[, day:= factor(time$mday)]
train[, hour:= factor(time$hour)]
train[, weekday := factor(weekdays(as.Date(train$datetime)))]
setcolorder(train, c(1, 13, 14, 15, 16, 17, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

train[, season:=factor(season, levels=1:4, labels=c("winter", "spring", "summer", "fall"))]

train[, weather:=factor(weather, levels=1:4, labels=c("clear", "clouds", "light-rain-snow", "heavy-storm"))]

train[, holiday:=factor(holiday)]
train[, workingday:=factor(workingday)]

saveRDS(train, "data/train.rds")

## now apply same transformations on test dataset
test <- read.csv("data/raw/test.csv", header = T)
test <- data.table(test)
time <- strptime(test$datetime, format="%Y-%m-%d %H:%M:%S")
test[, year:=year(as.Date(datetime))]
test[, month:= factor(time$mon + 1)]
test[, day:= factor(time$mday)]
test[, hour:= factor(time$hour)]
test[, weekday := factor(weekdays(as.Date(test$datetime)))]
setcolorder(test, c(1, 10, 11, 12, 13, 14, 2, 3, 4, 5, 6, 7, 8, 9))

test[, season:=factor(season, levels=1:4, labels=c("winter", "spring", "summer", "fall"))]

test[, weather:=factor(weather, levels=1:4, labels=c("clear", "clouds", "light-rain-snow", "heavy-storm"))]

test[, holiday:=factor(holiday)]
test[, workingday:=factor(workingday)]
saveRDS(test, "data/test.rds")


