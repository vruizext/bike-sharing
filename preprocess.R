library(data.table)
library(Hmisc)
train <- read.csv("data/raw/train.csv", header = T)
test <- read.csv("data/raw/test.csv", header = T)
full <- read.csv("data/raw/Bike-Sharing-Dataset/hour.csv", header=T)


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

train <- data.table(train)
train.counts <- train[,.(casual, registered, count)]
train <- train[,!colnames(train) %in% c("casual", "registered", "count"), with = F]
str(train)

max.temp <- max(c(train$temp, test$temp))
max.atemp <- max(c(train$atemp, test$atemp))
max.wind <- max(c(train$windspeed, test$windspeed))
max.humi <- 100

time <- strptime(train$datetime, format="%Y-%m-%d %H:%M:%S")
train[, datetime:=as.Date(datetime)]
train[, year:=factor(year(datetime))]
train[, month:= factor(time$mon + 1)]
train[, day:= factor(time$mday)]
train[, hour:= factor(time$hour)]
train[, weekday := factor(weekdays(as.Date(time)))]
train[, weekday := factor(weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))]
train[, season:=factor(season, levels=1:4, labels=c("winter", "spring", "summer", "fall"))]
train[, weather:=factor(weather, levels=1:4, labels=c("clear", "clouds", "light-rain-snow", "heavy-storm"))]
train[, holiday:=factor(holiday)]
train[, workingday:=factor(workingday)]
#normalize numeric variables
train[, temp.n:=temp/max.temp]
train[, atemp.n:=atemp/max.atemp]
train[, humidity.n:=humidity/max.humi]
train[, windspeed.n:=windspeed/max.wind]
#transform numeric to integers
train[, temp.i:=as.integer(round(temp))]
train[, atemp.i:=as.integer(round(atemp))]
train[, humidity.i:=as.integer(round(humidity))]
train[, windspeed.i:=as.integer(round(windspeed))]
#slice variables
train[, temp.c:=factor(cut2(temp, seq(0, 5 * ceiling(max.temp / 5), 5)))]
train[, atemp.c:=factor(cut2(atemp, seq(0, 5 * ceiling(max.atemp / 5), 5)))]
train[, humidity.c:=factor(cut2(humidity, seq(0, 100, 5)))]
train[, windspeed.c:=factor(cut2(windspeed, seq(0, 5 * ceiling(max.wind / 5), 5)))]
train[, hour.c:=factor(cut2(as.integer(hour) - 1, c(0, 3, 6, 9, 12, 15, 18, 21)))]
#add weekend factor
train[,weekend:="0"]
train[train$weekday %in% c("Saturday", "Sunday"),weekend:="1"]
train[, weekend:=factor(weekend)]
str(train)
saveRDS(train.counts, "data/train.counts.rds")
saveRDS(train, "data/train.rds")

#weather 4, outlier, only 1 occurrence, set it to 3?
train[weather == "heavy-storm", weather := "light-rain-snow"]
#impute humidity = 80 to all registers of 2011-03-10 which are wrongly set to 0
train[datetime == "2011-03-10", humidity := 80]


## now apply same transformations on test dataset
test <- data.table(test)
time <- strptime(test$datetime, format="%Y-%m-%d %H:%M:%S")
test[, year:=factor(year(as.Date(datetime)))]
test[, month:= factor(time$mon + 1)]
test[, day:= factor(time$mday)]
test[, hour:= factor(time$hour)]
test[, weekday := factor(weekdays(as.Date(time)))]
test[, weekday := factor(weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))]
test[, season:=factor(season, levels=1:4, labels=c("winter", "spring", "summer", "fall"))]
test[, weather:=factor(weather, levels=1:4, labels=c("clear", "clouds", "light-rain-snow", "heavy-storm"))]

test[, holiday:=factor(holiday)]
test[, workingday:=factor(workingday)]
#slice temp and humidity data
test[, temp.c:=factor(cut2(temp, seq(0, 5 * ceiling(max.temp / 5), 5)))]
test[, atemp.c:=factor(cut2(atemp, seq(0, 5 * ceiling(max.atemp / 5), 5)))]
test[, humidity.c:=factor(cut2(humidity, seq(0, 100, 5)))]
test[, windspeed.c:=factor(cut2(windspeed, seq(0, 5 * ceiling(max.wind / 5), 5)))]
test[, hour.c:=factor(cut2(as.integer(hour) - 1, c(0, 3, 6, 9, 12, 15, 18, 21)))]

#normalize numeric variables
test[, temp.n:=temp/max.temp]
test[, atemp.n:=atemp/max.atemp]
test[, humidity.n:=humidity/max.humi]
test[, windspeed.n:=windspeed/max.wind]
#transform numeric to integers
test[, temp.i:=as.integer(round(temp))]
test[, atemp.i:=as.integer(round(atemp))]
test[, humidity.i:=as.integer(round(humidity))]
test[, windspeed.i:=as.integer(round(windspeed))]

#add weekend factor
test[,weekend:="0"]
test[train$weekday %in% c("Saturday", "Sunday"),weekend:="1"]
test[, weekend:=factor(weekend)]

test[weather == "heavy-storm", weather := "light-rain-snow"]

saveRDS(test, "data/test.rds")

#save test counts to test models
full <- data.table(full)
time <- strptime(full$dteday, format="%Y-%m-%d")
full[, day:=as.integer(time$mday)]
train.rows <- which(full$day %in% 1:19)
test.counts <- full[-train.rows, .(casual, registered, cnt)]
setnames(test.counts, "cnt", "count")
saveRDS(test.counts, "data/test.counts.csv")
