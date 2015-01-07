library(ggplot2); library(caret); library(Hmisc)

train <- readRDS("data/train.rds")
summary(train)


featurePlot(x = train[,.("hour", "season", "holiday", "workingday")], y = train$count, plot = "pairs")

qplot(hour, count, colour = weather, data = train)

qplot(hour, count, colour = season, data = train)

qplot(hour, count, colour = weekday, data = train)

qplot(hour, casual, data = train, facets=.~workingday)
qplot(hour, casual, data = train, facets=.~weekday)
qplot(hour, casual, data = train, facets=.~month)
qplot(hour, registered, data = train, facets=.~workingday)
qplot(hour, registered, data = train, facets=.~weekday)
qplot(hour, registered, data = train, facets=.~month)

qplot(atemp, count, colour = weather, data = train)

qplot(atemp, count, colour = season, data = train)

qplot(hour, temp, data = train)
qplot(hour, sin(temp * 2 * pi / 24), data = train)
qplot(hour, cos(temp * 2 * pi / 24), data = train)
qplot(hour, humidity, data = train)
qplot(month, humidity, data = train)
qplot(hour, windspeed, data = train)

hist(train$count, breaks = 200)
hist(train$casual, breaks = 200)
hist(train$registered, breaks = 200)
hist(log(train$count + 1), breaks = 100)
hist(log10(train$casual + 1), breaks = 100)
hist(sqrt(train$casual), breaks = 100)
hist(log10(train$registered + 1), breaks = 100)
hist(train$atemp, breaks = 100)
hist(log10(10 * train$atemp + 1), breaks = 100)
hist(log10(10 * train$temp), breaks = 100)
hist(sqrt(train$temp), breaks = 100)
hist(sin(2 * pi * train$temp * 24 / 365), breaks = 100)
hist(cos(2 * pi * train$temp * 24 / 365), breaks = 100)
hist(sin(train$humidity * 24 / 365), breaks = 100)
hist(cos(train$humidity * 24 / 365), breaks = 100)
hist(train$windspeed, breaks = 10)
hist(log(train$atemp + 1), breaks = 100)
hist(log10(train$humidity + 1), breaks = 100)
hist(log10(train$windspeed + 1), breaks = 100)
hist(train$temp / max(train$temp), breaks = 100)
hist(train$windspeed / max(train$windspeed), breaks = 100)
hist(train$humidity / 100, breaks = 100)

par(mfrow = c(3, 1))
hist(log10(train$casual + 1), breaks = 100)
hist(log10(train[workingday == "1", casual] + 1), breaks = 100)
hist(log10(train[workingday == "0", casual] + 1), breaks = 100)
par(mfrow = c(1, 1))

par(mfrow = c(3, 1))
hist(log10(train$registered + 1), breaks = 100)
hist(log10(train[workingday == "1", registered] + 1), breaks = 100)
hist(log10(train[workingday == "0", registered] + 1), breaks = 100)
par(mfrow = c(1, 1))

qplot(train$workingday, train$casual, geom= c("boxplot"))
qplot(train$workingday, train$registered, geom= c("boxplot"))

qplot(train$weekday, train$casual, geom= c("boxplot"))
qplot(train$weekday, train$registered, geom= c("boxplot"))

qplot(train$hour.c, train$casual, geom= c("boxplot"))
qplot(train$hour.c, train$registered, geom= c("boxplot"))

qplot(train$weather, train$casual, geom= c("boxplot"))
qplot(train$weather, train$registered, geom= c("boxplot"))

qplot(train$temp.c, train$casual, geom= c("boxplot"))
qplot(train$temp.c, train$registered, geom= c("boxplot"))

qplot(train$temp, train$casual)
qplot(train$temp, train$registered)

qplot(train$humidity, train$casual)
qplot(train$humidity, train$registered)

qplot(train$windspeed, train$casual)
qplot(train$windspeed, train$registered)

qplot(weekday, casual, data = train, facets = .~year)
qplot(weekday, registered, data = train, facets = .~year)

qplot(train$year, train$casual, geom= c("boxplot"))
qplot(train$year, train$registered, geom= c("boxplot"))

qplot(month, train$casual, data = train, facets = .~year)
qplot(month, train$registered, data = train, facets = .~year)
qplot(hour, train$casual, data = train, facets = .~year)
qplot(hour, train$registered, data = train, facets = .~year)

hist(train[as.integer(weather) == 3, humidity], breaks = 100)
qplot(train[as.integer(weather) == 3, humidity])
mean(train[as.integer(weather) == 3, humidity])

ctemp <- cut2(train$atemp, g = 5)
ccount <- cut2(train$count, g = 3)
qplot(month, count, colour = train$temp.c, data = train)
qplot(month, casual, colour = train$temp.c, data = train)
qplot(month, registered, colour = train$temp.c, data = train)

qplot(season, casual, colour = train$temp.c, data = train)
qplot(season, registered, colour = train$temp.c, data = train)

ccount <- cut2(train$casual, g = 4)
qplot(train$month,train$temp, colour = ccount)


# casual
ccount <- cut2(train$casual, g = 3)
png("plots/casual-predictors.png",  width=800, height = 800)
pairs(train[, .(year, month, weekday, hour, weather, atemp, humidity, windspeed, holiday, workingday,  casual)], panel = panel.smooth, col = 6 - as.numeric(ccount))
dev.off()

#registered
ccount <- cut2(train$registered, g = 3)
png("plots/registered-predictors.png",  width=800, height = 800)
pairs(train[, .(year, month, weekday, hour, weather, atemp, humidity, windspeed, holiday, workingday, registered)], panel = panel.smooth, col = 6 - as.numeric(ccount))
dev.off()



library(corrplot)
train.num <- data.table(train)
train.num[, hour:=as.integer(hour)]
train.num[, month:=as.integer(month)]
train.num[, weekday:=as.integer(weekday)]
train.num[, season:=as.integer(season)]
train.num[, weather:=as.integer(weather)]
train.num[, holiday:=as.integer(holiday)]
train.num[, workingday:=as.integer(workingday)]
train.num[, datetime:=NULL]
train.num[, day:=NULL]
str(train.num)
M<-cor(train.num)
png("plots/corrplot.png", width = 1000, height = 1000)
corrplot(M, type = "lower",method = "square",tl.cex=.6)
dev.off()

M<-cor(train.num[, .(year, month, hour, weekday, holiday, workingday, weather, atemp, humidity, windspeed, casual)])
png("plots/corrplot-casual.png", width = 1000, height = 1000)
corrplot(M, type = "lower",method = "square",tl.cex=.7)
dev.off()

M<-cor(train.num[, .(year, month, hour, weekday, holiday, workingday, weather, atemp, humidity, windspeed, registered)])
png("plots/corrplot-registered.png", width = 1000, height = 1000)
corrplot(M, type = "lower",method = "square",tl.cex=.7)
dev.off()



