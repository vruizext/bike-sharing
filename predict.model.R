library(data.table)
library(caret)
library(randomForest)
library(doMC)
registerDoMC(cores = 2)


#train model using all available data and generate count predictions
count.model <- function(predictors, counts, ntree,
						mtry.min, mtry.max = length(predictors), seed = 12345) {
	model = list()
	ctrl <- trainControl(method = "oob", allowParallel = TRUE )
	#ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE )
	set.seed(seed)
	model$cas <- train(predictors, counts$casual, method = "rf", trControl = ctrl,
					   varImp = TRUE, importance = TRUE,  ntree = ntree, prox = TRUE,
					   tuneGrid = expand.grid(.mtry = mtry.min:mtry.max))
	set.seed(seed)
	model$reg <- train(predictors, counts$registered, method = "rf", trControl = ctrl,
					   varImp = TRUE, importance = TRUE,  ntree = ntree, prox = TRUE,
					   tuneGrid = expand.grid(.mtry = mtry.min:mtry.max))
# 	set.seed(12345)
# 	model$tot <- train(predictors, counts$count, method = "rf", trControl = ctrl,
# 					   importance = TRUE,  ntree = ntree, prox = TRUE,
# 					   tuneGrid = data.frame(.mtry = mtry))
	model
}

#return predictions for the given predictors values
count.predict <- function(model, predictors) {
	cas.test <- predict(model$cas, predictors)
	reg.test <- predict(model$reg, predictors)
	floor(cas.test + reg.test)
}

#write results to csv file, as required by kaggle
predictions.write <- function(hours, predictions, file = "test-results.csv") {
	test.result <- data.frame(datetime=hours, count=predictions, row.names=NULL)
	write.csv(file, x=test.result, row.names = F, quote = F)
}

