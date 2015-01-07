library(data.table)
library(caret)
library(randomForest)
library(doMC)
registerDoMC(cores = 2)


#train model using all available data and generate count predictions
count.model.rf <- function(predictors, count, ntree, mtry, nsize = 5, seed = 12345) {
	ctrl <- trainControl(method = "oob", allowParallel = TRUE)
	grid <- expand.grid(.mtry = mtry) #, .ntree = ntree
	set.seed(seed)
	model <- train(predictors, count, method = "rf",
				   	trControl = ctrl, tuneGrid = grid,
					varImp = TRUE, importance = TRUE, prox = TRUE,
					ntree = ntree,  nodesize = nsize) #ntree = ntree,
	model
}

count.model.gbm <- function(predictors, count, ntree, inter = 3, shrink  = 0.01) {
	ctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3,
							allowParallel = TRUE, verbose = F)

	grid = expand.grid(.interaction.depth = inter, .n.trees = ntree, .shrinkage = shrink)
	set.seed(12345)
	model = train(predictors, count, method='gbm',
				  metric   = "RMSE",
				  tuneGrid = grid,
				  trControl = ctrl,
				  distribution = "gaussian",
				  n.minobsinnode = 5,
				  verbose = FALSE)
}

#return predictions for the given predictors values
count.predict <- function(model, predictors) {
	cas.test <- predict(model$cas, predictors)
	reg.test <- predict(model$reg, predictors)
	round(10^(cas.test) + 10^(reg.test) - 2)
}

#write results to csv file, as required by kaggle
predictions.write <- function(hours, predictions, file = "test-results.csv") {
	test.result <- data.frame(datetime=hours, count=predictions, row.names=NULL)
	write.csv(file, x=test.result, row.names = F, quote = F)
}


rmsle <- function(pv, av) {
	(sum((log(av + 1) - log(pv + 1))^2) / length(pv))^0.5
}

RMSLE <- function(data, lev = NULL, model = NULL,...) {
	if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
		stop("levels of observed and predicted data do not match")
	rmsle(data$pred, data$obs)
}



