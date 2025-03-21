#package FSelector needs to be preinstalled to calculate information gain
#package dplyr,caret,car,Metrics,rpart.plot,xgboost needs to be pre-installed
source("data_preprocessing.R")
library(caret)
set.seed(123)

#split train test and validation data set
data<-data %>% mutate_if(is.character, as.factor)
data$is_canceled <- as.factor(data$is_canceled)
trainIdex<-createDataPartition(data$is_canceled,p=0.7,list=FALSE)
trainData<-data[trainIdex,]
tempData<-data[-trainIdex,]
validIndex<-createDataPartition(tempData$is_canceled,p = 0.5,list=FALSE)
validData<-tempData[validIndex,]
testData<-tempData[-validIndex,]

cat("Number of samples in the training set:", nrow(trainData), "\n")
cat("Number of samples in the validatioin set:", nrow(validData), "\n")
cat("Number of samples in the testing set:", nrow(testData), "\n")


data$is_canceled <- factor(data$is_canceled, levels = c(0, 1), labels = c("No", "Yes"))
#1. logistic linear regression
#caret train Support cross-validation, auto-tuning
# library(pROC)
library(Metrics)
#glm
custom_summary <- function(data, lev = NULL, model = NULL) {
  acc <- sum(data$pred == data$obs) / nrow(data)
  y_true <- ifelse(data$obs == lev[2], 1, 0)
  logloss <- logLoss(y_true, data[, lev[2]])
  return(c(Accuracy = acc, LogLoss = logloss))
}

train_control_glm <- trainControl(method = "cv", number = 5,
                                  summaryFunction = custom_summary,
                                  classProbs = TRUE,
                                  seeds = lapply(1:6, function(x) rep(123, 1)))
glm_model <- train(
  is_canceled ~ .,
  data = data,
  method = "glm",
  family = binomial,
  trControl = train_control_glm
)
print(glm_model)
glm_logistic_regression_results<-data.frame(Model="Logistic Regression",
                                        Accuracy=glm_model$results$Accuracy,
                                        LogLoss=glm_model$results$LogLoss)
#glmnet
custom_summary <- function(data, lev = NULL, model = NULL) {
  acc <- sum(data$pred == data$obs) / nrow(data)
  y_true <- ifelse(data$obs == lev[2], 1, 0)
  logloss <- -logLoss(y_true, data[, lev[2]])
  return(c(Accuracy = acc, LogLoss = logloss))
}
train_control_glmnet <- trainControl(method = "cv", number = 5,
                                  summaryFunction = custom_summary,
                                  classProbs = TRUE,verboseIter = TRUE,
                                  seeds = lapply(1:6, function(x) rep(123, 1)))

lambda_seq <- seq(0.001,0.1,length.out = 20)
# glmnet_model<-train(is_canceled ~ .,,data=data,
#                     method="glmnet",
#                     trControl=train_control_glmnet,
#                     metric = "LogLoss",
#                     tuneGrid=expand.grid(alpha=1,lambda=lambda_seq))
# lambda_seq<-seq(0.0001,0.001,length.out=10)
glmnet_model<-train(is_canceled ~ .,,data=data,
                    method="glmnet",
                    trControl=train_control_glmnet,
                    metric = "LogLoss",
                    tuneGrid=expand.grid(alpha=1,lambda=lambda_seq))
print(glmnet_model)

#decision tree
library(rpart)
library(rpart.plot)
dt_cv_control<-trainControl(method = "cv",number=5,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            seeds = lapply(1:6, function(x) rep(123, 1))
                            )
dt_model<-train(is_canceled~.,data=data,method="rpart",
                          trControl=dt_cv_control,tuneLength=10,metric="ROC")
best_row_dt <- dt_model$results[which.max(dt_model$results$ROC), ]
P <- mean(data$is_canceled == "Yes")
N<-1-P
dt_accuracy<-best_row_dt$Sens*P+best_row_dt$Spec*N
rpart.plot(dt_model$finalModel,type = 2,under = TRUE,tweak = 1.2)


dt_grid <- expand.grid(cp = seq(0.001,0.003,0.0002))
results_dt_grid <- list()
max_depth_values <- c(3, 5, 7, 10)
# max_depth_values <- c(10,12,15)
for (depth in max_depth_values){
  dt_model_grid<-train(is_canceled~.,data=data,method="rpart",
                       trControl=dt_cv_control,tuneGrid=dt_grid,metric="ROC",
                       control = rpart.control(maxdepth = depth))
  results_dt_grid[[paste("max_depth=", depth)]] <- dt_model_grid
}
best_model_name_dt <- names(results_dt_grid)[which.max(sapply(results_dt_grid, function(m) max(m$results$ROC)))]

best_model_dt <- results_dt_grid[[best_model_name_dt]]
best_row_dt_grid<-best_model_dt$results[which.max(dt_model$results$ROC), ]
dt_accuracy_grid<-best_row_dt_grid$Sens*P+best_row_dt_grid$Spec*N
rpart.plot(best_model_dt$finalModel,type = 2,under = TRUE,tweak = 1.8)

#XGboost
library(xgboost)
seeds_xgb<-lapply(1:5, function(x) rep(123, 36))
seeds_xgb[[6]]<-123
xgb_control <- trainControl(
  method = "cv", number = 5, classProbs = TRUE,
  summaryFunction = twoClassSummary, verboseIter = TRUE,
  seeds = seeds_xgb)
xgb_model<-train(is_canceled~.,data=data,method="xgbTree",
                 trControl=xgb_control,metric="ROC")
best_row_xgb <- xgb_model$results[which.max(xgb_model$results$ROC), ]
print(best_row_xgb)
accuracy_xgboost=best_row_xgb$Sens*P+best_row_xgb$Spec*N


xgb_grid <- expand.grid(
  nrounds = c(100, 150,200),
  max_depth = c(3,4,5),
  eta = c(0.1,0.2,0.4),
  gamma = c(0,0.3,0.5),
  colsample_bytree = 0.8,
  min_child_weight = c(1, 3),
  subsample=0.75
)
seeds_xgb_grid<-lapply(1:5, function(x) rep(123,180))
seeds_xgb_grid[[6]]<-123
xgb_control_grid <- trainControl(
  method = "cv", number = 5, classProbs = TRUE,
  summaryFunction = twoClassSummary, verboseIter = TRUE,
  seeds = seeds_xgb_grid)
xgb_model_grid <- train(
  is_canceled ~ ., data = data, method = "xgbTree",
  trControl = xgb_control_grid,metric="ROC",
  tuneGrid = xgb_grid
)

print(xgb_model_grid$bestTune)
best_row_xgb_grid <- xgb_model_grid$results[which.max(xgb_model_grid$results$ROC), ]
print(best_row_xgb_grid)
accuracy_xgboost_grid=best_row_xgb_grid$Sens*P+best_row_xgb_grid$Spec*N

