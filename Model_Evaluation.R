########### ACCURACY & KAPPA ###########
# load packages
library(caret)
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
# prepare resampling method
trainControl <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="Accuracy",
             trControl=trainControl)
# display results
print(fit)

############# RMSE & R2 #############
# load packages
library(caret)
# load data
data(longley)
# prepare resampling method
trainControl <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Employed~., data=longley, method="lm", metric="RMSE", trControl=trainControl)
# display results
print(fit)

############# ROC CURVE #############
# ONLY FOR BINARY CLASSIFICATION!!!

# load packages
library(caret)
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
# prepare resampling method
trainControl <- trainControl(method="cv", number=5, classProbs=TRUE,
                             summaryFunction=twoClassSummary)
set.seed(7)
fit <- train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="ROC",
             trControl=trainControl)
# display results
print(fit)

############ LOG LOSS #############
# load packages
library(caret)
# load the dataset
data(iris)
# prepare resampling method
trainControl <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(7)
fit <- train(Species~., data=iris, method="rpart", metric="logLoss", trControl=trainControl)
# display results
print(fit)
                             











