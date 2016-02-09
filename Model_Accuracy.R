############## DATA SPLIT #################
# Explicitly split dataset into separate test and train sets
library(caret)
library(klaR)
# load the iris dataset
data(iris)
# define an 80%/20% train/test split of the dataset
trainIndex <- createDataPartition(iris$Species, p=0.80, list=FALSE)
dataTrain <- iris[ trainIndex,]
dataTest <- iris[-trainIndex,]
# train a naive Bayes model
fit <- NaiveBayes(Species~., data=dataTrain)
# make predictions
predictions <- predict(fit, dataTest[,1:4])
# summarize results
confusionMatrix(predictions$class, dataTest$Species)

############# BOOTSTRAP ##################
# load the package
library(caret)
# load the iris dataset
data(iris)
# define training control
trainControl <- trainControl(method="boot", number=100)
# evalaute the model
fit <- train(Species~., data=iris, trControl=trainControl, method="nb")
# display the results
print(fit)

######## K-FOLD CROSS VALIDATION ########
library(caret)
# load the iris dataset
data(iris)
# define training control
trainControl <- trainControl(method="cv", number=10)
# evaluate the model
fit <- train(Species~., data=iris, trControl=trainControl, method="nb")
# display the results
print(fit)

########### REPEATED K-FOLD ############
# load the package
library(caret)
# load the iris dataset
data(iris)
# define training control
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
# evaluate the model
fit <- train(Species~., data=iris, trControl=trainControl, method="nb")
# display the results
print(fit)

#### LEAVE ONE OUT CROSS VALIDATION ####
# load the package
library(caret)
# load the iris dataset
data(iris)
# define training control
trainControl <- trainControl(method="LOOCV")
# evaluate the model
fit <- train(Species~., data=iris, trControl=trainControl, method="nb")
# display the results
print(fit)





