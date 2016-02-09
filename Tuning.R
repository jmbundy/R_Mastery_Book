###### Algorithm Tuning
# Load packages
library(randomForest)
library(mlbench)
library(caret)
# Load Dataset
data(Sonar)
dataset <- Sonar
x <- dataset[,1:60]
y <- dataset[,61]


### Test algorithm is Random Forest, 2 parameters for tuning are most likely to have the largest effect.
### mtry is the number of variables randomly sampled from each split, ntree is the number of trees to grow.

# Create model with default paramters
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rfDefault <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid,
                   trControl=trainControl)
print(rfDefault)

###### Tune using caret
### Random search
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rfRandom <- train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15,
                  trControl=trainControl)
print(rfRandom)
plot(rfRandom)

###### Grid Search
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rfGrid <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid,
                trControl=trainControl)
print(rfGrid)
plot(rfGrid)

######### Tune using algorithm tools
# Algorithm Tune (tuneRF) : Lower is better
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

######## Create your own manually or by extending caret
# Manual Search
### Caret won't let you tune ntree, but you can run it manually holding mtry constant to get an idea
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid,
               trControl=trainControl, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

####### Extend caret
customRF <- list(type="Classification", library="randomForest", loop=NULL)
customRF$parameters <- data.frame(parameter=c("mtry", "ntree"), class=rep("numeric", 2),
                                  label=c("mtry", "ntree"))
customRF$grid <- function(x, y, len=NULL, search="grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry=param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc=NULL, submodels=NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc=NULL, submodels=NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom <- train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid,
                trControl=trainControl)
summary(custom)
plot(custom)


