####### Prepare
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)

####### Train the Models
# prepare training scheme
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(7)
fit.cart <- train(diabetes~., data=PimaIndiansDiabetes, method="rpart", trControl=trainControl)
# LDA
set.seed(7)
fit.lda <- train(diabetes~., data=PimaIndiansDiabetes, method="lda", trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial", trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(diabetes~., data=PimaIndiansDiabetes, method="knn", trControl=trainControl)
# Random Forest
set.seed(7)
fit.rf <- train(diabetes~., data=PimaIndiansDiabetes, method="rf", trControl=trainControl)
# collect resamples
results <- resamples(list(CART=fit.cart, LDA=fit.lda, SVM=fit.svm, KNN=fit.knn, RF=fit.rf))


######### 8 Methods of Comparison
##### 1. Table Summary
### Focus more on Mean and Max
summary(results)

#### 2. Box and whisker plots to compare models
##Note that the boxes are ordered from highest to lowest mean accuracy. I find it useful to
##look at the mean values (dots) and the overlaps of the boxes (middle 50% of results).

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

###### 3. Density plots of accuracy
#### I like to look at the differences in the peaks as well as the spread or base of the distributions.
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")

##### 4. Dot Plots
#### These are useful plots as they show both the mean estimated accuracy as well as the 95%
#### confidence interval (e.g. the range in which 95% of observed scores fell).
scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results, scales=scales)


##### 5. Parallel Plots
### Shows each trial of each CV
parallelplot(results)

##### 6. Scatterplot Matrix
# This is invaluable when considering whether the predictions from two different algorithms
# are correlated. If weakly correlated, they are good candidates for being combined in an ensemble
# prediction. For example, eye-balling the graphs it looks like LDA and SVM look strongly
# correlated, as does SVM and RF. SVM and CART look weakly correlated.
splom(results)

####### 7. Pairwise xyPlot
xyplot(results, models=c("LDA", "SVM"))

###### 8. Statistical Significance Tests
#### Smaller the better
# difference in model predictions
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)







