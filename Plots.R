data(iris)
# Create histograms for each attribute
par(mfrow=c(1,4))
for(i in 1:4){
  hist(iris[,i],main=names(iris)[i])
}
# Same but smoothed out
library(lattice)
for(i in 1:4){
  plot(density(iris[,i]),main=names(iris)[i])
}

# Box and whisker plots
for(i in 1:4){
  boxplot((iris[,i]),main=names(iris)[i])
}

# Missing plot
# load packages
library(Amelia)
library(mlbench)
# load dataset
data(Soybean)
# create a missing map
missmap(Soybean, col=c("black", "grey"), legend=FALSE)

# Correlation plot
library(corrplot)
# load the data
data(iris)
# calculate correlations
correlations <- cor(iris[,1:4])
# create correlation plot
corrplot(correlations, method="circle")

# Scatterplot matrix
pairs(iris)

# Color the classes
data(iris)
# pair-wise scatterplots colored by class
pairs(Species~., data=iris, col=iris$Species)

# Density plots
library(caret)
x <- iris[,1:4]
y <- iris[,5]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

