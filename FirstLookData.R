# Load the package
library(mlbench)
# Load the dataset
data("PimaIndiansDiabetes")
# Display first 20 rows
head(PimaIndiansDiabetes,n=20)
# Display the dimensions of the dataset
dim(PimaIndiansDiabetes)
# List types for each attribute
sapply(PimaIndiansDiabetes,class)
# Distribution of class variable
y<-PimaIndiansDiabetes$diabetes
cbind(freq=table(y),percentage=prop.table(table(y))*100)
# Show summary of attributes
summary(PimaIndiansDiabetes)
# Standard deviations of the data
sapply(PimaIndiansDiabetes[,1:8],sd)
# Calculate skewness
library(mlbench)
library(e1071)
skew<-apply(PimaIndiansDiabetes[1:8],2,skewness)
print(skew)

# Calculate correlations
correlations<-cor(PimaIndiansDiabetes[,1:8])
print(correlations)
