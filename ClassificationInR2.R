# ClassificatioInR.R
# Complete the code where you find the comment "# Make change here"
# The new code will:
#   (1) Partition data into testing and training sets using a "good" function
#   (2) Classify outcomes using Naive Bayes
#   (3) Show a Confusion Matrix for the Naive Bayes classification

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

# Set repeatable random seed
set.seed(4)

# Add functions and objects from ModelingHelper.R
source("ClassificationHelper.R")

###################################################

# Partition data between training and testing sets

# Replace the following line with a function that partitions the data correctly
#dataframeSplit <- BadPartition(dataframe, fractionOfTest=0.4)
dataframeSplit <- ExactPartition(dataframe, fractionOfTest=0.4)
#dataframeSplit <- FastPartition(dataframe, fractionOfTest=0.4)
testSet <- dataframeSplit$testSet
trainSet <-dataframeSplit$trainSet

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression
glmModel <- glm(formula, data=trainSet, family="binomial")
# Predict the outcomes for the test data
predictedProbabilities.GLM <- predict(glmModel, newdata=testSet, type="response")
###################################################

# Naive Bayes

# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf

# Create Naive Bayes model
nbModel <- naiveBayes(formula, data=trainSet)
# Predict the outcomes for the test data
predictedProbabilities.NB <- predict(nbModel, newdata=testSet, type="raw")

###################################################

# Confusion Matrix

threshold = 0.5

#Confusion Matrix for Logistic Regression
# convert the predicted probabilities to predictions
predicted.GLM <- as.numeric(predictedProbabilities.GLM > threshold)
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
print(table(predicted.GLM, testSet$Severity))

#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions
predicted.NB <- as.numeric(predictedProbabilities.NB[,2] > threshold)  
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")
# create a table to compare predicted values to actual values
print(table(predicted.NB, testSet$Severity))

