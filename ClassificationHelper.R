# ClassificationHelper.R

###################################################

# Get and Prepare Data

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/mammographic-masses/mammographic_masses.data"
# Download a rectangular dataset without a header to create a dataframe
dataframe <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)
# Assign a vector of real names to the data frame;  The following names were from:
# http://archive.ics.uci.edu/ml/machine-learning-databases/mammographic-masses/mammographic_masses.names
headers <- c("BIRADS", "Age", "Shape", "Margin", "Density", "Severity")
names(dataframe) <- headers

# Clean the data
# Create numeric vectors from factors.  Warning, non-numeric data are converted to NA
suppressWarnings(dataframe <- data.frame(sapply(dataframe, as.numeric)))
# Remove all NA
dataframe <- na.omit(dataframe)
dataframe <- dataframe[order(-dataframe$Age), ]
dataframe$BIRADS[dataframe$BIRADS > 6] <- 6

# Shape the data
formula <- Severity ~ BIRADS + Age + Shape + Margin + Density

###################################################

# Get the algorithm

reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(e1071)

###################################################

# Data Partition functions

BadPartition <- function(dataSet, fractionOfTest = 0.3)
{
  numberOfRows <- nrow(dataSet)
  numberOfTestRows <- fractionOfTest * numberOfRows
  testFlag <- 1:numberOfRows <= numberOfTestRows
  testSet <- dataSet[testFlag, ]
  trainSet <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainSet=trainSet, testSet=testSet)
  return(dataSetSplit)
}

ExactPartition <- function(dataSet, fractionOfTest = 0.3)
{
  randoms <- runif(nrow(dataSet))
  sortedRandoms <- sort(randoms)
  testFlag <- sortedRandoms[length(sortedRandoms)*fractionOfTest]
  testSelection <- randoms <= testThreshold
  testData <- dataSet[testSelection,]
  testData <- dataSet[!testSelection,]
  dataSetSplit <- list(trainset = trainData, testSet=testData)
  return(dataSetSplit)
}

FastPartition <- function(dataSet, fractionOfTest = 0.3)
{
  randoms <- runif(nrow(dataSet))
  testSelection <- randoms <= fractionOfTest
  testData <- dataSet[testSelection,]
  trainData <- dataSet[!testSelection,]
  dataSetSplit <- list(trainSet = trainData, testSet = testData)
  return(dataSetSplit)
}

###################################################
