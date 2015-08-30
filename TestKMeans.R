# TestKMeans.R
# Clear Console:
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

source("KMeans.R")
source("KMeansHelper.R")

print("Test 0:  ClusterPlot")
tryCatch({
  ClusterPlot(observations=sampleObservations, centers=centersGuess, labels=labelsRandom)
  print("True")
}, error=function(e){print("ClusterPlot throws error")}) # tryCatch

print("Test 1:  calculateClusterCenters")
tryCatch({
actualResult<-calculateClusterCenters()
expectedResult <- -matrix(nrow=3, ncol=2, byrow= T, data=c(
0.02392857,  0.02464286,
-0.10321429,  0.10071429,
0.08370370, -0.13000000))
print(sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001)
}, error=function(e){print("calculateClusterCenters with initial values throws error")}) # tryCatch

print("Test 2:  calculateClusterCenters")
tryCatch({  
  clusterLabels = c(
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1,
    3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 2, 2, 2, 1, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 3, 2)
  actualResult<-calculateClusterCenters(observations=sampleObservations, clusterLabels=clusterLabels)
  expectedResult <- -matrix(nrow=3, ncol=2, byrow= T, data=c(
    -0.0332000, -0.6508000,
    -1.5163158, -1.0057895,
    0.7610256,  0.9071795))
  print(sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001)
}, error=function(e){print("calculateClusterCenters with best labels throws error")}) # tryCatch

print("Test 3:  findLabelOfClosestCluster")
tryCatch({
actualResult<-findLabelOfClosestCluster()
expectedResult <- c(
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 1, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3, 3, 3, 3, 2, 2, 3, 3,
  2, 2, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3)
print(sum((actualResult - expectedResult)^2)< 0.0001)
}, error=function(e){print("findLabelOfClosestCluster throws error")}) # tryCatch

print("Test 4:  KMeans standard 3 clusters")
tryCatch({
actualResult<-KMeans(clusterCenters = centersGuess)
expectedResult <- -matrix(nrow=3, ncol=2, byrow= T, data=c(
-0.0332000, -0.6508000,
-1.5163158, -1.0057895,
0.7610256,  0.9071795))
print(sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001)
}, error=function(e){print("KMeans throws error")}) # tryCatch

print("Test 5:  KMeans 4 clusters")
tryCatch({
testCentroids4 <- -matrix(c(0, -1.5, .8, 0, -0.7, -1, 1, 1), nrow=4)
actualResult<-KMeans(clusterCenters = testCentroids4)
expectedResult <- -matrix(nrow=4, ncol=2, byrow= T, data=c(
-0.0332000, -0.6508000,
-1.5163158, -1.0057895,
0.8734286,  0.8557143,
-0.2225000,  1.3575000))
print(sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001)
}, error=function(e){print("KMeans throws error")}) # tryCatch

print("Test 6:  KMeans 2 clusters")
tryCatch({
testCentroids2 <- -matrix(c(-2, 2, -2, 2), nrow=2)
actualResult<-KMeans(clusterCenters = testCentroids2)
expectedResult <- -matrix(nrow=2, ncol=2, byrow= T, data=c(
-0.7385714, -0.8280952,
0.7575610,  0.8482927))
print(sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001)
}, error=function(e){print("Two Clusters Throws Error")}) # tryCatch

print("Test 7:  KMeans 4 clusters; single point cluster")
tryCatch({
singlePointCluster <- -matrix(c(-1.47, -0.01, 0.79, 1.91, -1.04, -0.50, 0.94, 1.43), nrow=4)
actualResult<-KMeans(clusterCenters = singlePointCluster)
expectedResult <- -matrix(nrow=4, ncol=2, byrow= T, data=c(
-1.466500000, -1.0430000,
-0.007777778, -0.4992593,
0.790571429,  0.9402857,
1.910000000,  1.4300000))
print(sum((actualResult[order(actualResult[,1]),] - expectedResult[order(expectedResult[,1]),])^2)< 0.0001)
}, error=function(e){print("Single Point Cluster Throws Error")}) # tryCatch
