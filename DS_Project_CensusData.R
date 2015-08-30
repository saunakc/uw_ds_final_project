##--------------------------------------------
##
## Lecture 8 R methods
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

setwd("C:/Users/saunak/Documents/R/datafiles/CensusData/pums")

library(boot)
library(ggplot2)
library(MASS)
library(caret)
library(e1071)
library(ROCR)

housing_a_data = read.csv("ss13husa.csv", stringsAsFactors= FALSE)

str(housing_a_data)

range(housing_a_data$ADJHSG)
summary(housing_a_data$BUS)

barplot(housing_a_data$RT)

hist(housing_a_data$REGION)
