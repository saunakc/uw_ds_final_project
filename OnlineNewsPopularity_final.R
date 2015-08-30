## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: OnlineNewsPopularity_latest.R
##
## Purpose: Run statistical linear regression on the dataset of online news popularity to 
##         produce a predictive model. The dataset summarizes a heterogeneous set of features about 
##         articles published by Mashable in a period of two years. 
##         The goal is to identify features with more influence in predicting the popularity
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-08-22
##
##
## Datasets located: http://archive.ics.uci.edu/ml/machine-learning-databases/00332/
## Dataset Description: http://archive.ics.uci.edu/ml/datasets/Online+News+Popularity#
##
## Need : OnlineNewsPopularity.csv
## Log:   OnlineNewsPopularity.log
##
##--------------------------------------------
#---------Clear objects from Memory-----------
rm(list=ls())
# Clear Console:
cat("\014")
# Gurbage collection
gc()## Load libraries
library(logging)
library(data.table)
library(plyr)
library(glmnet)
library(gdata)

library(boot)
library(ggplot2)
library(MASS)
library(caret)
library(e1071)
library(ROCR)

setwd("C:/Users/saunak/Documents/R/datafiles/OnlineNewsPopularity")

## Functions
get_factors = function(data, logger = NA){
  loginfo(paste("get_factors()"))
  factors = names(data)
  ## Ignore non-predictive factors and the target
  factors = factors[-which(factors == "url")]
  factors = factors[-which(factors == "shares")]
  factors = factors[-which(factors == "timedelta")]
  factors = factors[-which(factors == "popular")]
  return(factors)
}

popular_predict_lasso = function (data, logger=NA){
  ##Pick train sample
  loginfo(paste("Start of linear regression prediction"), logger = "info")
  print(paste("Linear regression using training sample of 90% of the online news data."))
  train_ix = sample(1:nrow(data), round(0.9 * nrow(data)))
  train_data = data[train_ix,]
  test_data = data[-train_ix,]
  
  loginfo(paste("Calling get_factors()"), logger = "info")
  factors = get_factors(data)
  fmla = as.formula(paste("popular ~ ", paste(factors, collapse= "+")))
  xfactors = model.matrix(fmla, data = data)[, -1]
  loginfo(paste("model.matrix() done!"), logger = "info")
  print(paste("Running Lasso Regression."))
  outcomes_lasso = glmnet(x = xfactors, y = as.factor(data$popular), alpha=1, family="binomial")
  loginfo(paste("glmnet() done!"), logger = "info")
  plot(outcomes_lasso)
    
  ### Cross validate lasso to choose best lambda
  set.seed(40)
  loginfo(paste("Running cross validation."), logger = "info")
  outcomes_lasso_cv = cv.glmnet(x = xfactors, y = as.factor(data$popular), alpha=1, family='binomial')
  loginfo(paste("cv.glmnet() done!"), logger = "info")
  print(paste("Plotting Lasso cross validation results."))
  loginfo(paste("Plotting cross validation results."), logger  = "info")
  plot(outcomes_lasso_cv)
  plot(outcomes_lasso_cv$glmnet.fit, "norm", label = TRUE)
  plot(outcomes_lasso_cv$glmnet.fit, "lambda", label = TRUE)
  best_lambda = outcomes_lasso_cv$lambda.min
  print(paste("Cross-validated best lambda:", best_lambda))
  
  
  ## Filter Lasso coefficients using best_lambda as cutoff
  loginfo(paste("Best lambda:", best_lambda), logger = "info")
  best_coef = coef(outcomes_lasso)[,outcomes_lasso$lambda == best_lambda]
  best_coef = best_coef[best_coef > 0]
  
  print(paste("Applying", best_lambda, "as cut-off to regular Lasso Regression"))
  
  ## Prediction using co-efficients determined by Lasso
  print(paste("Number of coefficients", length(best_coef)))
  xnam = names(best_coef)
  print(paste("And the factors are"))
  cat(paste(xnam), sep = "\n")
  fmla = as.formula(paste("popular ~ ", paste(xnam, collapse= "+")))
      
  ## Apply logistics regression 
  print(paste("Applying logictic regression using the formula comprising of effective co-efficients ..."))
  
  popular_logit = glm(fmla, data = train_data, family = "binomial")
  loginfo("glm() completed!", logger  = "info")
  test_pred = predict(popular_logit, test_data, type = "response")
  loginfo("predict() done!", logger  = "info")
  cutoff = .5
  prediction = as.numeric(test_pred > cutoff)
  actuals = test_data$popular
  
  ## Confusion Matrix:
  conf_mat= confusionMatrix(prediction, actuals)
  loginfo("confusionMatrix() done!", logger  = "info")
  conf_mat
  sensitivity = conf_mat$byClass[1]
  specificity = conf_mat$byClass[2]
  accuracy = conf_mat$overall[1]
  print(paste("-----Prediction results---------"))
  print(paste("Accuracy = ", round(accuracy,2)))
  print(paste("Sensitivity = ", round(sensitivity, 2)))
  print(paste("Specificity = ", round(specificity, 2)))
  
  logistic_prediction = prediction(popular_logit$fitted.values, train_data$popular)
  loginfo("prediction() done!", logger  = "info")
  
  auc_logistic = performance(logistic_prediction, measure = 'auc')
  loginfo("performance() measure done!", logger  = "info")
  AUC = slot(auc_logistic, "y.values")[[1]]
  print(paste("Area under curve (AUC)", round(AUC, 2)))
  print(paste("--------------------------------"))
  loginfo("End of Linear regression prediction.", logger  = "info")
    
}

if(interactive()){
  
  basicConfig() # Use default logging configuration
  addHandler(writeToFile, logger="info", file="OnlineNewsPopularity.log") # Add a logging function handler
  
  ## Read data from csv
  data = read.csv("OnlineNewsPopularity.csv", stringsAsFactors = FALSE)
  loginfo(paste("Data file OnlineNewsPopularity.csv successfully read!"), logger = "info")
  
  ##-----------Explore data-------------------------------------
  summary(data)
  head(data.table(data))
  
  print(paste("Number of cases:", nrow(data)))
  print(paste("Number of attributes:", ncol(data)))
  print(paste("Number of attributes with character col:",  length(which(sapply(data, FUN="class") == "character") )))
  print(paste("Number of attributes with numeric col:",  length(which(sapply(data, FUN="class") == "numeric") )))
  print(paste("Number of attributes with integer col:",  length(which(sapply(data, FUN="class") == "integer") )))
  
  ## How shares is distributed
  print(paste("------Output variable \"shares\" distribution------"))
  print(paste("Mean        Median           IQR          Range"))
  print(paste(round(mean(data$shares)), "      ", round(median(data$shares)), "       ", round(IQR(data$shares)), "       ", range(data$shares)[1], "-", range(data$shares)[2]))
  
  print(paste("Plotting shares vs weekend"))
  print(paste("Boxplot of shares"))
  p <- ggplot(data, aes(factor(is_weekend), shares))
  p + geom_boxplot(outlier.colour = "red") + labs(title = "Shares vs Weekend(Y/N)") 
  
  print(paste("Histogram of shares"))
  m <- ggplot(data, aes(x=shares))
  m + geom_histogram(binwidth = 30) + scale_x_sqrt() + labs(title = "Histogram of Mashable news article shares") 
  loginfo(paste("Data exploration using gglplot done!"), logger = "info")
  
  print(paste("Plotting pairs"))
  print(paste("Variables: num_imgs + num_videos + data_channel_is_socmed + n_tokens_content +  is_weekend"))
  pairs(shares ~ num_imgs + num_videos + data_channel_is_socmed + n_tokens_content +  is_weekend, data = data)
  loginfo(paste("Scatterplot using pairs() presented!"), logger = "info")
  
  ##----------------- End Explore data----------------------------
  
  ##---Define popular ----
  ## Due to highly skewed shares we will consider median as popularity threshold
  popularity_threshold = median(data$shares)
  data$popular = as.numeric(data$shares > popularity_threshold)
  
  ##----- Lasso regression for prediction model-----
  loginfo(paste("Calling popular_predict_lasso"), logger = "info")
  system.time(popular_predict_lasso(data, logger = "info"))
}