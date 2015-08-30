## Load libraries
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

## Functions
get_train_data = function(data=NA, percentage = .7, logger=NA){
  return( data[sample(1:nrow(data), size = nrow(data) * percentage, replace = FALSE),])
}

get_factors = function(data, logger = NA){
  loginfo(paste("get_factors() for", names(data)))
  factors = names(data)
  factors = factors[-which(factors == "url")]
  factors = factors[-which(factors == "popular")]
  return(factors)
}

popular_predict_lasso= function (data, logger=NA){
  
  factors = get_factors(data)
  fmla = as.formula(paste("popular ~ ", paste(factors, collapse= "+")))
  xfactors = model.matrix(factors, data = data)[, -1]
  
  outcomes_lasso = glmnet(x = xfactors, y = as.factor(data$popular), alpha=1, family="binomial")
  
  #plot(outcomes_lasso,xvar="lambda")
  
  ### Cross validate lasso to choose best lambda
  outcomes_lasso_cv = cv.glmnet(x = xfactors, y = as.factor(data$popular), alpha=1, family='binomial')
  plot(outcomes_lasso_cv)
  best_lambda = outcomes_lasso_cv$lambda.min
  loginfo(paste("Cross-validated best lambda:", best_lambda))
  
  
  ## Filter Lasso coefficients using best_lambda as cutoff
  best_coef = coef(outcomes_lasso)[,outcomes_lasso$lambda == best_lambda]
  best_coef = best_coef[best_coef > 0]
  str(best_coef)
  length(best_coef)
  
  loginfo(paste("Applying", best_lambda, "as cut-off to regular Lasso Regression"))
  
  ## Prediction using co-efficients determined by Lasso
  loginfo(paste("Number of coefficients", length(best_coef)))
  xnam = names(best_coef)
  #paste("X", 1:length(best_coef), sep = "")
  fmla = as.formula(paste("popular ~ ", paste(xnam, collapse= "+")))
  
  
  ## Apply logistics regression 
  loginfo("Applying logictic regression using the formula comprising of effective co-efficients ...")
  
  xfactors_test = model.matrix(fmla, data = test_data) [,-1]
  #popular_logit = glmnet( x = xfactors_test, y = as.factor(test_data$popular), alpha=1, family="binomial")
  #loginfo(summary(cancer_logit))
  popular_logit = glm(fmla, data = train_data, family = "binomial")
  
  log_predict_test = predict(popular_logit, newdata = test_data, type="response")
  test_predictions = as.numeric(log_predict_test>0.5)
  test_actuals = test_data$popular
  
  # Confusion Matrix:
  #confusionMatrix(test_predictions, test_actuals)
  
  cutoff = 0.7
  
  prediction = as.numeric(popular_logit$fitted.values>cutoff)
  # loginfo(paste("Original outcomes: ", list(cancer_samples), logger = ""))
  # loginfo(paste("Predicted outcomes:", list(prediction), logger = ""))
  
  pred_popular_actual_popular = sum( (prediction == 1) & (test_data$popular == 1) )
  pred_no_actual_no = sum( (prediction == 0) & (test_data$popular == 0) )
  
  # Maybe say our accuracy is total right out of total:
  accuracy = (pred_popular_actual_popular + pred_no_actual_no)/(nrow(test_data))
  print (paste('Accuracy of our prediction:', accuracy))
  
  # False positives
  pred_popular_actual_no = sum( (prediction == 1) & (test_data$popular == 0) )
  print(paste('False positive:', pred_popular_actual_no))
  
  # False negatives
  pred_no_actual_popular = sum( (prediction == 0) & (test_data$popular == 1) ) 
  print(paste("False negative:", pred_no_actual_popular))
  
}

setwd("C:/Users/saunak/Documents/R/datafiles/OnlineNewsPopularity")

if(interactive()){
  
  basicConfig() # Use default logging configuration
  addHandler(writeToFile, logger="info", file="OnlineNewsPopularity.log") # Add a logging function handler
  
  ## Read data from csv
  data = read.csv("OnlineNewsPopularity.csv", stringsAsFactors = FALSE)
  
  ## Explore data
  summary(data)
  head(data.table(data))
  
  ## How shares is distributed
  boxplot(data$shares)
  hist(data$shares, breaks = 1000)
  
  ## Due to highly skewed data we will consider meadian as popularity threshold
  popularity_threshold = median(data$shares)
  
  data$popular = as.numeric(data$shares > popularity_threshold)
  
  ## What is the probability that a randomly selected url will be popular
  prob_popularity = nrow(data[data$popular == 1,])/ nrow(data)
  print(paste("Probability of a randomly selected url as popular=", prob))
  loginfo(paste("Probability of a randomly selected url as popular=", prob), logger = "info")

  ##----- Lasso regression for prediction model-----
  ##Pick train sample
  train_ix = sample(1:nrow(data), round(0.2*nrow(data)))
  
  train_data = data[train_ix,]
  test_data = data[-train_ix,]
  data.table(train_data)

## Let's consider token factors ONLY for predicting popularity
  loginfo(paste("Logistics Linear regression with token factors START!"))
  factors = names(data)
  factors = factors[-which(factors == "url")]
  factors = factors[-which(factors == "popular")]


  fmla = as.formula(paste("popular ~ ", paste(factors, collapse= "+")))
  log_model = glm(fmla, data = train_data, family = "binomial")
  
  plot(log_model)
  
  log_predict_test = predict(log_model, newdata = test_data, type="response")
  test_predictions = as.numeric(log_predict_test>0.5)
  test_actuals = test_data$popular
  
 # Confusion Matrix:
  confusionMatrix(test_predictions, test_actuals)
  
  print(paste("Tokens itself does not much influence the popularity."))
  loginfo(paste("Logistics Linear regression with channel factor DONE!"))
  
  
  # Change the cutoffs
  cutoff_seq = seq(0.001, 0.999, length = 100)
  
  true_pos_rate = sapply(cutoff_seq, function(x){
    log_predictions = as.numeric(log_model$fitted.values>x)
    true_pos = sum((test_predictions==1) & (test_actuals==1))/sum(test_actuals==1)
  })
  
  false_pos_rate = sapply(cutoff_seq, function(x){
    log_predictions = as.numeric(log_model$fitted.values>x)
    false_pos = sum((test_predictions==1) & (test_actuals==0))/sum(test_actuals==1)
  })
  
  plot(false_pos_rate, true_pos_rate, type="l", xlim=c(0,1), ylim=c(0,1))
  abline(0,1)

}