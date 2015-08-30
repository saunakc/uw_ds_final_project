## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: Cancer_Gene_Predictor.R
##
## Purpose: From a dataset of 10000 sample gene expression apply Lasso Regression 
##          on 19 samples to come up with most influential genes.
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-08-06
##
## --------------------------------------------
##--------------------------------------------
##
## MicroArray.txt
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 6
##
## Datasets located:
##
##
##   Need:
##
##    - http://www.ncbi.nlm.nih.gov/pubmed/21532620
##
##--------------------------------------------

##Load libraries
require(glmnet)


##Define function to read MicroArray.txt
read_microarray = function(){

## Read MicroArray.txt
data_microarray = read.table("MicroArray.txt", header = TRUE)
}

if (interactive()){
  
  library(logging)
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/cancer_gene_prediction.log", level='DEBUG')
  
  
data = read_microarray() 

##Look at the data
dim(data)
str(data)

##Normalize each column
data= scale(data)

# Breast Cancer Samples:
cancer_samples = c(0,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1)

## Transpose observation factors into columns
data_frame = data.frame(t(data))

## Add cancer sample outcomes to data frame
data_frame$outcomes = cancer_samples

##Apply Lasso Regression on the outcome
set.seed(164)
xfactors = model.matrix(data_frame$outcomes ~ . - data_frame$outcomes, data = data_frame)[, -1]
outcomes_lasso = glmnet(x = xfactors, y = as.factor(data_frame$outcomes), alpha=1, family="binomial")

plot(outcomes_lasso,xvar="lambda")

### Cross validate lasso to choose best lambda
outcomes_lasso_cv = cv.glmnet(x = xfactors, y = as.factor(data_frame$outcomes), alpha=1, family='binomial')
plot(outcomes_lasso_cv)
best_lambda = outcomes_lasso_cv$lambda.min
loginfo(paste("Cross-validated best lambda:", best_lambda))


## Filter Lasso coefficients using best_lambda as cutoff
best_coef = coef(outcomes_lasso)[,outcomes_lasso$lambda == best_lambda]
best_coef = best_coef[best_coef > 0]
str(best_coef)

loginfo(paste("Applying", best_lambda, "as cut-off to regular Lasso Regression"))

## Prediction using co-efficients determined by Lasso
loginfo(paste("Number of coefficients", length(best_coef)))
xnam = names(best_coef[2:5])
#paste("X", 1:length(best_coef), sep = "")
fmla = as.formula(paste("data_frame$outcomes ~ ", paste(xnam, collapse= "+")))


## Apply logistics regression 
loginfo("Applying logictic regression using the formula comprising of effective co-efficients ...")
cancer_logit = glm(fmla, data = data_frame, family = binomial)
#loginfo(summary(cancer_logit))

# See what we predicted:
loginfo("Now it's time to show the prediction results using cutoff 0.5")
cutoff = 0.5
prediction = as.numeric(cancer_logit$fitted.values>cutoff)
loginfo(paste("Original outcomes: ", list(cancer_samples), logger = ""))
loginfo(paste("Predicted outcomes:", list(prediction), logger = ""))

pred_cancer_actual_cancer = sum( (prediction == 1) & (data_frame$outcomes == 1) )
pred_no_actual_no = sum( (prediction == 0) & (data_frame$outcomes == 0) )

# Maybe say our accuracy is total right out of total:
accuracy = (pred_cancer_actual_cancer + pred_no_actual_no)/(nrow(data_frame))
loginfo(paste('Accuracy of our prediction:', accuracy))

# False positives
pred_cancer_actual_no = sum( (prediction == 1) & (data_frame$outcomes == 0) )
loginfo(paste('False positive:', pred_cancer_actual_no))

# False negatives
pred_no_actual_cancer = sum( (prediction == 0) & (data_frame$outcomes == 1) ) 
loginfo(paste("False negative:", pred_no_actual_cancer))
}

