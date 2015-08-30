## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: ChicagoDiabetesHospital_homework5.R
##
## Purpose: Prediction model for 1) NUmber of Hospitalization vs Crude Admittance Rate 
##                               2) Delta of NUmber of Hospitalization vs Delta of Crude Admittance Rate
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-07-23
##
## --------------------------------------------
##--------------------------------------------
##
## Chicago Diabetes Hospital data
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 5
##
## Datasets located:https://data.cityofchicago.org/Health-Human-Services/Public-Health-Statistics-Diabetes-hospitalizations/vekt-28b5
##
##
##   Need:
##
##    -Chicago Hospital data: ChicagoDiabetesData.csv
##
##--------------------------------------------


##----Run Main Function Here----
if(interactive()){
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')
  
  ##-----Read in the data-----
  data = read.csv("ChicagoDiabetesData.csv", sep=",",
                  header=TRUE, stringsAsFactors=FALSE)
  
  ##-------Sum up by all zip codes----------
  data_sum = apply(data[-1], 2, sum)
  
  
  ##-------Separate out hospitalizations and crude admittance rate -----
  hospitalizations = data_sum[grepl('^Hospitalizations.[0-9]+$', names(data_sum), perl = TRUE)]
  crude_rate       = data_sum[grepl('^Crude.Rate.[0-9]+$', names(data_sum), perl = TRUE)]
  
  ##-------Running difference of hospitalizations and crude admittance rate ------
  hospitalizations_diffs = diff(hospitalizations)
  crude_rate_diffs = diff(crude_rate)
  
  
  ##-------Fit a linear model of hopitalizations vs crude admittance rate----------
  hosp_cruderate_bestline = lm(hospitalizations~crude_rate)
  
  ##----Plot the best fit line----
  plot(crude_rate, hospitalizations,
       main = 'Hospitalization vs Crude Admittance Rate',
       xlab = 'Crude Admittance',
       ylab = 'Hospitalization')
  abline(hosp_cruderate_bestline, col = 'green')
  grid()
  
  ##---Look at the mean of errors, absolute mean and r-squared value of the model---
  print(paste('Below are some findings of the best fit linear model of hospitalizations in Chicago across all zip code vs Crude admittance rate'))
  print(paste('Mean of errors:', round(mean(hosp_cruderate_bestline$fitted.values - hospitalizations),3)))
  print(paste('Mean of absolute errors:', round(mean(abs(hosp_cruderate_bestline$fitted.values - hospitalizations)), 3)))
  print(paste('R-Squared value:', round(summary(hosp_cruderate_bestline)$r.squared, 4)))

  
  ##---Interpreting the slope of the bets fit line----
  slope_est = summary(hosp_cruderate_bestline)$coefficients[2]
  print(paste('Slope estimate:', round(slope_est, 3)))
  if ( round(slope_est, 3) > 0 ){ # case of positive slope
  print(paste('This implies that every crude_rate increase by 1 hospitalization increases by', round(slope_est, 3) ) )
  } else
    print(paste('This implies that every crude_rate increase by 1 hospitalization decreases by', round(slope_est, 3) ) )
  
  
  ##-------Fit a linear model of changes of hopitalizations vs changes of crude admittance rate----------
  changes_bestline = lm(hospitalizations_diffs ~ crude_rate_diffs)
    
  ##Plot the linear model
  plot(crude_rate_diffs, hospitalizations_diffs,
       main = 'Hospitalization Changes vs Crude Admittance Rate Changes',
       xlab = 'Crude Admittance Changes',
       ylab = 'Hospitalization Changes')
  abline(changes_bestline, col = 'purple')
  grid()
  
  ##---Look at the mean of errors, absolute mean and r-squared value of the model---
  print(paste('Below are some findings of the best fit linear model of CHANGES in hospitalizations in Chicago across all zip code vs CHANGES in Crude admittance rate'))
  print(paste('Mean of errors:', round(mean(changes_bestline$fitted.values - hospitalizations_diffs), 3)))
  print(paste('Mean of absolute errors:', round(mean(abs(changes_bestline$fitted.values - hospitalizations_diffs)), 3)))
  print(paste('R-Squared value:', round(summary(changes_bestline)$r.squared, 4)))
  
  ##---Interpreting the slope of the bets fit line----
  changes_slope_est = changes_bestline$coefficients[2]
    
  if (changes_slope_est > 0) { # case when the estimated slope is positive
    print(paste('Slope',round(changes_slope_est, 3)
                , 'implies when crude rate change increases by 1 hospitalization change increases by'
                , round(changes_slope_est,3)))
  }  else  # case when the estimated slope is negative
    print(paste('Slope',round(changes_slope_est, 3)
                , 'implies when crude rate change increases by 1 hospitalization change decreases by'
                , round(changes_slope_est,3)))

  ##-----Interprete the co-efficient of the two linear models
  print('R-squared value of the changes model is higher than the regular model; this implies regular model is better fitted in the linear regression model than the changes model.')

}