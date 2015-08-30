## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: Homework7_CaesarPrediction.R
##
## Purpose: 
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-08-16
##
## --------------------------------------------
##--------------------------------------------
##
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 7
##
## Datasets located: http://moodle.extn.washington.edu/course/view.php?id=6268 
##                  under Lesson 7: Time Series, Spatial Stats, Intro to Bayesian Stats
##
##
##   Need: 
## 1. JitteredHeadCount.csv
## Jittered Head Count information:
##   
## GameCode: 2 digit game code.
## DateFormat: Date
## Hour: 0-23 of the day
## TabelsOcc:  Number of Tables Occupied in the casino
## TablesOpen: Number of tables open in the casino.  This number can be different that occupied because the pit boss can open a table and no one can be sitting at it.
## Tables Closed: Number of tables closed in the casino.
## HeadCount: Total Count of people at the hour, day, and table type
## DayOfWeek: 1-7.  1 = Monday, 7 = Sunday.
## DayNumber 1-366:  Day of year.  Not really needed, but note that the dates start in September.
##
## 2. las_vegas_hourly_weather.csv
## Las Vegas weather information during 1 year starting from 2011 August 
## taken every hour
## dew_pt
## humidity
## pressure
## visibility
## wind_dir
## wind_speed
## gust_speed
## precipitation
## conditions
## wind_dir_deg
##--------------------------------------------


##-----Load Libraries-----
library(dplyr)
library(data.table)
library(forecast)

if( interactive()){

setwd("C:/Users/saunak/Documents/R/datafiles")

##-----Load Data-----
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)


##-----Format Data----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
names(weather) = c('time','temp','dew_pt','humidity','pressure',
                   'visibility','wind_dir','wind_speed','gust_speed',
                   'precipitation','events','conditions',
                   'wind_dir_deg','date')

weather$datetime = paste(weather$date,weather$time)
weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))

##----Drop Duplicates----
weather = weather[!duplicated(weather[c("date", 'Hour')]),]


##----Merge Data-----
weather$DateFormat = weather$date
weather$date = NULL
weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")

headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))

##----Imputation for NAs in weather-----
numeric_cols = c(11:15, 17:19, 22)
# Linear Interpolation:
headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)

##---Drop character columns----
headcount$wind_dir = NULL
headcount$time = NULL
headcount$datetime = NULL

##-----Deal with events/conditions----
headcount$events[headcount$events == ""] = "None"
headcount$events[is.na(headcount$events)] = "None"
headcount$conditions[is.na(headcount$conditions)] = "None"

##----Format Data for Time Series Exploration-----
daily_headcount= aggregate(headcount$HeadCount ~ headcount$DateFormat, FUN = sum)
names(daily_headcount ) = c("Date", "HeadCount")
plot(x = as.Date(daily_headcount$Date), y = daily_headcount$HeadCount, type = "l"
     , xlab = "Date", ylab = "Headcount")

observed_mean = mean(daily_headcount$HeadCount)
observed_sd = sd(daily_headcount$HeadCount)


# use forecast's ses() function:
exp_smooth1 = ses(daily_headcount$HeadCount, alpha=0.05, h=30) # h is how many t-units to forecast out
exp_smooth2 = ses(daily_headcount$HeadCount, alpha=0.25, h=30)
exp_smooth3 = ses(daily_headcount$HeadCount, alpha=0.95, h=30)

plot(exp_smooth1)
lines(exp_smooth1$fitted, col="blue")
lines(exp_smooth2$fitted, col="green")
lines(exp_smooth3$fitted, col="red")
legend('topleft', c('Original Data','alpha=0.05', 'alpha=0.25', 'alpha=0.95'),
       col=c('black','blue', 'green', 'red'), lty=c(1,1,1))

##------------ 1st Order auto regression -------------------
autoreg_1storder = Arima(daily_headcount$HeadCount, order = c(1,0,0))
print(paste("Standard error for 1st Order auto regression:", sqrt(autoreg_1storder$sigma2)))
print(paste("Standard error of the 1st Order Auto regression is less than the observed standard deviation."))

autoreg_1storder_fit = daily_headcount$HeadCount - autoreg_1storder$residuals
plot(daily_headcount$Date, daily_headcount$HeadCount,type="l", lwd=2)
lines(daily_headcount$Date, autoreg_1storder_fit, col="red", lwd=2, lty=2)

# prediction
autoreg_1storder_pred = predict(autoreg_1storder, n.ahead = 30)

lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      autoreg_1storder_pred$pred, lwd=2, col='green')

# Add in standard error lines
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      autoreg_1storder_pred$pred + autoreg_1storder_pred$se, lwd=2, col='green')
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      autoreg_1storder_pred$pred - autoreg_1storder_pred$se, lwd=2, col='green')


## ------------------1st Order moving average----------------
ma_1storder = Arima(daily_headcount$HeadCount, order = c(1,0,0))
print(paste("Standard error for 1st Order moving average:", sqrt(ma_1storder$sigma2)))
print(paste("Standard error of the 1st Order moving average is less than the observed standard deviation."))

ma_1storder_fit = daily_headcount$HeadCount - ma_1storder$residuals
plot(daily_headcount$Date, daily_headcount$HeadCount,type="l", lwd=2)
lines(daily_headcount$Date, ma_1storder_fit, col="red", lwd=2, lty=2)

# prediction
ma_1storder_pred = predict(ma_1storder, n.ahead = 30)

lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      ma_1storder_pred$pred, lwd=2, col='green')

# Add in standard error lines
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      ma_1storder_pred$pred + ma_1storder_pred$se, lwd=2, col='green')
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      ma_1storder_pred$pred - ma_1storder_pred$se, lwd=2, col='green')

##------------------------2nd Order AR and 1st Order MA-------------------
ar_2ndorder_1stma = Arima(daily_headcount$HeadCount, order = c(1,0,0))
print(paste("Standard error 2nd order AR, 1st Order MA:", sqrt(ar_2ndorder_1stma$sigma2)))
print(paste("Standard error of the 2nd Order AR and 1st Order MA is less than the observed standard deviation."))


ar_2ndorder_1stma_fit = daily_headcount$HeadCount - ar_2ndorder_1stma$residuals
plot(daily_headcount$Date, daily_headcount$HeadCount,type="l", lwd=2)
lines(daily_headcount$Date, ar_2ndorder_1stma_fit, col="red", lwd=2, lty=2)

# prediction
ar_2ndorder_1stma_pred = predict(ar_2ndorder_1stma, n.ahead = 30)

lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      ar_2ndorder_1stma_pred$pred, lwd=2, col='green')

# Add in standard error lines
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      ar_2ndorder_1stma_pred$pred + ar_2ndorder_1stma_pred$se, lwd=2, col='green')
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      ar_2ndorder_1stma_pred$pred - ar_2ndorder_1stma_pred$se, lwd=2, col='green')


##----Double Exponential Smoothing----
double_exp_smooth = Arima(daily_headcount$HeadCount, order = c(0,1,1))
double_exp_fit = daily_headcount$HeadCount - double_exp_smooth$residuals # fitted values
plot(daily_headcount$Date, daily_headcount$HeadCount,type="l", lwd=2)
lines(daily_headcount$Date, double_exp_fit, col="red", lwd=2, lty=2)
print(paste("Standard error double exponential:", sqrt(double_exp_smooth$sigma2)))
print(paste("Standard error of the Double Exponential Smoothing is less than the observed standard deviation."))

# prediction
double_exp_pred = predict(double_exp_smooth, n.ahead = 30)

lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      double_exp_pred$pred, lwd=2, col='green')
# Add in standard error lines
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      double_exp_pred$pred + double_exp_pred$se, lwd=2, col='green')
lines(seq(from=daily_headcount$Date[350], to=daily_headcount$Date[350]+30, by=1)[-1],
      double_exp_pred$pred - double_exp_pred$se, lwd=2, col='green')

}