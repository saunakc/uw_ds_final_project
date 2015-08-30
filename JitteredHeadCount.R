## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: Homework_01_Saunak.R
##
## Purpose: Explore 'JitteredHeadCount.csv', a data set from Caesar's Entertainment that has falsified/jittered table headcounts
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-06-25
##
## --------------------------------------------

## Jittered Head Count information:
## GameCode: 2 digit game code.
## DateFormat: Date
## Hour: 0-23 of the day
## TabelsOcc:  Number of Tables Occupied in the casino
## TablesOpen: Number of tables open in the casino.  This number can be different that occupied because the pit boss can open a table and no one can be sitting at it.
## Tables Closed: Number of tables closed in the casino.
## HeadCount: Total Count of people at the hour, day, and table type
## DayOfWeek: 1-7.  1 = Monday, 7 = Sunday.
## DayNumber 1-366:  Day of year.  Not really needed, but note that the dates start in September.


##----Import Libraries-----
require(logging)
require(ggplot2)

#Load data file declaration

load_data = function (datafile='JitteredHeadCount.csv', logger=NA){
  data = read.csv(datafile)
  loginfo ("Data loaded into frame.", logger="data_logger")
  
  #Check if data got loaded
  if(nrow(data)==0){
    logwarn("No data loaded", logger="data_logger")
  }
  
  return (data)
}


if(interactive()){
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, logger="data_logger", file="file.log")  
  
  ## Setting working directory to load the data from 
  loginfo("Setting working directory.", logger="data_logger")
  
  setwd("C:/users/saunak/Documents")
  datafile="JitteredHeadCount.csv"
  data=load_data(datafile, "JitterHeadCount_interactive_logger")
  
  #Look at the type of data in the file
  print("Checking the data types")
  str(data)
  
  #Analysis 1: Check what are different number of open tables in an hour
  print("What are different values of TablesOpen.")
  summary(data$TablesOpen)
  hist(data$TablesOpen, right = FALSE, breaks=c(0:25),
       main = "Open Tables Frequency distribution", xlab="Number of Tables Open", ylab="Frequency of Open Table count")
  
  
  #Analysis 2: Check which game codes have most number average open tables per hour
  
  print("For which GameCodes there are at least one table open during an hour")
  df.opentable_by_hour <- data[data$TablesOpen != 0,]
  gamecode_ot_mean = aggregate(df.opentable_by_hour$TablesOpen ~ df.opentable_by_hour$GameCode, FUN=mean)
      
  plot(gamecode_ot_mean, type='l', main="Mean Open Tables by GameCode", labels=TRUE,
       xlab="Game Code", ylab="Mean Open Tables", xaxt="n")
  str(gamecode_ot_mean)
  names(gamecode_ot_mean) <- c("GameCode", "MeanOpenTables")
  
  print(ggplot(data=gamecode_ot_mean, aes(x=GameCode, y=MeanOpenTables)) +
          geom_bar(aes(fill=MeanOpenTables),stat="identity") + 
          xlab("GameCode") +
          ylab("Mean Open Tables") +
          ggtitle("Mean Open Table by Game") + 
          geom_text(aes(label = round(MeanOpenTables,1),y=MeanOpenTables+1), size=3))
  
  
  #Analysis 3: Tables open by hour
  
  print("Hourly open tables irrsepective of game")
  hourly_ot_mean = aggregate(df.opentable_by_hour$TablesOpen ~ df.opentable_by_hour$Hour, FUN=mean)
  names(hourly_ot_mean) <- c("Hour", "MeanOpenTables")
  print(hourly_ot_mean)
  
  print(ggplot(data=hourly_ot_mean, aes(x=Hour, y=MeanOpenTables)) +
          geom_bar(aes(fill=MeanOpenTables),stat="identity") + 
          xlab("Hour of Day") +
          ylab("Average Number Open Tables") +
          ggtitle("Average Open Table by Hour") + 
          geom_text(aes(label = round(MeanOpenTables,1),y=MeanOpenTables+.25), size=3))
  
}