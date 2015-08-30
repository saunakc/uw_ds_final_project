## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: Homework_02_Saunak.R
##
## Purpose: Combine JitteredHeadCount.csv with weather underground data in Las Vegas and explore temperature and humidity factors on Casino headcount
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-07-05
##
## --------------------------------------------

setwd("C:/Users/saunak/Documents")

##-----Load Libraries-----
library(dplyr)
library(data.table)
library(ggplot2)

source('weather_retrieval.R')

# Load jittered Data
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")

weather_file_name = 'las_vegas_hourly_weather.csv'

# Let's test if the file is in the directry using list.files()
#        If it is, load that file instead of running webscraper
# Saunak: 7/1/2015
weather_df = tryCatch( if ( list.files(pattern = weather_file_name, ignore.case = TRUE) != "" ) {
                             read.csv(weather_file_name, stringsAsFactors = FALSE)
          },
         warn = function(e)
         {
           print("Warning: possibly more than one file found." )
           return (data.frame())
         },
         error = function(e)
         {
           print("Error: can not find weather file ")
           return (data.frame())
        }
)

if( length(weather_df) == 0 ){
  print("Data frame is empty. Run webscrapper to load the data frame.")

# Nick's code below instead of re-inventing the wheel
# If it isn't, run webscraper:
# Look at dates:
range(headcount$DateFormat)

airport = 'KLAS'
dates = seq(from=min(headcount$DateFormat),
            to=max(headcount$DateFormat),
            by=1)

dates = seq(from=as.Date("2011/08/01", format="%Y/%m/%d"),
            to=as.Date("2012/07/31", format = "%Y/%m/%d"),
            by=1)
weather_df = get_weather_data(airport, dates)
names(weather_df) = c('time','temp','dew_pt','humidity','pressure',
                        'visibility','wind_dir','wind_speed','gust_speed',
                        'precipitation','events','conditions',
                        'wind_dir_deg','date')

# Let's create a datetime in the weather data
weather_df$datetime = paste(weather_df$date,weather_df$time)
weather_df$datetime = strptime(weather_df$datetime, format="%Y-%m-%d %I:%M %p")
weather_df$Hour = as.numeric(format(round(weather_df$datetime, units="hours"), format="%H"))

write.csv(weather_df, file = 'las_vegas_hourly_weather.csv', row.names=FALSE)
}

# Let's merge with different methods.
#   - If we were truly merging to analyze casino data, we don't want to lose
#      headcount data if weather doesn't exist, so we want to do a
#      left merge (keeping all the headcount data)
#
#   - Remember, we want to merge on date AND hour.
#   - Note: the headcount data has multiple rows for these (more than 1 game type)

# Check for duplicates!
anyDuplicated(headcount[c("DateFormat", "Hour","GameCode")])

anyDuplicated(weather_df[c("date", 'Hour')]) # Oh no!  How could this happen?

# Drop for now:
weather_df= weather_df[!duplicated(weather_df[c("date", 'Hour')]),]

# Rename some columns:
intersect(names(headcount), names(weather_df))
weather_df$DateFormat = as.Date(weather_df$date, format="%Y-%m-%d")
weather_df$date = NULL


# Pick one of the below merges, and comment out the other two.
# Merge (base)
headcount_base_all = merge(headcount, weather_df, all.x=TRUE, by=c("DateFormat","Hour"))


# Merge(data.table)
# Note that data.table has a problem.  It canNOT take POSIX values. So we drop it (we are done with that column anyways)
weather_df$datetime = NULL
library(data.table)
headcount = as.data.table(headcount)
weather_df = as.data.table(weather_df)


# Set keys for faster merges
setkeyv(headcount, c("DateFormat", "Hour"))
setkeyv(weather_df, c("DateFormat", "Hour"))

headcount_dt_all = merge(headcount, weather_df, all.x=TRUE, by=c("DateFormat", "Hour"))

# Merge(dplyr)
library(dplyr)
headcount_dplyr_all = left_join(headcount, weather_df, by=c("DateFormat", "Hour"))


##----Find another insight involving weather------

# For now, drop all NA rows:
#     use the command 'complete.cases'
# Use 'complete.cases()' as a row filter on your data frame.
# Total headcount per day
headcount_dplyr_all <= complete.cases(headcount_dplyr_all)
total_hc_per_day = aggregate ( headcount_dplyr_all$HeadCount ~ headcount_dplyr_all$DayNumber 
                             + headcount_dplyr_all$DateFormat, FUN=sum)
names(total_hc_per_day) = c("DayNumber" , "DateFormat", "HeadCount")


#Average temp per day
avg_temp_by_day = aggregate( headcount_dplyr_all$temp ~ headcount_dplyr_all$DayNumber 
                             + headcount_dplyr_all$DateFormat, FUN=mean )
names(avg_temp_by_day) = c("DayNumber" , "DateFormat", "temp")

# Round the temparature
avg_temp_by_day$temp_bin = round(avg_temp_by_day$temp)

#Join avg temp with total headcount
temp_with_headcount = inner_join(avg_temp_by_day, total_hc_per_day, by=c("DateFormat", "DayNumber"))

avg_headcount_temp_bin = aggregate(temp_with_headcount$HeadCount ~ temp_with_headcount$temp_bin, FUN=mean)
names(avg_headcount_temp_bin) = c("temp_bin", "HeadCount")

#plot(avg_headcount_temp_bin, main = "Heacount vs temp plot", xlab = "Temparature", ylab = "Avg HeadCount", type = 'l')
#grid()
# Look at some key information of the temparature vs headcount analysis
min(avg_headcount_temp_bin$temp_bin)
max(avg_headcount_temp_bin$temp_bin)
mean(avg_headcount_temp_bin$HeadCount)
ggplot(avg_headcount_temp_bin, aes(x = temp_bin, y = HeadCount)) + geom_line()  + 
  geom_smooth() + 
  ggtitle("Daily Headcount vs Temprature") + labs( x = "Temparature", y = "Head Count")



# Next we are going to show how temparature of LAS influences type of top games played by customers

avg_headcount_by_game_temp = aggregate(headcount_dplyr_all$HeadCount ~ headcount_dplyr_all$GameCode + headcount_dplyr_all$temp, FUN=mean)
names(avg_headcount_by_game_temp) = c("GameCode", "temp", "AvgHeadcount")

ggplot(avg_headcount_by_game_temp, aes(x = temp, y = AvgHeadcount, color=GameCode)) + 
  geom_line(shape=1) + geom_smooth() +
  ggtitle("Game Codes Popularity Trend by Temparature") + labs( x = "Temparature", y = "Average Headcount")
 

# Next we are going to see how temparature and humidity influence the headcount
avg_humidity_by_day = aggregate( headcount_dplyr_all$humidity ~ headcount_dplyr_all$DayNumber 
                                                 + headcount_dplyr_all$DateFormat, FUN=mean )

names(avg_humidity_by_day) = c("DayNumber" , "DateFormat", "humidity")
min(avg_humidity_by_day$humidity)
max(avg_humidity_by_day$humidity)
mean(avg_humidity_by_day$humidity)
ggplot(data=avg_humidity_by_day, aes(avg_humidity_by_day$humidity)) + 
  geom_histogram(breaks=seq(0, 80, by =5), 
                 col="green", 
                 fill = 'blue') + 
  ggtitle("Histogram of Humidity") + labs( x = "Humidity", y = "# of Days")

temp_with_headcount = inner_join(temp_with_headcount, avg_humidity_by_day, by=c("DateFormat", "DayNumber"))

ggplot(temp_with_headcount, aes(x = temp, y = HeadCount)) + 
  geom_point(colour = "orange", aes(size = humidity))  + geom_smooth() + 
  ggtitle("HeadCount vs Temparature & Himidity") + labs( x = "Temparature", y = "Average Headcount")
