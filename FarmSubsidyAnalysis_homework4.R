## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: FarmSubsidyAnalysis_homework4.R
##
## Purpose: Test if 1) all states in 2011 Farm Subsidy data are equally represented and 
##                  2) all states in 2011 Farm Subsidy data are represented by farms/state equally.
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-07-18
##
## --------------------------------------------
##--------------------------------------------
##
## Test Farm-Subsidies Data set
##
## Class: PCE Data Science Methods Class
##
## Purpose: Framework for Homework 3
##
## Datasets located:
##
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi
##
##   Need:
##
##    -2011 Farm Payment File (27MB) txt file
##    -State and County Code List (374KB) xls file (probably convert this to csv)
##
##--------------------------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)

##----Hypotheses to test-----
#
#  Test these two things:
#
#    1.  Does our sample equally represent all 50 states?
#
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
#
#     Note- you can find the farms per state in census data.
#

trim = function (x) gsub("^\\s+|\\s+$", "", x)

##-----Declare Functions Here-----
chisquare_proportion = function (occurence) {
  Xsqr = chisq.test(occurence)
  }

##----Run Main Function Here----
if(interactive()){
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')
  
  ##-----Read in the data-----
  data = read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";",
                  header=FALSE, stringsAsFactors=FALSE)
  require(logging)
  loginfo(paste0("Data file with farmland data is read."))
  ##----Trim Whitespaces-----
  data = as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)
  
  names(data) = c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  ##------Read State/County File-----
  county_state_codes = read.csv("foia_state_county_codes.csv", stringsAsFactors=FALSE)
  loginfo(paste0("State/ County code file data is read."))
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together----
  data = merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
  loginfo(paste0("Data file is merged with state/ county code file"))
  summary(data)
  data$state = data$ST
  summary(data)
  data$ST = NULL
  data$city = data$City
  data$City = NULL

  ##-----Probably do some data exploration----
  ##-----What fiscal years are represented in this data ---------
  require(dplyr)
  loginfo(paste0("Starting exploring the payment amount received by each state in 2011 fiscal year"))
  data %>% distinct(calendar_year) %>% select (calendar_year)
  unique(data$calendar_year)
  sort(unique(data$program_year))
  length(data$program_year == "0")
   
  ## -----Show payments received by states
  data$amount = as.numeric(data$amount)
  data$amount[is.na(data$amount)] = 0
  amount_by_state = aggregate(amount ~ state, data, sum)
  amount_by_state$amount_1000 = amount_by_state$amount/1000
  
  print(paste0("Largest fund received by any state is: ", max(amount_by_state$amount_1000), "K and Minimum fund received is: ", min(amount_by_state$amount_1000), "K."))
  print(paste0("Payment received by states- Mean= ", mean(amount_by_state$amount_1000), "K Median= ", median(amount_by_state$amount_1000), "K"))
    
  
  require(ggplot2)
  qplot(x=state, y=amount_1000, 
        data=amount_by_state, geom="bar", stat="identity", color="blue",
        position="dodge") + 
        ggtitle("State wise Payment") + xlab("State") + ylab("Payment Amount (K)") +
        geom_hline(yintercept=mean(amount_by_state$amount_1000), color="red") + 
        geom_hline(yintercept=median(amount_by_state$amount_1000), color="orange")

  ##---IA, IL, NE, KS, IN are top five states received payments
  
    
  ##----Perform a test for equal representation-----
  ##----Null hypothesis: sample is equal representation of all 50 states
  require(plyr)
  state_df = count(data$state)
  summary(state_df)
  names(state_df) = c("state", "occurence")
  summary(state_df)
  chisq.test(state_df$occurence)
  Xsqr = chisquare_proportion(state_df$occurence)
  summary(Xsqr)
  ifelse ((Xsqr$p.value < 0.09),
    print("There is not enough evidence to state that the sample represents 50 states equally.")
  , print("We can state that the sample represents 50 states equally.")
  )
  loginfo(paste0("Chi square is run on states data."))
  
  
  ##----Access the farms/state data-----
  farms_per_state = read.csv("farms_per_state.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
  
  ##----Trim Whitespaces-----
  farms_per_state = as.data.frame(apply(farms_per_state,2,trim), stringsAsFactors=FALSE)
  summary(farms_per_state)
  names(farms_per_state) = c("state_name", "state", "fips", "farms")
  farms_per_state$farms = as.numeric(farms_per_state$farms)
  farms_per_state = farms_per_state[complete.cases(farms_per_state), ]
    
  ##----Perform a test for equal repreentation by farms/state-----
  Xsq = chisquare_proportion(farms_per_state$farms)
  Xsq$p.value
  loginfo(paste0("Chi square is run on farms/state."))
  ifelse ((Xsq$p.value < 0.09),
          print("There is not enough evidence to state that the sample represents 50 states equally by farms per state.")
          , print("We can state that the sample represents 50 states equally.")
  )
}


