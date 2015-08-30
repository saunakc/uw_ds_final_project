## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: FacebookEdges_homework6.R
##
## Purpose: Load Facebook edges file and compute the mean degree and 
##          run K-S test of the mean degree for Poisson distribution 
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-07-29
##
## --------------------------------------------
##--------------------------------------------
##
## facebook_edge_list.csv
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
##    -Facebook edges data: facebook_edge_list.csv
##
##--------------------------------------------

##Load libraries
require(plyr)
require(logging)
require(igraph)

##Define functions
read_facebook_edges = function(path=NA, log_info=NA){
  ##
  if ( ! is.na(path) ){ # Read from home directory
    setwd(path)
    }
  fb_edges  = tryCatch( read.csv("facebook_edge_list.csv", stringsAsFactors=FALSE),
                                     warn = function(e){
                                       print(paste('Warning:', e) )
                                     },
                                     error = function(e)
                                     {
                                       print(paste('Error:', e) )
                                     }
  )
}


##---- Create k-s statistic function------
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}


##----Run Main Function Here----
if(interactive()){
  library(logging)
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')
  
  ##-----Read in the data and explore its structure-----
  data = read_facebook_edges()
    
  ##-----Find the degree of edges-----
  library(plyr)
  source_edges = count(data, 'Source') ## How many times source appears in the list is its degree
  names(source_edges) = c('Source', 'Degrees')
  degree_list = source_edges$Degrees
  
  
  ##Plot the histogram of Facebook connection edges degrees
  hist(degree_list, main = "Histogram of Facebook Connection Degree",
       xlab = "Degree", 
       ylab = "Frequency")
  
  
  ##----Mean of degrees-----------
  mean_degree = round(mean(degree_list), 3)
  
  ##----Print the mean degree-------
  print(paste("Mean degree of connection edges:", mean_degree))
    
  ##---K-S testing of this distribution starts here---
  ##---Comparing this distribution with Poisson distribution
  
  ##---Take a random Poisson sample---
  n = 1000
  pois_sample = rpois(n, lambda = mean_degree)
  
  ##---Plot the poisson sample along with actual data---
  hist(pois_sample)
  
  plot(ecdf(pois_sample), col = "green")
  lines(ecdf(degree_list), col = "red")
  
  #------ Standardize the x-values--------
  x_seq <- seq(0, 20,len=10000)
  y_cdf1 <- sapply(x_seq, function(x){
    sum( pois_sample < x)/ length(pois_sample)
  })
  y_cdf2 <- sapply(x_seq, function(x){
    sum( degree_list < x)/ length(degree_list)
  })

  plot(x_seq,y_cdf1, col='blue', pch=16)
  points(x_seq,y_cdf2,col='red', pch=16)
  grid()
  
  ##--- Find K-S stat against my sample ---
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  print(paste("K-S stat of facebook connection using varioud methods"))
  print(paste("K-S stat =", round(k_s_stat,3), "using crude method against a Poisson sample of n =", n , "and lambda =", mean_degree))
  # where does it occur?
  k_index = which.max(abs(y_cdf1-y_cdf2))
  k_s_x = x_seq[k_index]
  # Add to plot
  lines(c(k_s_x,k_s_x), c(y_cdf1[k_index],y_cdf2[k_index]),
        col='black', lwd=2)
  
  ks_test_outcome = ks.test(degree_list, pois_sample)
  print(paste("K-S stat =", round(ks_test_outcome$statistic[1], 3), "using ks.test() at a p-value of",ks_test_outcome$p.value))
  
  ## ---- Repeat N times against random lambda between 1 and 100----
  N = 500
  k_s_rep = lapply(1:N, function(i){
    dist_a = rpois(100, lambda = mean_degree)
    dist_b = degree_list
    return(ks_stat(min(degree_list), max(degree_list), dist_a, dist_b))
  })


  ##---Histogram and density function of ks_stats
  k_s_rep = unlist(k_s_rep)
  hist(k_s_rep, breaks=10, freq=FALSE, main = "Density Distribution of K-S Stats",
       xlab = "D-value", ylab = "Density")
  lines(density(k_s_rep))
  
  ##----Test Power law distribution----
  library(igraph)
  pow_fit <- power.law.fit(degree_list) # Fit power law
  names(pow_fit)
  pow_fit$KS.stat
  print(paste("K-S stat =", round(pow_fit$KS.stat,3), "using power fit at a p-value of",round(pow_fit$KS.p,3) ))
   
}