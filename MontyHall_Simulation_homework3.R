## --------------------------------------------
##
## Class: DATASCI 350 Methods for Data Analysis
## 
## File: MontyHall_Simulation_homework3.R
##
## Purpose: Run Monty Hall simulation to print mean and SD for Stay and Switch game mode
##
## Created by: Saunak Chandra (saunakc@uw.edu)
##
## Created on: 2015-07-11
##
## --------------------------------------------


##-----Load Libraries-----
library(microbenchmark)
library(logging)

## Declare the simulation function

## In this Monty Hall simulation we play the game "sample_size" times each in Stay and Switch mode
## and return the success_ratio for each mode as a vector- (stay_success_ratio, switch_success_ratio)
monty_hall_simulation = function (sample_size = 100, logger = NA){
  if (is.function ( logger)){
    loginfo(paste0("Simulation run using sample_size = ", sample_size),
            logger = logger)
  }
    
  ## Take a sample for Stay game mode: remember the contestant stays with the door he picks.
  stay_sample = matrix(sapply(1:sample_size, function(x){ 
    sample(c('Car', 'Goat', 'Goat'), 1)
  }
  ), nrow = sample_size)
  
  ## Success if he picks the Car in the draw
  stay_success = length(stay_sample[stay_sample[,1] == 'Car'])
  stay_success_ratio = stay_success/ sample_size
  
  if (is.function ( logger)){
    loginfo(paste0("Success_ratio for stay:", stay_success_ratio),
            logger = logger)
  }
  
  
  ## In switch mode take a fresh sample once again. We could consider the sample already taken too.
  switch_sample = matrix(sapply(1:sample_size, function(x){ 
    sample(c('Car', 'Goat', 'Goat'), 1)
  }
  ), nrow = sample_size)
  
  ## it's a success only when contestant's turn was a Goat. Note host's turn has to be Goat for a success.
  switch_success = length(switch_sample[switch_sample[,1] == 'Goat'])
  switch_success_ratio = switch_success/ sample_size
  
  if (is.function ( logger)){
    loginfo(paste0("Success_ratio for switch:", switch_success_ratio),
            logger = logger)
  }
  
  return(c(stay_success_ratio, switch_success_ratio))
}

## Below Monty hall function is a trivial run without any simulation. This should be used with 
## care- it can be run only in For loop and your simulation may run for several minutes.
play_monty_hall = function( game_mode='Stay', logger=NA){
  ## Initialize parameters
  #logger = 'monty_hall.log'
  #game_mode='Switch'
  gates <- c('Goat', 'Goat', 'Car')
  
  ## contenstant's turn
  player_turn = sample(gates, 1)
  remaining_gates <- gates[- match(player_turn, gates)]
  
  ## Return win/ loss in 'Stay' mode
  if (game_mode == 'Stay') {
    game_result = ifelse (player_turn == "Car", 'Win', 'Loss')
    #print("The game result is ", game_result)
    if (! is.na(logger)){
      loginfo(paste0("The game result in ", game_mode, " is ", game_result), 
              logger = logger)
    }
    return (game_result)
  }
      
  ## For Switch game_mode continue the game  
  if (game_mode == 'Switch') { 
    host_turn_gate_position <- match('Goat', remaining_gates)
    game_result = ifelse (remaining_gates[-host_turn_gate_position] == 'Car', 'Win', 'Loss')
    if (! is.na(logger)){
      loginfo(paste0("The game result in ", game_mode, " is ", game_result), 
              logger = logger)
    }
  return(game_result)
  }
}

if(interactive()){
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/MH_testing.log", level='DEBUG')  
  
  ## Inititalize parameters for simulation
  n = 1000
  
  ## Record time to take the simulation using system.time 
  system.time(sapply(1:n, function (x) {
    return (monty_hall_simulation())
  }))
    
  ## Recod the time using microbenchmark
  microbenchmark(sapply(1:1, function (x) {
    return (monty_hall_simulation())
  }))
  
  ## Run simulation eith 100 sample size
  monty_hall_simulation_result = sapply(1:n, function (x) {
    return (monty_hall_simulation(sample_size = 1000))
  })
  
  stay_mean = mean(monty_hall_simulation_result[1, ])
  stay_variance = sd(monty_hall_simulation_result[1, ]) ^ 2
  
  switch_mean = mean(monty_hall_simulation_result[2, ])
  switch_variance = sd(monty_hall_simulation_result[2, ]) ^ 2

  print(paste0("Mean for Stay: ", stay_mean, ";  Mean for Switch: ", switch_mean))
  print(paste0("SD for Stay: ", stay_variance, ";  SD for Switch: ", switch_variance))

}


