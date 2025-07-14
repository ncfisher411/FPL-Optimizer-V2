#---------------------------------------#
# Source script for FPL Probability Model
# Written by: ncfisher
# Last updated: July 10 2025
#---------------------------------------#

## Paste timestamp for model beginning
print(paste0('FPL Probability Model run beginning at: ', Sys.time()))

## Source the other scripts
error_occured <- FALSE

print(paste0('Data compilation beginning at: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/Data compile_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in data compilation: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Data compilation complete: ', Sys.time()))
})

print(paste0('Load model parameters: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/parameters.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in loading parameters: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Parameter loading complete: ', Sys.time()))
})

print(paste0('Developing probabilities for play time: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/time_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing play time probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Play time probabilities complete: ', Sys.time()))
})

print(paste0('Developing probabilities for goal scoring: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/goal_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing goal scoring probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Goal scoring probabilities complete: ', Sys.time()))
})

print(paste0('Developing probabilities for assists: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/assist_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing assist probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Assist probabilities complete: ', Sys.time()))
})

print(paste0('Developing probabilities for yellow/red cards: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/cards_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing yellow/red card probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Yellow/red card probabilities complete: ', Sys.time()))
})

print(paste0('Developing probabilities for goals conceded: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/cs_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing goals conceded probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Goals conceded probabilities complete: ', Sys.time()))
})

print(paste0('Developing probabilities for goal keeper stats: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/gk_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing goal keeper stats probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Goal keeper stats probabilities complete: ', Sys.time()))
})

print(paste0('Developing probabilities for negative stats: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/negative_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing negative stats probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Negative stats probabilities complete: ', Sys.time()))
})

print(paste0('Developing probabilities for bonus points: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/bonus_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing bonus points probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Bonus points probabilities complete: ', Sys.time()))
})

print(paste0('Compiling final results: ', Sys.time()))

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('Probability model/modules/results_compile_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in compiling results: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print(paste0('Results compilation complete: ', Sys.time()))
})