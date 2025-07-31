#---------------------------------------#
# Source script for FPL Probability Model
# Written by: ncfisher
# Last updated: July 30 2025
#---------------------------------------#

## Paste timestamp for model beginning
print(paste0('FPL Probability Model run beginning at: ', Sys.time()))

## Source the other scripts
error_occured <- FALSE

print('Data compilation beginning')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/Data compile_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in data compilation: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Data compilation complete')
})

print('Load model parameters')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/parameters.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in loading parameters: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Parameter loading complete')
})

print('Developing probabilities for play time')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/time_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing play time probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Play time probabilities complete')
})

print('Developing probabilities for goal scoring')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/goal_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing goal scoring probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Goal scoring probabilities complete')
})

print('Developing probabilities for assists')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/assist_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing assist probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Assist probabilities complete')
})

print('Developing probabilities for yellow/red cards')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/cards_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing yellow/red card probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Yellow/red card probabilities complete')
})

print('Developing probabilities for goals conceded')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/cs_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing goals conceded probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Goals conceded probabilities complete')
})

print('Developing probabilities for goal keeper stats')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/gk_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing goal keeper stats probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Goal keeper stats probabilities complete')
})

print('Developing probabilities for negative stats')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/negative_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing negative stats probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Negative stats probabilities complete')
})

print('Developing probabilities for bonus points')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/bonus_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing bonus points probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Bonus points probabilities complete')
})

print('Developing probabilities for defensive contributions')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/def_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in developing defensive contribution probabilities: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Defensive contribution probabilities complete')
})

print('Compiling final results')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/results_compile_prob.R', local = T) 
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in compiling results: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Results compilation complete')
})

print('Validating 2024 results')

tryCatch({
  suppressMessages(
    suppressWarnings(
      source('modules/validation_prob.R', local = T)
    )
  )
}, error = function(err){
  error_occured <<- TRUE
  cat('Error in compiling results: ', conditionMessage(err), '\n')
  cat('Traceback: \n')
  traceback()
}, finally = {
  print('Validation compilation complete')
})

print(paste0('Model run complete: ', Sys.time()))

rm(list=ls())
gc()
