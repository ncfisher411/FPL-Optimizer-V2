#---------------------------------------#
# Run script for FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: June 23 2026
#---------------------------------------#
# This script is used to run the FPL optimizer model and Probability distribution
# model. 
# 
# The Probability model uses weighted probabilities to calculate the likelihood
# of FPL scoring events occurring within a match.
#
# Probability model run time: ~ 20 minutes
# 
# To use the models, press control + enter on each command below. This may require
# installation of several R packages.

packages <- c('tidyverse', 'worldfootballR', 'randomForest', 'rstudioapi',
              'httr', 'jsonlite', 'openxlsx', 'data.table', 'rvest',
              'stringdist', 'stats', 'fuzzyjoin', 'stringi')

for (package in packages) {
  if (!requireNamespace(package, quietly = T)) {
    install.packages(package, dependencies = T)
  }
  library(package, character.only = T)
  cat(paste(package, "package loaded.\n"))
}

setwd(dirname(getActiveDocumentContext()$path))

#---------------------------------------------------------------------------------#
#### This line will run the probabilities model
source('modules/prob_model_run.R')
