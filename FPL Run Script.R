#---------------------------------------#
# Run script for FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: August 31 2024
#---------------------------------------#
# This script is used to run the FPL optimizer models. This script will begin a
# set of models that will predict statistics by match and then calculate 
# predicted points
# 
# Current model run time: ~ 60 minutes
# If the model appears stuck, restart your R session
# 
# To use the model, press control + enter on each command below. This may require
# installation of several R packages.
#
# WARNING: THERE IS OCCASIONAL TIMEOUT OF WEB-BASED FUNCTIONS IN THE DATA.
# COMPILATION. IF THE MODEL DOES NOT COMPLETE DUE TO A TIMEOUT, RUN THE
# MODEL AGAIN. 

packages <- c('tidyverse', 'worldfootballR', 'randomForest', 'rstudioapi',
              'httr', 'jsonlite', 'fuzzyjoin', 'openxlsx', 'data.table',
              'stringdist', 'stats')

for (package in packages) {
  if (!requireNamespace(package, quietly = T)) {
    install.packages(package, dependencies = T)
  }
  library(package, character.only = T)
  cat(paste(package, "package loaded.\n"))
}

setwd(dirname(getActiveDocumentContext()$path))

source('modules/Compile FPL Results.R')
