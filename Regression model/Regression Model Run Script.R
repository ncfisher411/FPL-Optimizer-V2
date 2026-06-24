#---------------------------------------#
# Run script for FPL Lineup Optimizer
# Written by: ncfisher
# Last updated: June 19 2026
#---------------------------------------#
# This script is used to run the FPL optimizer model and Probability distribution
# model. 

# The FPL prediction model runs assesses a series of model options and chooses
# the best model to run from linear or random forest models. This set of models
# predict statistics by match and then calculate points
# 
# Current FPL predictive model run time: ~ 45 minutes
# 
# To use the models, press control + enter on each command below. This may require
# installation of several R packages.

packages <- c('tidyverse', 'worldfootballR', 'randomForest', 'rstudioapi',
              'httr', 'jsonlite', 'openxlsx', 'data.table',
              'stringdist', 'stats', 'fuzzyjoin', 'stringi',
              'broom.mixed')

for (package in packages) {
  if (!requireNamespace(package, quietly = T)) {
    install.packages(package, dependencies = T)
  }
  library(package, character.only = T)
  cat(paste(package, "package loaded.\n"))
}

setwd(dirname(getActiveDocumentContext()$path))

#---------------------------------------------------------------------------------#
#### DEV VERSION? Only change to "Yes" if attempting to re-assess/estimate the models
dev = 'No'
#---------------------------------------------------------------------------------#

#### This line will run the FPL model
source('modules/Compile FPL Results.R')
