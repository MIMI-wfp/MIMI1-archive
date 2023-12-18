################################################################################
#### SCRIPT FOR EXTRACTING AND CLEANING VARIABLES FOR FORTIFICATION MODELS #####
################################################################################

# Author: Mo Osman
# Date created: 18-Dec-2023
# Last edited: 

# Install and load required packages:
rq_packages <- c("readr", "tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Source script required to create base models: 
# source("all_base_models/scripts/base_model_functions.R")
# Re-add above line of code once base model functions have been fixed.

#-------------------------------------------------------------------------------

# NIGERIA

# Read-in base model data:
nga_food_consumption <- read_csv("all_base_models/data/nga1819_food_consumption.csv")
nga_fct <- read_csv("all_base_models/data/nga1819_fct.csv")
nga_afe <- read_csv("all_base_models/data/nga1819_afe.csv")
