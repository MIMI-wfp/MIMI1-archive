###############################
#      MIMI-EPHI training     # 
#   Contributions from wheat  #
#     flour fortification     #
###############################

# call the useful libraries

# install.packages("pacman")

rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven","foreign", "survey")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

###############################################################################
# read in data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets directory to where this is saved

hices1516 <- read_dta("data/hices1516.dta")


# ------------------------------------------------------------------------------


# Caclulating the contributions from wheat flour fortification

# Define the standards for micronutrient(s), g/kg 



# create variable of increased micronutrient supply from 

