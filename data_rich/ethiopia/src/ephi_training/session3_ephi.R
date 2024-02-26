###############################
#      MIMI-EPHI training     # 
# Modelling wheat flour reach #
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




# calculating reach of wheat flour ---------------------------------------------

# isolate wheat flour vehicle 



# explore the variable in different domains




# calculate the overall reach of the country



# calculate the reach at adm1/adm2 level, substrat by res



# discussion of reach of fortifiable wheat, do these estimates make sense?
