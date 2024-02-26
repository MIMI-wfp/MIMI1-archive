###############################
#      MIMI-EPHI training     # 
# Risk of inadequate intake   #
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
# create the folate HAR variable

folate_har <- 250 #allen et al 

# create a new variable of whether above or below H-AR 





# aggregate at different levels


# adm1


# adm2



# sep


# educ head

# sex head


# residence 



# etc



# save the base case for use later