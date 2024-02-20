###############################
#      MIMI-EPHI training     # 
# MN intake and distribution  #
###############################

# call the useful libraries

# install.packages("pacman")

rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven","foreign")
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

# Look at folate as an example 
hices1516 %>% 
  ggplot(aes(x = folate_mcg)) + 
  geom_histogram() +
  xlim(0,2000)

summary(hices1516$folate_mcg)


# look at the distributions for different subpopulations

# residence
hices1516 %>% 
  ggplot(aes(x = folate_mcg, fill = res)) + 
  geom_histogram(position = 'dodge') +
  xlim(0,2000)


urban <- hices1516 %>% 
  filter(res == "Urban")

rural <- hices1516 %>% 
  filter(res == " Rural")

## look at the quintiles, is there a large distribution, which are higher and lower

folate_har <- 250

quantile(hices1516$folate_mcg)
quantile(urban$folate_mcg)
quantile(rural$folate_mcg)

#prevalence
sum(urban$folate_mcg<=folate_har)/nrow(urban)
sum(rural$folate_mcg<=folate_har)/nrow(rural)


