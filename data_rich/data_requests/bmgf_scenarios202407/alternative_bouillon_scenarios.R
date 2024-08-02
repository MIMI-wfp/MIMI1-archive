################################################################################
############ NIGERIA - ALTERNATIVE BOUILLON FORTIFICATION SCENARIOS ############
################################################################################

# Author: Mo Osman
# Collaborator: Kevin Tang
# Date created: 30-Jul-2024
# Last edited: 

# This script is to calculate alternative Bouillon fortification level scenarios
# in Nigeria.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("tidyverse", "readr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA: 
nigeria_fort <- read_csv("data_rich/data_requests/bmgf_scenarios202407/nga_lss1819_bmgfrequest.csv")

# De-select variables that will require re-computing: 
nigeria_fort <- nigeria_fort %>% 
  dplyr::select(-c(thia_allvehicles, ribo_allvehicles, thia_bouillon, ribo_bouillon))

#-------------------------------------------------------------------------------

# CALCULATE ALTERNATIVE BOUILLON FORTIFICATION SCENARIOS:

# Scenario 1 - fortification at 15% of CODEX NRV - 0.18mg/2.5g serving = 7.2mg/100g
scenario1_conc <- 0.18/2.5 # mg/g

nigeria_fort <- nigeria_fort %>% 
  mutate(thia_bouillon1 = case_when(zone == 1 ~ scenario1_conc * 4.8,
                                   zone == 2 ~ scenario1_conc * 8.5,
                                   zone == 3 ~ scenario1_conc * 9.1,
                                   zone == 4 ~ scenario1_conc * 3.9,
                                   zone == 5 ~ scenario1_conc * 4.8,
                                   zone == 6 ~ scenario1_conc * 2.7),
         ribo_bouillon1 = case_when(zone == 1 ~ scenario1_conc * 4.8,
                                   zone == 2 ~ scenario1_conc * 8.5,
                                   zone == 3 ~ scenario1_conc * 9.1,
                                   zone == 4 ~ scenario1_conc * 3.9,
                                   zone == 5 ~ scenario1_conc * 4.8,
                                   zone == 6 ~ scenario1_conc * 2.7))

# Scenario 2 - fortification at 15% of CODEX NRV per 6.3g serving - 0.18mg/6.3g: 
scenario2_conc <- 0.18/6.3 # mg/g

nigeria_fort <- nigeria_fort %>% 
  mutate(thia_bouillon2 = case_when(zone == 1 ~ scenario2_conc * 4.8,
                                   zone == 2 ~ scenario2_conc * 8.5,
                                   zone == 3 ~ scenario2_conc * 9.1,
                                   zone == 4 ~ scenario2_conc * 3.9,
                                   zone == 5 ~ scenario2_conc * 4.8,
                                   zone == 6 ~ scenario2_conc * 2.7),
         ribo_bouillon2 = case_when(zone == 1 ~ scenario2_conc * 4.8,
                                   zone == 2 ~ scenario2_conc * 8.5,
                                   zone == 3 ~ scenario2_conc * 9.1,
                                   zone == 4 ~ scenario2_conc * 3.9,
                                   zone == 5 ~ scenario2_conc * 4.8,
                                   zone == 6 ~ scenario2_conc * 2.7))

#-------------------------------------------------------------------------------

# Calculate additional fortificaiton contents from fortifying all vehicles, 
# both scenario 1 and scenario 2: 
nigeria_fort <- nigeria_fort %>% 
  mutate(thia_allvehicles1 = thia_wheatflour + thia_rice + thia_bouillon1,
         ribo_allvehicles1 = ribo_wheatflour + ribo_rice + ribo_bouillon1,
         thia_allvehicles2 = thia_wheatflour + thia_rice + thia_bouillon2,
         ribo_allvehicles2 = ribo_wheatflour + ribo_rice + ribo_bouillon2)

#-------------------------------------------------------------------------------

# WRITE DATA: 
# write_csv(nigeria_fort, "data_rich/data_requests/bmgf_scenarios202407/nga_lss1819_bmgfrequest2.csv")

rm(list=ls()[!ls() %in% "nigeria_fort"])

################################################################################
############################## END OF SCRIPT ###################################
################################################################################

