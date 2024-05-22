################################################################################
########################### FRAYM DATA REQUEST #################################
################################################################################

# A script to put together a csv file as requested by fraym, that includes binary 
# reach of wheat flour, and binary adequacy of 4 selected micronutrients:
# Vitamin A, Folate, Zinc, and Vitamin B12.

# INSTALL AND LOAD PACKAGES:
rq_packages <- c("readr", "tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Read in relevant data:
vehicle_quantities <- read_csv("data_rich/fortification_models/data/eth_ess1819_vehicle_quantities.csv")
nut_adequacy <- read_csv("data_rich/data_requests/fraym20240202/ess_binary.csv")

#-------------------------------------------------------------------------------

# Select relevant variables from each data-frame: 
vehicle_quantities <- vehicle_quantities %>% dplyr::select("hhid", "wheatflour")

# Transform "wheatflour" to binary:
vehicle_quantities$wheatflour <- ifelse(vehicle_quantities$wheatflour == "Yes", 1, 0)

# Nutrient adequacy: 
nut_adequacy <- nut_adequacy %>% dplyr::select("hhid", "vita", "folate", "vitb12", 
                                               "zn")

#-------------------------------------------------------------------------------

# Merge the two data-frames:
ess1819_fraym <- nut_adequacy %>% left_join(vehicle_quantities, by = "hhid")

# Write csv file:
# write_csv(ess1819_fraym, "data_rich/data_requests/fraym20240202/ess1819_fraym.csv")

#-------------------------------------------------------------------------------

# 2nd data request - Reach of edible oil for each hhid:
vehicle_quantities <- read_csv("data_rich/fortification_models/data/eth_ess1819_vehicle_quantities.csv")

# Select relevant variables from vehicle_quantities: 
vehicle_quantities <- vehicle_quantities %>% dplyr::select("hhid", "edible_oil")

# Transform "edible_oil" to binary:
vehicle_quantities$edible_oil <- ifelse(vehicle_quantities$edible_oil == "Yes", 1, 0)

# Write csv: 
# write_csv(vehicle_quantities, "data_rich/data_requests/fraym20240202/ess1819_edible_oil.csv")


rm(list = ls())

################################################################################
############################# END OF SCRIPT ####################################
################################################################################

