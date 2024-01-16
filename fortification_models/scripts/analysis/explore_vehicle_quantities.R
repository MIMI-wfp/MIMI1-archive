################################################################################
################ EXPLORATORY ANALYSIS FOR FORTIFICATION VEHICLES ###############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 09-Jan-2024
# Last edited: 11-Jan-2024

# This script is for performing exploratory analysis of reach of fortification 
# vehicles, and quantities consumed. 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "ggridges")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN FUNCTIONS:
source("fortification_models/scripts/analysis/eda_functions.R")

#-------------------------------------------------------------------------------

# READ IN DATA: 

# Nigeria LSS:
nga_lss <- read_csv("fortification_models/data/nga_lss1819_vehicle_quantities.csv")

# India NSS:
ind_nss <- read_csv("fortification_models/data/ind_nss1112_vehicle_quantities.csv")

#-------------------------------------------------------------------------------

# Read in survey weights from hh_info data: 

# Nigeria LSS:
nga_lss <- nga_lss %>% 
  left_join(read_csv("all_base_models/data/current-20240108/nga_lss1819_hh_info.csv") %>% 
              dplyr::select("hhid", "survey_wgt"), by = "hhid")

# India NSS:
ind_nss <- ind_nss %>% 
  left_join(read_csv("all_base_models/data/current-20240108/ind_nss1112_hh_info.csv") %>% 
              dplyr::select("hhid", "survey_wgt"), by = "hhid")

#-------------------------------------------------------------------------------

# CREATE tbl_svy OBJECTS FOR EACH OF THE DATAFRAMES: 

# Nigeria LSS:
svy_nga_lss <- nga_lss %>%
  srvyr::as_survey_design(weights = survey_wgt)

# India NSS:
svy_ind_nss <- ind_nss %>%
  srvyr::as_survey_design(weights = survey_wgt)

#-------------------------------------------------------------------------------

# REACH:

plot_reach(svy_nga_lss) + labs(title = "Nigeria (LSS)")

plot_reach(svy_ind_nss) + labs(title = "India (NSS)")

#-------------------------------------------------------------------------------

# QUANTITY OF CONSUMPTION: 

plot_quantites(nga_lss) + labs(title = "Nigeria (LSS)") + xlim(0, 600)

plot_quantites(ind_nss) + labs(title = "India (NSS)") + xlim(0, 400)

#-------------------------------------------------------------------------------

# Produce table with summary statistics for each vehicle:

#-------------------------------------------------------------------------------

# Code for Gabriel to calculate reach by ADM1:

# Read in hh_info for India: 
ind_hh_info <- read_csv("all_base_models/data/current-20240108/ind_nss1112_hh_info.csv") 

ind_nss <- ind_nss %>% 
  left_join(ind_hh_info %>% 
              dplyr::select("hhid", "adm1"), by = "hhid")

# Create a table with reach of each vehicle by adm1: 
ind_reach <- ind_nss %>% 
  dplyr::select("adm1", "rice", "wheatflour", "maizeflour", "edible_oil", "sugar", 
               "salt") %>%
  group_by(adm1) %>%
  # Summrarise percentage of households with "Yes" for each food item: 
  summarise_all(funs(sum(. == "Yes")/n())) %>%
  gather(key = "vehicle", value = "reach", -adm1) %>%
  mutate(vehicle = recode(vehicle, 
                          "rice" = "Rice", 
                          "wheatflour" = "Wheat flour", 
                          "maizeflour" = "Maize flour", 
                          "edible_oil" = "Edible oil", 
                          "sugar" = "Sugar", 
                          "salt" = "Salt")) %>%
  arrange(vehicle)

# Plot reach of each vehicle (facet wrapped by ADM1):
ggplot(ind_reach, aes(x = vehicle, y = reach)) +
  geom_bar(stat = "identity") +
  facet_wrap(~adm1, ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Fortification vehicle", y = "Reach (%)") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5))

