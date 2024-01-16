################################################################################
################# FUNCTIONS FOR EXPLORATORY DATA ANALYSIS ######################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 16-Jan-2024
# Last edited: 

# This script is for creating functions required in the exploratory data analysis.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr", "ggridges", "ghibli")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Create a function that calculates the reach of each fortification vehicle, and
# plots this in a ggplot bar chart:

plot_reach <- function(svy_data) {
  
  rice_reach <- svy_data %>% 
    group_by(rice) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(rice, percentage)
  
  wheatflour_reach <- svy_data %>% 
    group_by(wheatflour) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(wheatflour, percentage)
  
  maizeflour_reach <- svy_data %>%
    group_by(maizeflour) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(maizeflour, percentage)
  
  edibleoil_reach <- svy_data %>%
    group_by(edible_oil) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(edible_oil, percentage)
  
  sugar_reach <- svy_data %>%
    group_by(sugar) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(sugar, percentage)
  
  salt_reach <- svy_data %>%
    group_by(salt) %>% 
    summarise(n = survey_total()) %>%
    mutate(percentage = round(n/sum(n)*100, digits = 1)) %>% 
    dplyr::select(salt, percentage)
  
  # Create a data-frame for reach of each vehicle:
  vehicle <- c("Rice", "Wheat flour", "Maize flour", "Edible oil", "Sugar", "Salt")
  
  reach <- c(rice_reach[2,2], wheatflour_reach[2,2], maizeflour_reach[2,2], 
             edibleoil_reach[2,2], sugar_reach[2,2], salt_reach[2,2])
  
  reach <- as.double(reach)
  
  vehicle_reach <- data.frame(vehicle, reach)
  
  # Convert "vehicle" to an odered factor with custom levels: 
  vehicle_reach$vehicle <- factor(vehicle_reach$vehicle, 
                                  levels = c("Rice", "Wheat flour", "Maize flour", 
                                             "Edible oil", "Sugar", "Salt"))
  
  
  # Now plot the reach of each vehicle:
  ggplot(vehicle_reach, aes(x = vehicle, 
                            y = reach, fill = vehicle)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(reach, "%")), vjust = -0.5, size = 3) +
    labs(x = "", y = "Reach of fortification vehicle (%)") +
    scale_colour_ghibli_d("MononokeMedium", direction = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#-------------------------------------------------------------------------------

# Create a function that extracts quantity consumed of each vehicle, and presents
# this data in a density plot using ggridges:

plot_quantites <- function(vehicle_quantities) {
  long_data <- vehicle_quantities %>% select(hhid, rice_100g, wheatflour_100g,
                                             maizeflour_100g, edible_oil_100g,
                                             sugar_100g, salt_100g) %>%
    gather(key = "vehicle", value = "quantity", -hhid)
  
  # Mutate df so that quantity is in grams:
  long_data <- long_data %>% 
    mutate(quantity_g = quantity*100) %>% 
    dplyr::select(-quantity)
  
  long_data$vehicle <- recode(long_data$vehicle, 
                              "rice_100g" = "Rice", 
                              "wheatflour_100g" = "Wheat flour", 
                              "maizeflour_100g" = "Maize flour", 
                              "edible_oil_100g" = "Edible oil", 
                              "sugar_100g" = "Sugar", 
                              "salt_100g" = "Salt")
  
  
  ggplot(long_data, aes(x = quantity_g, y = vehicle)) +
    geom_density_ridges(na.rm = TRUE) + 
    theme_ridges() + 
    theme(legend.position = "none") + 
    labs(x = "Quantity consumed per day, per AFE (grams)", 
         y = "Fortification vehicle") +
    theme(axis.title.x = element_text(hjust = 0.5),
          axis.title.y = element_text(hjust = 0.5)) 
  
}

################################################################################
################################## END OF SCRIPT ###############################
################################################################################
