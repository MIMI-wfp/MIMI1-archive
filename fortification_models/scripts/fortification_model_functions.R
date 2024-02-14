################################################################################
####################### FORTIFICATION MODEL FUNCTIONS ##########################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 28-Dec-2023
# Last edited: 05-Feb-2024

# This script contains functions required for creating the fortification models.

# INSTALL AND LOAD PACKAGES: 

rq_packages <- c("tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Function to get access and quantities of each fortrification vehicle: 

get_vehicle_quantities <- function(base_ai, food_consumption, hh_info) {
  vehicle_quantities <<- base_ai %>% select("hhid") %>% 
    # RICE
    left_join((food_consumption %>%
                 dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                               "purchased_100g") %>%
                 filter(food_item == "Rice",
                        consumed == "Yes",
                        food_purchased == "Yes") %>%
                 rename("rice_100g" = "purchased_100g") %>%
                 ungroup() %>% 
                 mutate(rice = "Yes") %>% 
                 dplyr::select("hhid", "rice", "rice_100g")), by = "hhid") %>% 
    # WHEAT FLOUR:
    left_join((food_consumption %>%
                 dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                               "purchased_100g") %>%
                 filter(food_item == "Wheat flour",
                        consumed == "Yes",
                        food_purchased == "Yes") %>%
                 rename("wheatflour_100g" = "purchased_100g") %>%
                 ungroup() %>% 
                 mutate(wheatflour = "Yes") %>% 
                 dplyr::select("hhid", "wheatflour", "wheatflour_100g")), by = "hhid") %>% 
    # MAIZE FLOUR:
    left_join((food_consumption %>%
                 dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                               "purchased_100g") %>%
                 filter(food_item == "Maize flour",
                        consumed == "Yes",
                        food_purchased == "Yes") %>%
                 rename("maizeflour_100g" = "purchased_100g") %>%
                 ungroup() %>% 
                 mutate(maizeflour = "Yes") %>% 
                 dplyr::select("hhid", "maizeflour", "maizeflour_100g")), by = "hhid") %>%
    # SUGAR:
    left_join((food_consumption %>%
                 dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                               "purchased_100g") %>%
                 filter(food_item == "Sugar",
                        consumed == "Yes",
                        food_purchased == "Yes") %>%
                 rename("sugar_100g" = "purchased_100g") %>%
                 ungroup() %>% 
                 mutate(sugar = "Yes") %>% 
                 dplyr::select("hhid", "sugar", "sugar_100g")), by = "hhid") %>%
    # EDIBLE OIL:
    left_join((food_consumption %>%
                 dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                               "purchased_100g") %>%
                 filter(food_item == "Edible oil",
                        consumed == "Yes",
                        food_purchased == "Yes") %>%
                 rename("edible_oil_100g" = "purchased_100g") %>%
                 ungroup() %>% 
                 mutate(edible_oil = "Yes") %>% 
                 dplyr::select("hhid", "edible_oil", "edible_oil_100g")), by = "hhid") %>%
    # SALT:
    left_join((food_consumption %>%
                 dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                               "purchased_100g") %>%
                 filter(food_item == "Salt",
                        consumed == "Yes",
                        food_purchased == "Yes") %>%
                 rename("salt_100g" = "purchased_100g") %>%
                 ungroup() %>% 
                 mutate(salt = "Yes") %>% 
                 dplyr::select("hhid", "salt", "salt_100g")), by = "hhid") %>%
    # If food item is not consumed, then replace NA with "No":
    mutate(across(c(rice, wheatflour, maizeflour, sugar, edible_oil, salt), 
                  ~ ifelse(is.na(.), "No", .)))%>% 
    # Change food item variables to factor: 
    mutate(across(c(rice, wheatflour, maizeflour, sugar, edible_oil, salt), 
                  ~ factor(., levels = c("Yes", "No")))) %>% 
    # Create a new variable for overall staple grain consumption:
    mutate(staple_grain = ifelse(rice == "Yes" | wheatflour == "Yes" |
                                   maizeflour == "Yes", "Yes", "No")) %>%
    # Calculate quantity consumed:
    mutate(staplegrain_100g = rowSums(.[,c("rice_100g", "wheatflour_100g", "maizeflour_100g")], 
                                      na.rm = TRUE)) %>%
    # Convert quantities to per AFE:
    left_join((hh_info %>% 
                 dplyr::select("hhid", "afe")), by = "hhid") %>%
    mutate(rice_100g = rice_100g / afe,
           wheatflour_100g = wheatflour_100g / afe,
           maizeflour_100g = maizeflour_100g / afe,
           sugar_100g = sugar_100g / afe,
           edible_oil_100g = edible_oil_100g / afe,
           salt_100g = salt_100g / afe,
           staplegrain_100g = staplegrain_100g / afe) %>% 
    # If staplegrain_100g has a 0 value, replace with NA: 
    mutate(staplegrain_100g = ifelse(staplegrain_100g == 0, NA, staplegrain_100g)) %>%
    # Remove afe column:
    select(-afe)
  
}

#-------------------------------------------------------------------------------