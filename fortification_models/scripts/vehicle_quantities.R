################################################################################
############## FORTIFICATION VEHICLE REACH AND QUANTITIES CONSUMED #############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 18-Dec-2023
# Last edited: 22-Dec-2023

# This script is for extracting binarised (Yes/No) consumption of fortification 
# vehicles for each household, and the quantities consumed (in kg or L). 

# Install and load required packages:
rq_packages <- c("readr", "tidyverse", "haven")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Source script required to create base models: 
source("all_base_models/scripts/base_model_functions.R")

#-------------------------------------------------------------------------------

#########################
#### PART 1: NIGERIA ####
#########################

# Derive base case apparent MN intake for each household
base_ai <- apparent_intake("nga1819")

# Note that the base_ai is based on MN values from UNFORTIFIED foods.

# Write csv to data folder:
# write_csv(base_ai, "fortification_models/data/nga1819_base_ai.csv")

# Remove objects that are no longer required:
rm(list = c("fct", "food_consumption", "afe", "full_item_list"))

# Read in food consumption module of the NLSS:
food_consumption <- read_csv("MIMI_data/nga/sect6b_food_cons.csv")

#-------------------------------------------------------------------------------

# Add a column with name of food item based on "item_cd" for all fortification vehicles:
food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_cd == 13 ~ "Rice", # Locally produced
  item_cd == 14 ~ "Rice", # Imported
  item_cd == 16 ~ "Maize flour",
  item_cd == 19 ~ "Wheat flour",
  item_cd == 50 ~ "Edible oil", # Palm oil
  item_cd == 52 ~ "Edible oil", # Groundnut oil
  item_cd == 53 ~ "Edible oil", # Other oil and fat
  item_cd == 130 ~ "Sugar", 
  item_cd == 141 ~ "Salt"
)) # ?To include other food items that include the above as ingredients?????????
# Need to consult recipe data for this.

# Add variable to indicate if the food item has been consumed (in last 7-days): 
food_consumption$consumed <- ifelse(food_consumption$s06bq01 == 1, "Yes", 
                                    ifelse(food_consumption$s06bq01 == 2, "No", NA))

# Quantify how much of each of these food items was consumed:
food_consumption <- food_consumption %>% 
  rename(quantity_consumed = s06bq02a, 
         quantity_unit = s06bq02b,
         unit_other = s06bq02b_os,
         quantity_size = s06bq02c,
         conversion_factor = s06bq02_cvn,
         quantity_purchased = s06bq03)

# Calculate the quantity consumed in standard units (kg/L)
food_consumption$quantity_kg_L <- food_consumption$quantity_consumed * food_consumption$conversion_factor

# Create a column if at least some of the food item was purchased (i.e. quantity > 0, and not NA):
food_consumption$food_purchased <- ifelse(food_consumption$quantity_purchased > 0, 
                                          "Yes", "No")

# If NA in food_purchased column, then replace with "No":
food_consumption$food_purchased <- ifelse(is.na(food_consumption$food_purchased), 
                                          "No", food_consumption$food_purchased)

food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_kg_L")

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities: 
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_kg_L = sum(quantity_kg_L))

# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles: 

vehicle_quantities <- base_ai %>% select("hhid") %>% 
  # RICE
  left_join((food_consumption %>%
               dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                             "quantity_kg_L") %>%
               filter(food_item == "Rice",
                      consumed == "Yes",
                      food_purchased == "Yes") %>%
               rename("rice_kg" = "quantity_kg_L") %>%
               ungroup() %>% 
               mutate(rice = "Yes") %>% 
               dplyr::select("hhid", "rice", "rice_kg")), by = "hhid") %>% 
  # WHEAT FLOUR:
  left_join((food_consumption %>%
               dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                             "quantity_kg_L") %>%
               filter(food_item == "Wheat flour",
                      consumed == "Yes",
                      food_purchased == "Yes") %>%
               rename("wheatflour_kg" = "quantity_kg_L") %>%
               ungroup() %>% 
               mutate(wheatflour = "Yes") %>% 
               dplyr::select("hhid", "wheatflour", "wheatflour_kg")), by = "hhid") %>% 
  # MAIZE FLOUR:
  left_join((food_consumption %>%
               dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                             "quantity_kg_L") %>%
               filter(food_item == "Maize flour",
                      consumed == "Yes",
                      food_purchased == "Yes") %>%
               rename("maizeflour_kg" = "quantity_kg_L") %>%
               ungroup() %>% 
               mutate(maizeflour = "Yes") %>% 
               dplyr::select("hhid", "maizeflour", "maizeflour_kg")), by = "hhid") %>%
  # SUGAR:
  left_join((food_consumption %>%
               dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                             "quantity_kg_L") %>%
               filter(food_item == "Sugar",
                      consumed == "Yes",
                      food_purchased == "Yes") %>%
               rename("sugar_kg" = "quantity_kg_L") %>%
               ungroup() %>% 
               mutate(sugar = "Yes") %>% 
               dplyr::select("hhid", "sugar", "sugar_kg")), by = "hhid") %>%
  # EDIBLE OIL:
  left_join((food_consumption %>%
               dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                             "quantity_kg_L") %>%
               filter(food_item == "Edible oil",
                      consumed == "Yes",
                      food_purchased == "Yes") %>%
               rename("edible_oil_kg" = "quantity_kg_L") %>%
               ungroup() %>% 
               mutate(edible_oil = "Yes") %>% 
               dplyr::select("hhid", "edible_oil", "edible_oil_kg")), by = "hhid") %>%
  # SALT:
  left_join((food_consumption %>%
               dplyr::select("hhid", "food_item", "consumed", "food_purchased",
                             "quantity_kg_L") %>%
               filter(food_item == "Salt",
                      consumed == "Yes",
                      food_purchased == "Yes") %>%
               rename("salt_kg" = "quantity_kg_L") %>%
               ungroup() %>% 
               mutate(salt = "Yes") %>% 
               dplyr::select("hhid", "salt", "salt_kg")), by = "hhid") %>%
  # If food item is not consumed, then replace NA with "No":
  mutate_at(vars(rice, wheatflour, maizeflour, sugar, edible_oil, salt), 
            funs(replace(., is.na(.), "No"))) %>% 
  # Change food item variables to factor: 
  mutate_at(vars(rice, wheatflour, maizeflour, sugar, edible_oil, salt), 
            funs(factor(., levels = c("Yes", "No"))))

# Save this data-frame as a .csv file:
# write_csv(vehicle_quantities, 
#           "fortification_models/data/nga1819_vehicle_quantities.csv")

# Remove objects no longer required: 
rm(list = c("food_consumption", "vehicle_quantities", "base_ai"))

#-------------------------------------------------------------------------------

#########################
#### PART 2: MALAWI #####
#########################

# Get base case apparent intake data for Malawi: 
base_ai <- apparent_intake("mwi1516")

# Check the year of this survey.

# Read in food consumption data for Malawi:
food_consumption <- read_csv("all_base_models/data/mwi1516_food_consumption.csv")

# Add a column called food_item which includes name of food item for all 
# fortification vehicles:
food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == 101 ~ "Maize flour", # normal flour
  item_code == 102 ~ "Maize flour", # fine flour
  item_code == 103 ~ "Maize flour", # bran flour
  item_code == 106 ~ "Rice",
  item_code == 110 ~ "Wheat flour",
  # item_code == 111 ~ "Bread",
  # item_code == 112 ~ "Buns, scones",
  # item_code == 113 ~ "Biscuits",
  # item_code == 114 ~ "Spaghetti, macaroni, pasta",
  # item_code == 115 ~ "Breakfast cereal",
  item_code == 801 ~ "Sugar",
  item_code == 803 ~ "Edible oil",
  item_code == 810 ~ "Salt"
  # Other food items to be included once recipe data available (e.g. bread)
))

# Need to add an additional column to indicate if the food item was purchased.

