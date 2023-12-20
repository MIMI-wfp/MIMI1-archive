################################################################################
#### SCRIPT FOR EXTRACTING AND CLEANING VARIABLES FOR FORTIFICATION MODELS #####
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 18-Dec-2023
# Last edited: 19-Dec-2023

# Install and load required packages:
rq_packages <- c("readr", "tidyverse")

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

# Read in data:
read_in_survey("nga1819")

# Derive base case apparent MN intake for each household
base_ai <- apparent_intake("nga1819")

# Note that the base_ai is based on MN values from UNFORTIFIED foods.

# Write csv to data folder: 
write_csv(base_ai, "fortification_models/data/nga1819_base_ai.csv")

# Remove objects that are no longer required: 
rm(list = c("fct", "food_consumption", "apparent_intake", 
            "full_item_list", "read_in_survey"))

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

# Calculate additional apparent intake achieved from fortifying each of the 
# following foods (per adult female equivalent): 

# RICE: 

# First get quantity of fortifiable rice consumed by each household:
rice_ai <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_kg_L") %>% 
  filter(food_item == "Rice",
         consumed == "Yes",
         food_purchased == "Yes") %>% 
  rename("rice_kg" = "quantity_kg_L") %>% 
  dplyr::select("hhid", "rice_kg") %>% 
  # Left join AFE variable: 
  left_join(afe, by = "hhid")



# WHEAT FLOUR: 

wheatflour_ai <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_kg_L") %>% 
  filter(food_item == "Wheat flour",
         consumed == "Yes",
         food_purchased == "Yes") %>% 
  rename("wheatflour_kg" = "quantity_kg_L") %>% 
  dplyr::select("hhid", "wheatflour_kg") %>% 
  # Left join AFE variable: 
  left_join(afe, by = "hhid")

# MAIZE FLOUR: 

maizeflour_ai <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_kg_L") %>% 
  filter(food_item == "Maize flour",
         consumed == "Yes",
         food_purchased == "Yes") %>% 
  rename("maizeflour_kg" = "quantity_kg_L") %>% 
  dplyr::select("hhid", "maizeflour_kg") %>% 
  # Left join AFE variable: 
  left_join(afe, by = "hhid")

# EDIBLE OIL:

edibleoil_ai <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_kg_L") %>% 
  filter(food_item == "Edible oil",
         consumed == "Yes",
         food_purchased == "Yes") %>% 
  rename("edibleoil_L" = "quantity_kg_L") %>% 
  dplyr::select("hhid", "edibleoil_L") %>% 
  # Left join AFE variable: 
  left_join(afe, by = "hhid")

# SUGAR:

sugar_ai <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_kg_L") %>% 
  filter(food_item == "Sugar",
         consumed == "Yes",
         food_purchased == "Yes") %>% 
  rename("sugar_kg" = "quantity_kg_L") %>% 
  dplyr::select("hhid", "sugar_kg") %>% 
  # Left join AFE variable: 
  left_join(afe, by = "hhid")

# SALT:

salt_ai <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_kg_L") %>% 
  filter(food_item == "Salt",
         consumed == "Yes",
         food_purchased == "Yes") %>% 
  rename("salt_kg" = "quantity_kg_L") %>% 
  dplyr::select("hhid", "salt_kg") %>% 
  # Left join AFE variable: 
  left_join(afe, by = "hhid")



