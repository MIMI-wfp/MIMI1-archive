################################################################################
############## PREPARATION OF CSV FOR FORTIFIABLE FOOD QUANTITIES ##############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 23-02-2024
# Last edited: 

# This script produces a .csv file that countains the quantities of potentially 
# fortifiable foods in Ethiopia, using the HICES 2015-2016 dataset.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "readxl", "stringr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN THE DATA:

food_consumption <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_food_consumption.csv")

#-------------------------------------------------------------------------------

# DATA CLEANING: 

# Select only the required columns from food consumption: 

food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Some of the food items are duplicated for households, therefore combine these
# rows and sum the quantities: 
food_consumption <- food_consumption %>% 
  group_by(hhid, item_code) %>% 
  summarise(quantity_100g = sum(quantity_100g))

# For food items to be fortifiable, they need to be industrially processed and 
# therefore purchased. Therefore I read in food and beverage data to get purchase
# info:
food_purchases <- read_csv("MIMI_data/Ethiopia/eth_hces1516_foodbev/ETH_HCES1516_foodbev.csv")
# Note than consumption quantities in this data-frame are ANNUAL consumption.

# Select relevant variables: 
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "ITEMC", "TYPE", "MEASURE", "QUANTITY") %>% 
  # And give variables appropriate names:
  rename("item_code" = "ITEMC",
         "food_purchased" = "TYPE",
         "unit" = "MEASURE", # Note that all units are in grams or cubic cm (ML)
         "quantity" = "QUANTITY")

# Get daily quantities instead of annual: 
food_purchases$quantity_100g <- food_purchases$quantity / (365*100)
food_purchases$quantity <- NULL

# If the response in food_purchased is "In Cash", then replace with "Yes":
food_purchases$food_purchased[food_purchases$food_purchased == "In Cash"] <- "Yes"
# Replace all other responses with "No":
food_purchases$food_purchased[food_purchases$food_purchased != "Yes"] <- "No"

# If food was not purchased, then replace quantity with 0, then rename this
# variable appropriately:
food_purchases$quantity_100g[food_purchases$food_purchased == "No"] <- 0
food_purchases <- food_purchases %>% 
  rename("purchased_100g" = "quantity_100g")

# Since there are duplicates, combine rows that have the same "hhid" and "item_code",
# sum the quantities:
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "item_code", "purchased_100g") %>%
  group_by(hhid, item_code) %>% 
  summarise(purchased_100g = sum(purchased_100g))

# Left join food_purchases to food_consumption:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

# Sense check values by checking proportions: 
food_consumption$proportion_purchased <- food_consumption$purchased_100g / food_consumption$quantity_100g
summary(food_consumption$proportion_purchased)
hist(food_consumption$proportion_purchased)

# Values appear plausible and ranging from 0 to 1.

food_consumption$proportion_purchased <- NULL

# Remove objects that are no longer required:
rm(food_purchases)

#-------------------------------------------------------------------------------

# READ IN DATA ON FORTIFIABLE FOOD ITEMS: 

fortifiable_foods <- read_excel("data_rich/fortification_models/fortification_models_data_mapping.xlsx", 
                                sheet = "Ethiopia (HICES) food items")

# Select relevant columns:
fortifiable_foods <- fortifiable_foods %>% 
  dplyr::select("item_cd", "prop_fortifiable")

# Spacing in item_codes is causing issues when performing joing, remove spaces:
food_consumption$item_code <- gsub(" ", "", food_consumption$item_code)
fortifiable_foods$item_cd <- gsub(" ", "", fortifiable_foods$item_cd)

# Left join fortifiable_foods to food_consumption:
food_consumption <- left_join(food_consumption, fortifiable_foods, by = c("item_code" = "item_cd"))

rm(fortifiable_foods)

# Code NA values == 1:
food_consumption$prop_fortifiable[is.na(food_consumption$prop_fortifiable)] <- 1

# Create a column to indicate whether food items contained fortification vehicles, 
# and specify which: 

food_consumption <- food_consumption %>% mutate(vehicle = dplyr::case_when(
  item_code == "Wheatwhite,flour" ~ "Wheat flour",
  item_code == "Wheatmixed,flour" ~ "Wheat flour",
  item_code == "Wheatblack,flour" ~ "Wheat flour",
  item_code == "Wheat&Barley(Duragna),flour" ~ "Wheat flour",
  item_code == "Wheat&othercereals,flour" ~ "Wheat flour",
  item_code == "Flour,factoryproduct,mainlyofwheat" ~ "Wheat flour",
  item_code == "Bread(Dufo,Anbashaetc),Wheat-homemade" ~ "Wheat flour",
  item_code == "Bread,wheat-bakery" ~ "Wheat flour",
  item_code == "Donat/bombolino" ~ "Wheat flour",
  item_code == "Boresh(Dolchi)" ~ "Wheat flour",
  item_code == "Pizzas" ~ "Wheat flour",
  item_code == "Cakes" ~ "Wheat flour",
  item_code == "Biscuits" ~ "Wheat flour",
  item_code == "Baqlaba/Mushebek" ~ "Wheat flour",
  item_code == "Edibleoil,local" ~ "Edible oil",
  item_code == "Edibleoil,imported" ~ "Edible oil",
  item_code == "Sandwitch,meat/egg/vegetable,normal" ~ "Wheat flour",
  item_code == "Burger/clubsandwich" ~ "Wheat flour",
  item_code == "Breadoranypastryproductswithhotdrinks" ~ "Wheat flour",
  item_code == "BreadoranypastryproductsandJuice" ~ "Wheat flour",
  item_code == "Othersn.e.c." ~ "Wheat flour"
))

# Filter food_consumption to include only food items that contained wheat flour
# or edible oil:
food_consumption <- food_consumption %>% 
  filter(vehicle == "Wheat flour" | vehicle == "Edible oil")

# Since we are only interested in purchased quantities, remove the quantity_100g
# variable: 
food_consumption$quantity_100g <- NULL

# Remove rows where purchased_100g == 0:
food_consumption <- food_consumption %>% 
  filter(purchased_100g != 0)

# If there are duplicate rows for households and food items, combine rows into one
# and sum: 
food_consumption <- food_consumption %>% 
  group_by(hhid, item_code, vehicle, prop_fortifiable) %>% 
  summarise(purchased_100g = sum(purchased_100g))

# Re-name purchase_100g
food_consumption <- food_consumption %>% 
  rename("quantity_100g" = "purchased_100g")

# Select variables in order we want them: 
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "vehicle", "quantity_100g", "prop_fortifiable")

#-------------------------------------------------------------------------------

# Read in data on AFE: 
hh_info <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_hh_info.csv")

# Add AFE to food_consumption:
food_consumption <- food_consumption %>% 
  dplyr::left_join(hh_info %>% dplyr::select("hhid", "afe"), by = "hhid")

rm(hh_info)

# Divide quantity_100g by AFE to get quantity_100g per adult equivalent:
food_consumption$quantity_100g <- food_consumption$quantity_100g / food_consumption$afe

food_consumption$afe <- NULL

#-------------------------------------------------------------------------------

# Write csv: 
# write_csv(food_consumption, "training/ethiopia/data/eth_hices1516_fortifiable_foods.csv")

rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################
