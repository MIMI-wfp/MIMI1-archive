################################################################################
############## FORTIFICATION VEHICLE REACH AND QUANTITIES CONSUMED #############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 18-Dec-2023
# Last edited: 28-Dec-2023

# This script is for extracting binarised (Yes/No) consumption of fortification 
# vehicles for each household, and the quantities consumed (in kg or L). 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "stringr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN FUNCTIONS:

# Source script required to create base models: 
source("all_base_models/scripts/base_model_functions.R")

# Source script required to get quantities of each fortification vehicle: 
source("fortification_models/scripts/fortification_model_functions.R")

#-------------------------------------------------------------------------------

#########################
#### PART 1: NIGERIA ####
#########################

# READ IN NIGERIA DATA: 

# Get base case apparent MN intake for each household
base_ai <- apparent_intake("nga1819")

# Note that the base_ai is based on MN values from UNFORTIFIED foods.

# Write csv to data folder:
# write_csv(base_ai, "fortification_models/data/nga1819_base_ai.csv")

# Remove objects that are no longer required:
rm(list = c("fct", "food_consumption", "afe", "full_item_list"))

# Read in food consumption module of the NLSS:
food_consumption <- read_csv("MIMI_data/nga/NGA_2018_LSS_v01_M_CSV/Household/sect6b_food_cons.csv")

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

# Calculate the quantity consumed as a multiple of 100 grams:
# (Multiply by 10, as conversion factor gives us quantity in kg)
food_consumption$quantity_100g <- food_consumption$quantity_consumed * food_consumption$conversion_factor * 10

# Create a column if at least some of the food item was purchased (i.e. quantity > 0, and not NA):
food_consumption$food_purchased <- ifelse(food_consumption$quantity_purchased > 0, 
                                          "Yes", "No")

# If NA in food_purchased column, then replace with "No":
food_consumption$food_purchased <- ifelse(is.na(food_consumption$food_purchased), 
                                          "No", food_consumption$food_purchased)

food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_100g")

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities: 
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g))

# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles: 

get_vehicle_quantities(base_ai, food_consumption)

# Save this data-frame as a .csv file:
# write_csv(vehicle_quantities,
#           "fortification_models/data/nga1819_vehicle_quantities.csv")

# Remove objects no longer required: 
rm(list = c("food_consumption", "vehicle_quantities", "base_ai"))

#-------------------------------------------------------------------------------

#########################
#### PART 2: MALAWI #####
#########################

# READ IN MALAWI DATA:  

# Get base case apparent intake data for Malawi: 
base_ai <- apparent_intake("mwi1516")

# Save data-frame for base_ai:
# write_csv(base_ai, "fortification_models/data/mwi1516_base_ai.csv")

# CHECK WITH GABRIEL SOURCE DATA - If it's the fourth integrated household survey
# then we have used the wrong dates in the naming structure (2016-2017 not 2015-2016)

# Read in food consumption data for Malawi:
food_consumption <- read_csv("all_base_models/data/mwi1516_food_consumption.csv")

# Select only required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Keep only required data: 
rm(fct)

# Get data for food purchases:
food_purchases <- read_csv("MIMI_data/mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_g1.csv")
food_purchases <- food_purchases %>% rename("hhid" = "HHID",
                                 "item_code" = "hh_g02",
                                 "quantity_purchased" = "hh_g04a") %>% 
  select("hhid", "item_code", "quantity_purchased")

# Keep only complete cases of food_purchases:
food_purchases <- na.omit(food_purchases)

# Check if there are any duplicate rows by "hhid" and "item_code" in food_purchases:
food_purchases[duplicated(food_purchases[c("hhid", "item_code")]), ]

# There are no duplicates, therefore left_join food_purchases to food_consumption:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

# Keep only required columns, and remove food_purchases df: 
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g", "quantity_purchased")

rm(food_purchases)

#-------------------------------------------------------------------------------

# FORTIFICATION VEHICLES: 

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
  # item_code == 116 ~ "Infant feeding cereals",
  item_code == 801 ~ "Sugar",
  item_code == 803 ~ "Edible oil",
  item_code == 810 ~ "Salt"
  # Other food items to be included once recipe data available (e.g. bread)
))

# TO RETURN TO THIS SECTION AFTER RECIPE DATA AVAILABLE: Need to add an additional
# column indicating the proportion of that food item that contains the fortificant.
# Use this data to calculate the quantity of the fortification vehicle consumed.

# Add a column to indicate if each food item was consumed (in last 7-days): 
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, "Yes", 
                                    ifelse(food_consumption$quantity_100g == 0, "No", NA))

# Add a column to indicate if each food item was purchased:
food_consumption$food_purchased <- ifelse(food_consumption$quantity_purchased > 0, "Yes", 
                                          ifelse(food_consumption$quantity_purchased == 0, "No", NA))

# Select required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", "quantity_100g") %>% 
  # Remove rows with missing values for food_item:
  drop_na("food_item")

# If food_consumption$food_purchased is NA, then replace value with "No":
food_consumption$food_purchased[is.na(food_consumption$food_purchased)] <- "No"

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities: 
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g))


# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles: 

get_vehicle_quantities(base_ai, food_consumption)

# Save this data-frame as a csv file: 
# write_csv(vehicle_quantities, "fortification_models/data/mwi1516_vehicle_quantities.csv")

# Remove objects no longer required: 
rm(list = c("afe", "base_ai", "food_consumption", "vehicle_quantities"))

#-------------------------------------------------------------------------------

###############################
#### PART 3: ETHIOPIA - ESS ###
###############################

# READ IN ETHIOPIA DATA (ESS):

# Get base case apparent intake data from ESS:
base_ai <- apparent_intake("ess1819")

# Save data-frame for base_ai:
# write_csv(base_ai, "fortification_models/data/ess1819_base_ai.csv")

# Read in food consumption data for ESS:
food_consumption <- read_csv("all_base_models/data/ess1819_food_consumption.csv")

# Select only required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Keep only required data:
rm(fct)

# Get data for food purchases from food consumption module:
food_purchases <- read_csv("MIMI_data/Ethiopia/ETH_2018_ESS_v03_M_CSV/sect6a_hh_w4.csv")

# Select and rename the columns that are required: 
food_purchases <- food_purchases %>% rename("hhid" = "household_id",
                                 "item_code" = "item_cd",
                                 "quantity_purchased" = "s6aq03a") %>% 
  select("hhid", "item_code", "quantity_purchased")

# Keep only complete cases of food_purchases:
food_purchases <- na.omit(food_purchases)

# Clean item_code column to include only numbers:
food_purchases$item_code <- str_extract(food_purchases$item_code, "\\d+") %>% 
  as.numeric()

# Check if there are any duplicate rows by "hhid" and "item_code" in food_purchases:
food_purchases[duplicated(food_purchases[c("hhid", "item_code")]), ] 

# There are no duplicates, therefore left_join food_purchases to food_consumption:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

rm(food_purchases)

#-------------------------------------------------------------------------------

# FORTIFICATION VEHICLES:

# Add a column called food_item which includes name of food item for all 
# fortification vehicles:

food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == 102 ~ "Wheat flour", # Wheat (Incl. Flour factory product)
  # item_code == 902 ~ "purchased bread/biscuit",
  # item_code == 903 ~ "Pasta/Macaroni",
  item_code == 104 ~ "Maize flour", # Maize (?Including flour)
  item_code == 107 ~ "Rice", 
  item_code == 708 ~ "Edible oil", # Oils (processed)
  item_code == 710 ~ "Sugar", 
  item_code == 712 ~ "Salt"
  # Other food items to be included once recipe data available (e.g. bread)
))

# TO RETURN TO THIS SECTION AFTER RECIPE DATA AVAILABLE: Need to add an additional
# column indicating the proportion of that food item that contains the fortificant.
# Use this data to calculate the quantity of the fortification vehicle consumed.

# Add a column to indicate if each food item was consumed (in last 7-days):
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, "Yes", 
                                    ifelse(food_consumption$quantity_100g == 0, "No", NA))

# Add a column to indicate if each food item was purchased:
food_consumption$food_purchased <- ifelse(food_consumption$quantity_purchased > 0, "Yes", 
                                          ifelse(food_consumption$quantity_purchased == 0, "No", NA))

# Select required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", "quantity_100g") %>% 
  # Remove rows with missing values for food_item:
  drop_na("food_item")

# If food_consumption$food_purchased is NA, then replace value with "No":
food_consumption$food_purchased[is.na(food_consumption$food_purchased)] <- "No"

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities:
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g))

# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles:
get_vehicle_quantities(base_ai, food_consumption)

# Save this data-frame as a csv file:
# write_csv(vehicle_quantities, "fortification_models/data/ess1819_vehicle_quantities.csv")

# Remove objects no longer required:
rm(list = c("afe", "base_ai", "food_consumption", "vehicle_quantities"))

#-------------------------------------------------------------------------------

###############################
### PART 4: ETHIOPIA - HICES ##
###############################

# READ IN ETHIOPIA DATA (HICES):

# Get base case apparent intake data from HICES:
base_ai <- apparent_intake("hices1516")

# Save data-frame for base_ai:
# write_csv(base_ai, "fortification_models/data/hices1516_base_ai.csv")

# Select only required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Note that there are many food items that are duplicated for households, 
# combine these rows and sum the quantities:
food_consumption <- food_consumption %>% 
  group_by(hhid, item_code) %>% 
  summarise(quantity_100g = sum(quantity_100g))

# Read in food and bevarage data-frame to get purchase info: 
food_purchases <- read_csv("MIMI_data/Ethiopia/eth_hces1516_foodbev/ETH_HCES1516_foodbev.csv")

# Select relevant variables: 
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "ITEMC", "TYPE") %>% 
  # And give variables appropriate names:
  rename("item_code" = "ITEMC",
         "food_purchased" = "TYPE")

# If the response in food_purchased is "In Cash", then replace with "Yes":
food_purchases$food_purchased[food_purchases$food_purchased == "In Cash"] <- "Yes"
# Replace all other responses with "No":
food_purchases$food_purchased[food_purchases$food_purchased != "Yes"] <- "No"

# Check if there are any duplicate rows by "hhid" and "item_code" in food_purchases:
food_purchases[duplicated(food_purchases[c("hhid", "item_code")]), ] 

# Since there are duplicates, combine rows that have the same "hhid" and "item_code",
# if any of the rows have "Yes" in the food_purchased column, keep the value "Yes":
food_purchases <- food_purchases %>% 
  group_by(hhid, item_code) %>% 
  summarise(food_purchased = ifelse(any(food_purchased == "Yes"), "Yes", "No"))

# Left join food_purchases to food_consumption:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

# Remove objects that are no longer required:
rm(food_purchases)

#-------------------------------------------------------------------------------

# FORTIFICATION VEHICLES:

# Add a column called food_item which includes name of food item for all
# fortification vehicles:

food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == "Wheat white, flour" ~ "Wheat flour",
  item_code == "Flour, factory product, mainly of wheat" ~ "Wheat flour",
  # ? Other types of wheat flour - clarify these with kevin
  item_code == "Maize, flour" ~ "Maize flour",
  item_code == "Rice" ~ "Rice",
  item_code == "Rice_duplicated_11202123" ~ "Rice",
  item_code == "Edible oil, local" ~ "Edible oil",
  item_code == "Edible oil, imported" ~ "Edible oil",
  item_code == "Sugar" ~ "Sugar",
  item_code == "Salt" ~ "Salt"
  # Other food items to be included once recipe data available, wheat flour products,
  # Also consider what to do with products that contain SUGAR, e.g. juices, condiments,
  # sweets.
))

# TO RETURN TO THIS SECTION AFTER RECIPE DATA AVAILABLE

# Add a column to indicate if each food item was consumed (in last 7-days):
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, "Yes", 
                                    ifelse(food_consumption$quantity_100g == 0, "No", NA))

# Select required columns:  
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", "quantity_100g") %>% 
  # Remove rows with missing values for food_item:
  drop_na("food_item")

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities:
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g))

# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles:
get_vehicle_quantities(base_ai, food_consumption)

# Save this data-frame as a csv file:
# write_csv(vehicle_quantities, "fortification_models/data/hices1516_vehicle_quantities.csv")

# Remove objects no longer required:
rm(list = c("afe", "base_ai", "fct", "food_consumption", "vehicle_quantities"))

#-------------------------------------------------------------------------------