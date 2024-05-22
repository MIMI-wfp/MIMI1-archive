################################################################################
############## FORTIFICATION VEHICLE REACH AND QUANTITIES CONSUMED #############
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 18-Dec-2023
# Last edited: 05-Feb-2024

# This script is for extracting binarised (Yes/No) consumption of fortification 
# vehicles for each household, and the quantities consumed (in kg or L). 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "stringr", "readxl")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN FUNCTIONS:

# Source script required to create base models: 
source("data_rich/all_base_models/scripts/base_model_functions.R")

# Source script required to get quantities of each fortification vehicle: 
source("data_rich/fortification_models/scripts/fortification_model_functions.R")

#-------------------------------------------------------------------------------

#########################
#### PART 1: NIGERIA ####
#########################

# READ IN NIGERIA DATA: 

# Get base case apparent MN intake for each household
base_ai <- apparent_intake("nga_lss1819")

# Note that the base_ai is based on MN values from UNFORTIFIED foods.

# Write csv to data folder:
# write_csv(base_ai, "data_rich/fortification_models/data/nga_lss1819_base_ai.csv")

# Remove objects that are no longer required:
rm(list = c("fc_table", "full_item_list"))

# Read in food consumption module of the NLSS:
food_purchases <- read_csv("MIMI_data/nga/NGA_2018_LSS_v01_M_CSV/Household/sect6b_food_cons.csv")

#-------------------------------------------------------------------------------

# Process data to indicate quantity of consumption that comes from purchases:

# Select required variables: 
food_purchases <- food_purchases %>% 
  rename(quantity_consumed = s06bq02a, 
         quantity_unit = s06bq02b,
         unit_other = s06bq02b_os,
         quantity_size = s06bq02c,
         conversion_factor = s06bq02_cvn,
         quantity_purchased = s06bq03) %>% 
  select(hhid, item_cd, quantity_consumed, quantity_unit, unit_other, 
         quantity_size, conversion_factor, quantity_purchased)

# Calculate daily daily quantity consumed that came from purchases:
food_purchases$purchased_100g <- (food_purchases$quantity_purchased * food_purchases$conversion_factor * 10) / 7

# Filter to include only required columns:
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "item_cd", "purchased_100g")

# Keep only complete entries:
food_purchases <- na.omit(food_purchases)

food_purchases <- food_purchases %>% 
  rename(item_code = item_cd)

#-------------------------------------------------------------------------------

# Filter data-frames and perform join: 

food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

food_consumption <- food_consumption %>% left_join(food_purchases, by = c("hhid", "item_code"))

rm(food_purchases)

#-------------------------------------------------------------------------------

# FORTIFIABLE PROPORTIONS OF COMPOSITE FOOD ITEMS:

# Read in data on proportion fortiable for each food item:
nigeria_proportions <- read_excel("data_rich/fortification_models/fortification_models_data_mapping.xlsx",
           sheet = "Nigeria food items")

nigeria_proportions <- nigeria_proportions %>% 
  dplyr::select("item_cd", "prop_fortifiable") %>% 
  rename("item_code" = "item_cd")

# Left join proportions to food_consumption: 
food_consumption <- food_consumption %>% left_join(nigeria_proportions, by = "item_code")

# Replace NA's with 1: 
food_consumption$prop_fortifiable[is.na(food_consumption$prop_fortifiable)] <- 1

# Multiply quantity_100g and purchased_100g by prop_fortifiable:
food_consumption$quantity_100g <- food_consumption$quantity_100g * food_consumption$prop_fortifiable
food_consumption$purchased_100g <- food_consumption$purchased_100g * food_consumption$prop_fortifiable

# Remove data that is no longer required: 
food_consumption$prop_fortifiable <- NULL
rm(nigeria_proportions)

#-------------------------------------------------------------------------------

# Add a column with name of food item based on "item_cd" for all fortification vehicles:
food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == 13 ~ "Rice", # Locally produced
  item_code == 14 ~ "Rice", # Imported
  item_code == 16 ~ "Maize flour",
  item_code == 19 ~ "Wheat flour",
  item_code == 25 ~ "Wheat flour", # Bread
  item_code == 26 ~ "Wheat flour", # Cake
  item_code == 27 ~ "Wheat flour", # Buns/pofpof/donuts
  item_code == 28 ~ "Wheat flour", # Biscuits
  item_code == 29 ~ "Wheat flour", # Meat pie/sausage roll
  item_code == 50 ~ "Edible oil", # Palm oil
  item_code == 52 ~ "Edible oil", # Groundnut oil
  item_code == 53 ~ "Edible oil", # Other oil and fat
  item_code == 130 ~ "Sugar", 
  item_code == 141 ~ "Salt"
)) 

# Add variable to indicate if the food item has been consumed (in last 7-days): 
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, 
                                    "Yes", "No")

# Create a column if at least some of the food item was purchased (i.e. quantity > 0):
food_consumption$food_purchased <- ifelse(food_consumption$purchased_100g > 0, 
                                          "Yes", "No")

#------------------------------------------------------------------------------

# Create column to indicate proportion purchased, use this to sense check values.
food_consumption$proportion_purchased <- food_consumption$purchased_100g / food_consumption$quantity_100g

summary(food_consumption$proportion_purchased)

# In some instances, household have reported consumption from purchases is greater
# than total consumption, which is not possible. Therefore, in these instances,
# code purchased_100g to equal quantity_100g:
food_consumption$purchased_100g <- ifelse(food_consumption$purchased_100g > food_consumption$quantity_100g, 
                                          food_consumption$quantity_100g, food_consumption$purchased_100g)

# Now repeat the sense check:
food_consumption$proportion_purchased <- food_consumption$purchased_100g / food_consumption$quantity_100g

summary(food_consumption$proportion_purchased)

# These are now satisfactory.

food_consumption$proportion_purchased <- NULL

#-------------------------------------------------------------------------------


# If NA in food_purchased column, then replace with "No":
food_consumption$food_purchased <- ifelse(is.na(food_consumption$food_purchased), 
                                          "No", food_consumption$food_purchased)

food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", 
                "quantity_100g", "purchased_100g")

# Filter food_consumption to include only rows where food_item is a fortification vehicle:
food_consumption <- food_consumption %>% 
  filter(!is.na(food_item))

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, summing the quantities for quantity_100g and
# purchased_100g:
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g),
            purchased_100g = sum(purchased_100g))

# write_csv(food_consumption, "data_rich/fortification_models/data/gabriel/nga_lss1819_vehicle_consumption.csv")


# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles: 

get_vehicle_quantities(base_ai, food_consumption, hh_info)


# Save this data-frame as a .csv file:
# write_csv(vehicle_quantities, "data_rich/fortification_models/data/nga_lss1819_vehicle_quantities.csv")

# Remove objects no longer required: 
rm(list = c("food_consumption", "vehicle_quantities", "base_ai", "hh_info"))

#-------------------------------------------------------------------------------

#########################
#### PART 2: MALAWI #####
#########################

# READ IN MALAWI DATA:  

# Get base case apparent intake data for Malawi: 
base_ai <- apparent_intake("mwi_ihs1617")

# Save data-frame for base_ai:
# write_csv(base_ai, "data_rich/fortification_models/data/mwi_ihs1617_base_ai.csv")

# Select only required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Keep only required data: 
rm(fc_table)

# Get data for food purchases:
food_purchases <- read_csv("MIMI_data/mwi/MWI_2016_IHS-IV_v04_M_CSV/household/hh_mod_g1.csv")
food_purchases <- food_purchases %>% rename("hhid" = "HHID",
                                 "item_code" = "hh_g02",
                                 "quantity_consumed" = "hh_g03a",
                                 "quantity_purchased" = "hh_g04a",
                                 "quantity_unit" = "hh_g04b_label") %>% 
  select("hhid", "item_code", "quantity_consumed","quantity_purchased", "quantity_unit")

# Calculate proportion of consumption that came from purchases: 
food_purchases$proportion_purchased <- food_purchases$quantity_purchased / food_purchases$quantity_consumed

summary(food_purchases$proportion_purchased)

# Note that there are values >1, probably due to misreporting. Code these as 1:
food_purchases$proportion_purchased[food_purchases$proportion_purchased > 1] <- 1

summary(food_purchases$proportion_purchased)
hist(food_purchases$proportion_purchased)

# Now filter food purchases to include only required variables: 
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "item_code","proportion_purchased")

# Keep only complete cases of food_purchases:
food_purchases <- na.omit(food_purchases)

# Check if there are any duplicate rows by "hhid" and "item_code" in food_purchases:
food_purchases[duplicated(food_purchases[c("hhid", "item_code")]), ]

# There are no duplicates, therefore left_join food_purchases to food_consumption, do not round:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

# Keep only required columns, and remove food_purchases df: 
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g", "proportion_purchased")

rm(food_purchases)

# Create a column to indicate quantity of each food item purchased:
food_consumption$purchased_100g <- food_consumption$quantity_100g * food_consumption$proportion_purchased

# Remove the proportion_purchased column (no longer required):
food_consumption <- dplyr::select(food_consumption, -proportion_purchased)

#-------------------------------------------------------------------------------

# FORTIFIABLE PROPORTIONS OF COMPOSITE FOOD ITEMS:

# Read in the data for fortifiable proportions:
malawi_proportions <- read_excel("data_rich/fortification_models/fortification_models_data_mapping.xlsx",
                                 sheet = "Malawi food items")

# Keep only required columns:
malawi_proportions <- malawi_proportions %>% 
  dplyr::select("item_cd", "prop_fortifiable") %>% 
  rename("item_code" = "item_cd")

# Data-frame full of NA's, keep complete cases: 
malawi_proportions <- na.omit(malawi_proportions)

# Left join malawi_proportions to food_consumption:
food_consumption <- left_join(food_consumption, malawi_proportions, by = "item_code")

rm(malawi_proportions)

# Record NA proportions as 1:
food_consumption$prop_fortifiable[is.na(food_consumption$prop_fortifiable)] <- 1

# Multiply quantity_100g and purchased_100g by prop_fortifiable:
food_consumption$quantity_100g <- food_consumption$quantity_100g * food_consumption$prop_fortifiable
food_consumption$purchased_100g <- food_consumption$purchased_100g * food_consumption$prop_fortifiable

# Remove the prop_fortifiable column (no longer required):
food_consumption$prop_fortifiable <- NULL

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
  item_code == 111 ~ "Wheat flour", # Bread
  item_code == 112 ~ "Wheat flour", # Buns, scones
  item_code == 827 ~ "Wheat flour", # Mandazi, doughnut (vendor)
  item_code == 801 ~ "Sugar",
  item_code == 803 ~ "Edible oil",
  item_code == 810 ~ "Salt"
))


# Add a column to indicate if each food item was consumed (in last 7-days): 
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, "Yes", 
                                    ifelse(food_consumption$quantity_100g == 0, "No", NA))

# Add a column to indicate if each food item was purchased:
food_consumption$food_purchased <- ifelse(food_consumption$purchased_100g > 0, "Yes", 
                                          ifelse(food_consumption$purchased_100g == 0, "No", NA))

# Select required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", "quantity_100g", "purchased_100g") %>% 
  # Remove rows with missing values for food_item:
  drop_na("food_item")

# If food_consumption$food_purchased is NA, then replace value with "No":
food_consumption$food_purchased[is.na(food_consumption$food_purchased)] <- "No"

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities: 
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g),
            purchased_100g = sum(purchased_100g))


# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles: 

get_vehicle_quantities(base_ai, food_consumption, hh_info)

# Save this data-frame as a csv file: 
# write_csv(vehicle_quantities, "data_rich/fortification_models/data/mwi_ihs1617_vehicle_quantities.csv")

# NOTE THAT CONSUMPTION FOR MAIZE FLOUR AND SALT APPEAR SUSPICIOUSLY HIGH, 
# RETURN TO THIS WHEN PERFORMING ANALYSES FOR MALAWI.

# Remove objects no longer required: 
rm(list = c("hh_info", "base_ai", "food_consumption", "vehicle_quantities"))

#-------------------------------------------------------------------------------

###############################
#### PART 3: ETHIOPIA - ESS ###
###############################

# READ IN ETHIOPIA DATA (ESS):

# Get base case apparent intake data from ESS:
base_ai <- apparent_intake("eth_ess1819")

# Save data-frame for base_ai:
# write_csv(base_ai, "data_rich/fortification_models/data/eth_ess1819_base_ai.csv")

# Select only required columns from food_consumption:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Keep only required data:
rm(fc_table)

# Get data for food purchases from food consumption module:
food_purchases <- read_csv("MIMI_data/Ethiopia/ETH_2018_ESS_v03_M_CSV/sect6a_hh_w4.csv")

# Select and rename the columns that are required: 
food_purchases <- food_purchases %>% rename("hhid" = "household_id",
                                 "item_code" = "item_cd",
                                 "quantity_consumed" = "s6aq02a",
                                 "quantity_purchased" = "s6aq03a") %>% 
  select("hhid", "item_code","quantity_consumed", "quantity_purchased")

# Keep only complete cases of food_purchases:
food_purchases <- na.omit(food_purchases)

# Clean item_code column to include only numbers:
food_purchases$item_code <- str_extract(food_purchases$item_code, "\\d+") %>% 
  as.numeric()

# Calculate proportion of consumption that came from purchases:
food_purchases$proportion_purchased <- food_purchases$quantity_purchased / food_purchases$quantity_consumed

# Sense check values: 
summary(food_purchases$proportion_purchased)
hist(food_purchases$proportion_purchased)

# Values range from 0 to 1, therefore no reporting errors.

# Filter food purchases to include only required variables: 
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "item_code", "proportion_purchased")

# Keep only complete cases of food_purchases:
food_purchases <- na.omit(food_purchases)

# Check if there are any duplicate rows by "hhid" and "item_code" in food_purchases:
food_purchases[duplicated(food_purchases[c("hhid", "item_code")]), ]

# There are no duplicates, therefore left_join food_purchases to food_consumption:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

# Keep only required columns, and remove food_purchases df: 
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g", "proportion_purchased")

rm(food_purchases)

# Create a column to indicate quantity of each food item purchased: 
food_consumption$purchased_100g <- food_consumption$quantity_100g * food_consumption$proportion_purchased

# Remove proportion_purchased column:
food_consumption <- dplyr::select(food_consumption, -proportion_purchased)

#-------------------------------------------------------------------------------

# FORTIFIABLE PROPORTIONS OF COMPOSITE FOOD ITEMS: 

ess_proportions <- read_excel("data_rich/fortification_models/fortification_models_data_mapping.xlsx", 
                              sheet = "Ethiopia (ESS) food items")

ess_proportions <- ess_proportions %>% 
  dplyr::select("item_cd", "prop_fortifiable")

# Left join proportions to food consumption data: 
food_consumption <- left_join(food_consumption, ess_proportions, by = c("item_code" = "item_cd"))

rm(ess_proportions)

# Code NA proportions == 1: 
food_consumption$prop_fortifiable[is.na(food_consumption$prop_fortifiable)] <- 1

# Multiply quantity_100g and purchased_100g by prop_fortifiable:
food_consumption$quantity_100g <- food_consumption$quantity_100g * food_consumption$prop_fortifiable
food_consumption$purchased_100g <- food_consumption$purchased_100g * food_consumption$prop_fortifiable

# Remove prop_fortifiable column:
food_consumption$prop_fortifiable <- NULL

#-------------------------------------------------------------------------------

# FORTIFICATION VEHICLES:

# Add a column called food_item which includes name of food item for all 
# fortification vehicles:

food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == 102 ~ "Wheat flour", # Wheat (Incl. Flour factory product)
  item_code == 902 ~ "Wheat flour", # Bread/biscuit
  item_code == 104 ~ "Maize flour", # Maize (?Including flour)
  item_code == 107 ~ "Rice", 
  item_code == 708 ~ "Edible oil", # Oils (processed)
  item_code == 710 ~ "Sugar", 
  item_code == 712 ~ "Salt",
  item_code == 902 ~ "Wheat flour" # Purchased bread/biscuit
))


# Add a column to indicate if each food item was consumed (in last 7-days):
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, "Yes", 
                                    ifelse(food_consumption$quantity_100g == 0, "No", NA))

# Add a column to indicate if each food item was purchased:
food_consumption$food_purchased <- ifelse(food_consumption$purchased_100g > 0, "Yes", 
                                          ifelse(food_consumption$purchased_100g == 0, "No", NA))

# Select required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", "quantity_100g", "purchased_100g") %>% 
  # Only keep rows where a fortification vehicle was consumed:
  drop_na("food_item")

# If food_consumption$food_purchased is NA, then replace value with "No":
food_consumption$food_purchased[is.na(food_consumption$food_purchased)] <- "No"

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities:
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g),
            purchased_100g = sum(purchased_100g))

# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles:
get_vehicle_quantities(base_ai, food_consumption, hh_info)

# Save this data-frame as a csv file:
# write_csv(vehicle_quantities, "data_rich/fortification_models/data/eth_ess1819_vehicle_quantities.csv")

# Remove objects no longer required:
rm(list = c("hh_info", "base_ai", "food_consumption", "vehicle_quantities"))

#-------------------------------------------------------------------------------

###############################
### PART 4: ETHIOPIA - HICES ##
###############################

# READ IN ETHIOPIA DATA (HICES):

# Get base case apparent intake data from HICES:
base_ai <- apparent_intake("eth_hices1516")

# Note that there are complete duplicates in base_ai, remove these entries: 
base_ai <- base_ai[!duplicated(base_ai), ]

# Also duplicated hhid's in hh_info:
hh_info <- hh_info %>% 
  dplyr::distinct(hhid, .keep_all = TRUE)

# Save data-frame for base_ai:
# write_csv(base_ai, "data_rich/fortification_models/data/eth_hices1516_base_ai.csv")

rm(fc_table)

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

# FORTIFIABLE PROPORTIONS OF COMPOSITE FOOD ITEMS: 

hices_proportions <- read_excel("data_rich/fortification_models/fortification_models_data_mapping.xlsx", 
                                sheet = "Ethiopia (HICES) food items")

hices_proportions <- hices_proportions %>% 
  dplyr::select("item_cd", "prop_fortifiable")

# Spacing in item_codes is causing issues when performing joing, remove spaces:
food_consumption$item_code <- gsub(" ", "", food_consumption$item_code)
hices_proportions$item_cd <- gsub(" ", "", hices_proportions$item_cd)


# Left join proportions to food consumption data: 
food_consumption <- left_join(food_consumption, hices_proportions, by = c("item_code" = "item_cd"))

rm(hices_proportions)

# Code NA values == 1: 
food_consumption$prop_fortifiable[is.na(food_consumption$prop_fortifiable)] <- 1

# Multiply quantity_100g and purchased_100g by prop_fortifiable:
food_consumption$quantity_100g <- food_consumption$quantity_100g * food_consumption$prop_fortifiable
food_consumption$purchased_100g <- food_consumption$purchased_100g * food_consumption$prop_fortifiable

# Remove prop_fortifiable:
food_consumption$prop_fortifiable <- NULL

#-------------------------------------------------------------------------------

# FORTIFICATION VEHICLES:

# Add a column called food_item which includes name of food item for all
# fortification vehicles:

food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == "Wheatwhite,flour" ~ "Wheat flour",
  item_code == "Wheatmixed,flour" ~ "Wheat flour",
  item_code == "Wheatblack,flour" ~ "Wheat flour",
  item_code == "Wheat&Barley(Duragna),flour" ~ "Wheat flour",
  item_code == "Wheat&othercereals,flour" ~ "Wheat flour",
  item_code == "Flour,factoryproduct,mainlyofwheat" ~ "Wheat flour",
  item_code == "Maize,flour" ~ "Maize flour",
  item_code == "Rice" ~ "Rice",
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
  item_code == "Sugar" ~ "Sugar",
  item_code == "Salt" ~ "Salt",
  item_code == "Sandwitch,meat/egg/vegetable,normal" ~ "Wheat flour",
  item_code == "Burger/clubsandwich" ~ "Wheat flour",
  item_code == "Breadoranypastryproductswithhotdrinks" ~ "Wheat flour",
  item_code == "BreadoranypastryproductsandJuice" ~ "Wheat flour",
  item_code == "Othersn.e.c." ~ "Wheat flour",
  item_code == "Rice_duplicated_11202123" ~ "Rice"
))


# Add a column to indicate if each food item was consumed (in last 7-days):
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, "Yes", 
                                    ifelse(food_consumption$quantity_100g == 0, "No", NA))

# Add a column to indicate if each food item was purchased (in last 7-days):
food_consumption$food_purchased <- ifelse(food_consumption$purchased_100g > 0, "Yes", 
                                          ifelse(food_consumption$purchased_100g == 0, "No", NA))

# Select required columns:  
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", "quantity_100g",
                "purchased_100g") %>% 
  # Select only rows where fortification vehicle was consumed:
  drop_na("food_item")

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities:
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g),
            purchased_100g = sum(purchased_100g))

# write_csv(food_consumption, "data_rich/fortification_models/data/eth_hices1516_vehicle_consumption.csv")

# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles:
get_vehicle_quantities(base_ai, food_consumption, hh_info)


# Note that salt consumption not recorded in this survey.

# Save this data-frame as a csv file:
# write_csv(vehicle_quantities, "data_rich/fortification_models/data/eth_hices1516_vehicle_quantities.csv")

# Remove objects no longer required:
rm(list = c("hh_info", "base_ai", "food_consumption", "vehicle_quantities"))

#-------------------------------------------------------------------------------

########################
#### PART 5: INDIA  ####
########################

# READ IN INDIA DATA (NSSO):

# Get base case apparent intake data from NSSO:
base_ai <- apparent_intake("ind_nss1112")


# Save data-frame for base_ai:
# write_csv(base_ai, "data_rich/fortification_models/data/ind_nss1112_base_ai.csv")



# Select only required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "item_code", "quantity_100g")

# Get data on food purchases (from Block 5 and 6): 
food_purchases <- read_csv("MIMI_data/India/nsso_data_subset/block_5_6_food_consumption.csv")

# Select relevant variables:
food_purchases <- food_purchases %>% 
  dplyr::select("HHID", "Item_Code", "Total_Consumption_Quantity",
                "Home_Produce_Quantity") %>% 
  # Rename variables:
  rename("hhid" = "HHID",
         "item_code" = "Item_Code",
         "total_consumed" = "Total_Consumption_Quantity",
         "home_produced" = "Home_Produce_Quantity")

# If there are NA values in total_consumed and home_produced, then replace with 0:
food_purchases$total_consumed[is.na(food_purchases$total_consumed)] <- 0
food_purchases$home_produced[is.na(food_purchases$home_produced)] <- 0

# Get quantity of consumption that came from purchases, and proportions:
food_purchases$quantity_purchased <- food_purchases$total_consumed - food_purchases$home_produced
food_purchases$proportion_purchased <- food_purchases$quantity_purchased / food_purchases$total_consumed

# Sense check proportions: 
summary(food_purchases$proportion_purchased)
hist(food_purchases$proportion_purchased)
# Proportions are all plausible values ranging between 0 and 1.

# Filter food_purchases to include only required variables: 
food_purchases <- food_purchases %>% 
  dplyr::select("hhid", "item_code", "proportion_purchased")

# Check if there are any duplicate rows by "hhid" and "item_code" in food_purchases:
food_purchases[duplicated(food_purchases[c("hhid", "item_code")]), ]
# There are no duplicates

# Remove values where proportion is NaN (0 divided by 0): 
food_purchases <- food_purchases[!is.nan(food_purchases$proportion_purchased), ]

# Code hhid and item_code as numeric data-type:
food_purchases$hhid <- as.numeric(food_purchases$hhid)
food_purchases$item_code <- as.numeric(food_purchases$item_code)

# Left join food_purchases to food_consumption:
food_consumption <- left_join(food_consumption, food_purchases, by = c("hhid", "item_code"))

# Remove objects that are no longer required:
rm(food_purchases)

#-------------------------------------------------------------------------------

# FORTIFIABLE PROPORTIONS OF COMPOSITE FOOD ITEMS: 

# Read in proportions data: 
ind_proportions <- read_excel("data_rich/fortification_models/fortification_models_data_mapping.xlsx", 
                              sheet = "India food items") %>% 
  dplyr::select("item_cd", "prop_fortifiable")

# Left join proportions to food consumption data: 
food_consumption <- left_join(food_consumption, ind_proportions, by = c("item_code" = "item_cd"))

rm(ind_proportions)

# Replace NA proportions with 1: 
food_consumption$prop_fortifiable[is.na(food_consumption$prop_fortifiable)] <- 1

# Use proportion_purchased to calculate quantities that came from purchases: 
food_consumption$purchased_100g <- food_consumption$quantity_100g * food_consumption$proportion_purchased

# Multiply quantity_100g and purchased_100g by prop_fortifiable:
food_consumption$quantity_100g <- food_consumption$quantity_100g * food_consumption$prop_fortifiable
food_consumption$purchased_100g <- food_consumption$purchased_100g * food_consumption$prop_fortifiable

food_consumption$prop_fortifiable <- NULL

#-------------------------------------------------------------------------------

# FORTIFICATION VEHICLES:

# Add a column called food_item which includes name of food item for all
# fortification vehicles:

food_consumption <- food_consumption  %>% mutate(food_item = dplyr::case_when(
  item_code == 101 ~ "Rice", # Rice - PDS
  item_code == 102 ~ "Rice", # Rice - other sources
  item_code == 107 ~ "Wheat flour", # Wheat/atta - PDS
  item_code == 108 ~ "Wheat flour", # Wheat/atta - other sources
  item_code == 110 ~ "Wheat flour", # Maida
  item_code == 113 ~ "Wheat flour", # Bread (bakery)
  item_code == 170 ~ "Salt",
  item_code == 171 ~ "Sugar", # Sugar - PDS
  item_code == 172 ~ "Sugar", # Sugar - other sources
  item_code == 181 ~ "Edible oil", # Mustard oil
  item_code == 182 ~ "Edible oil", # Groundnut oil
  item_code == 183 ~ "Edible oil", # Coconut oil
  item_code == 184 ~ "Edible oil", # Refined oil [sunflower, soyabean, saffola, etc.]
  item_code == 185 ~ "Edible oil", # Edible oil: others
))

# Add a column to indicate if each food item was consumed (in last 7-days):
food_consumption$consumed <- ifelse(food_consumption$quantity_100g > 0, "Yes", 
                                    ifelse(food_consumption$quantity_100g == 0, "No", NA))

# Add a column to indicate if each food item was purchased (in last 7-days):
food_consumption$food_purchased <- ifelse(food_consumption$proportion_purchased > 0, "Yes", 
                                          ifelse(food_consumption$proportion_purchased == 0, "No", NA))

# Select required columns:
food_consumption <- food_consumption %>% 
  dplyr::select("hhid", "food_item", "consumed", "food_purchased", "quantity_100g",
                "purchased_100g") %>% 
  # Remove rows with missing values for food_item:
  drop_na("food_item")

# If a household has both consumed and purchased a particular food item, then
# combine the rows into one row, and sum the quantities:
food_consumption <- food_consumption %>% 
  group_by(hhid, food_item, consumed, food_purchased) %>% 
  summarise(quantity_100g = sum(quantity_100g),
            purchased_100g = sum(purchased_100g))

# write_csv(food_consumption, "data_rich/fortification_models/data/gabriel/ind_nss1112_vehicle_consumption.csv")

# Create a data-frame to indicate consumption (including quantities), of each of
# the fortification vehicles:
get_vehicle_quantities(base_ai, food_consumption, hh_info)

# Save this data-frame as a csv file:
# write_csv(vehicle_quantities, "data_rich/fortification_models/data/ind_nss1112_vehicle_quantities.csv")

# Remove objects no longer required:
rm(list = c("hh_info", "base_ai", "food_consumption", "vehicle_quantities",
            "fc_table"))

#-------------------------------------------------------------------------------

# Remove all objects:
rm(list = ls())

################################################################################
############################## END OF SCRIPT ###################################
################################################################################


