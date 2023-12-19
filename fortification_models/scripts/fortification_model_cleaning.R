################################################################################
#### SCRIPT FOR EXTRACTING AND CLEANING VARIABLES FOR FORTIFICATION MODELS #####
################################################################################

# Author: Mo Osman
# Date created: 18-Dec-2023
# Last edited: 

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
# source("all_base_models/scripts/base_model_functions.R")
# Re-add above line of code once base model functions have been fixed.

#-------------------------------------------------------------------------------

# NIGERIA

# Read-in base model data:
nga_food_consumption <- read_csv("all_base_models/data/nga1819_food_consumption.csv")
nga_fct <- read_csv("all_base_models/data/nga1819_fct.csv")
nga_afe <- read_csv("all_base_models/data/nga1819_afe.csv")

nga_fct$item_code <- as.numeric(nga_fct$item_code)

# Create full list of food items for every household: 
full_item_list <- nga_afe %>% 
  select(hhid) %>% 
  cross_join(nga_fct %>% 
               select(item_code)) %>% 
  left_join(nga_food_consumption, 
            by = c("hhid", "item_code")) %>% 
  select(-food_group) %>% 
  mutate(
    across(
      c(quantity_100g, quantity_g),
      ~replace_na(.,0)
    )
  ) %>% 
  left_join(nga_fct, by = "item_code")

# Estimate base case apparent MN intake for each household:
base_ai <- nga_food_consumption %>% 
  left_join(nga_fct, by = "item_code") %>% 
  mutate(
    across(
      -c(item_code, hhid,food_group, quantity_100g, quantity_g),
      ~.x*quantity_100g
    )
  ) %>% 
  group_by(hhid) %>% 
  summarise(
    across(-c(item_code,quantity_100g,quantity_g, food_group),
           ~sum(.,na.rm = T))
  ) %>% 
  left_join(nga_afe, by = "hhid") %>% 
  mutate(
    across(
      -c(hhid,afe),
      ~.x/afe
    )
  ) %>% 
  ungroup()

# Note that the base_ai is based on MN values from UNFORTIFIED foods:

# Calculate additional apparent intake achieved from fortifying each of the following foods: 

# WHEAT FLOUR: 

# MAIZE FLOUR: 

# RICE: 

# EDIBLE OIL:

# SUGAR:

# SALT:

# ?BOUILLON:


