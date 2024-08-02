################################################################################
######### SCRIPT FOR STANDARDISING FOOD GROUPINGS ACROSS MIMI COUNTRIES ########
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 01-08-2024
# Last edited: 

# In this script, I will standardise the food groupings that are used across all
# MIMI countries. We will use the MDD-W food groupings, any food items that do not
# fit into one of these food groupings will be labelled as "Misc" (miscellaneous).

# MDD-W food groupings:
# https://inddex.nutrition.tufts.edu/data4diets/indicator/minimum-dietary-diversity-women-mdd-w

#-------------------------------------------------------------------------------

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "stringr", "readxl")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# List to specify if MIMI country cleaned data is already correctly in the MDD-W
# food groupings: 

# Tanzania: Yes
# Nigeria: No
# Ethiopia (ESS): No
# Ethiopia (HICES): No
# India: No

# Therefore need to edit food groupings for all countries except for Nigeria. 

#-------------------------------------------------------------------------------

# READ IN CSV FILES:

nga_food_consumption <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_food_consumption.csv")
eth_ess_food_consumption <- read_csv("data_rich/all_base_models/data/current/eth_ess1819_food_consumption.csv")
ind_food_consumption <- read_csv("data_rich/all_base_models/data/current/ind_nss1112_food_consumption.csv")
eth_hices_food_consumption <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_food_consumption.csv")

# Store data-frames in a list to keep environment tidy: 
food_consumption_list <- list(nga_food_consumption, eth_ess_food_consumption, 
                              ind_food_consumption, eth_hices_food_consumption)
rm(nga_food_consumption, eth_ess_food_consumption, eth_hices_food_consumption, ind_food_consumption)

# Read in MDD-W data dictionaries: 
nga_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/nga_mdd_w.csv")
ess_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/ess_mdd_w.csv")
ind_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/nsso_mdd_w.csv")
hices_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/hices_mdd_w.csv")


# Store data-frames in a list to keep environment tidy:
mddw_list <- list(nga_mddw, ess_mddw, ind_mddw, hices_mddw)
rm(ess_mddw, hices_mddw, ind_mddw, nga_mddw)

#-------------------------------------------------------------------------------

# BINARY FOOD GROUP CONSUMPTION FOR EACH HOUSEHOLD ID - DATA REQUEST:

# For each tibble in the food_consumption_list, keep only hhid and item_code: 
for (i in 1:length(food_consumption_list)) {
  food_consumption_list[[i]] <- food_consumption_list[[i]] %>% 
    dplyr::select(hhid, item_code)
}

# For each tibble in the food_consumption_list, left join the MDDW list: 
for (i in 1:length(food_consumption_list)) {
  food_consumption_list[[i]] <- left_join(food_consumption_list[[i]], mddw_list[[i]], 
                                          by = "item_code") %>% 
    dplyr::select(-item_code)
}

# Remove item_name columns where applicable: 
for(i in 1:3) {
  food_consumption_list[[i]] <- food_consumption_list[[i]] %>% 
    dplyr::select(-item_name)
}

# Filter each tibble to keep only one entry per household ID, but indicating if
# that household has consumed any of the food groups, with a binary variable (1 or 0): 
for (i in 1:length(food_consumption_list)) {
  food_consumption_list[[i]] <- food_consumption_list[[i]] %>% 
    group_by(hhid) %>% 
    summarise_all(funs(sum), na.rm = TRUE) %>% 
    mutate(across(!hhid, ~ifelse(. > 0, 1, 0)))
}

# Rename columns to indicate food groupings: 
for (i in 1:length(food_consumption_list)) {
  food_consumption_list[[i]] <- food_consumption_list[[i]] %>% 
    rename(grains_roots_tubers = cereals,
           meat_poultry_fish = asf, 
           green_leafy_veg = green_veg)
}

names(food_consumption_list) <- c("nga_food_consumption", "eth_ess_food_consumption", 
                                  "ind_food_consumption", "eth_hices_food_consumption")

# Unpackage list elements: 
list2env(food_consumption_list, envir = .GlobalEnv)

rm(i, mddw_list, food_consumption_list)

#-------------------------------------------------------------------------------

# WRITE DATA: 
# write_csv(nga_food_consumption, "data_rich/data_requests/food_groups_202408/nga_lss1819_food_groups.csv")
# write_csv(eth_ess_food_consumption, "data_rich/data_requests/food_groups_202408/eth_ess1819_food_groups.csv")
# write_csv(ind_food_consumption, "data_rich/data_requests/food_groups_202408/ind_nss1112_food_groups.csv")
# write_csv(eth_hices_food_consumption, "data_rich/data_requests/food_groups_202408/eth_hices1516_food_groups.csv")

# Clear environment: 
rm(list = ls())

#-------------------------------------------------------------------------------

# RE-READ IN CSV FILES:
nga_food_consumption <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_food_consumption.csv")
eth_ess_food_consumption <- read_csv("data_rich/all_base_models/data/current/eth_ess1819_food_consumption.csv")
ind_food_consumption <- read_csv("data_rich/all_base_models/data/current/ind_nss1112_food_consumption.csv")
eth_hices_food_consumption <- read_csv("data_rich/all_base_models/data/current/eth_hices1516_food_consumption.csv")

# Store data-frames in a list to keep environment tidy: 
food_consumption_list <- list(nga_food_consumption, eth_ess_food_consumption, 
                              ind_food_consumption, eth_hices_food_consumption)
rm(nga_food_consumption, eth_ess_food_consumption, eth_hices_food_consumption, ind_food_consumption)

# Read in MDD-W data dictionaries: 
nga_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/nga_mdd_w.csv")
ess_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/ess_mdd_w.csv")
ind_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/nsso_mdd_w.csv")
hices_mddw <- read_csv("data_rich/all_base_models/data_dictionaries/mddw/hices_mdd_w.csv")


# Store data-frames in a list to keep environment tidy:
mddw_list <- list(nga_mddw, ess_mddw, ind_mddw, hices_mddw)
rm(ess_mddw, hices_mddw, ind_mddw, nga_mddw)

#-------------------------------------------------------------------------------

# Rename columns to correctly indicate food groupings: 
for (i in 1:length(mddw_list)) {
  mddw_list[[i]] <- mddw_list[[i]] %>% 
    rename(grains_roots_tubers = cereals,
           meat_poultry_fish = asf, 
           green_leafy_veg = green_veg)
}

# For each tibble the mddw_list, identify which column contains a 1 for each row, 
# store this information in a new column:
for (i in 1:length(mddw_list)) {
  mddw_list[[i]] <- mddw_list[[i]] %>% 
    mutate(food_group = case_when(
      grains_roots_tubers == 1 ~ "grains_roots_tubers",
      pulses == 1 ~ "pulses",
      nuts_seeds == 1 ~ "nuts_seeds",
      dairy == 1 ~ "dairy",
      meat_poultry_fish == 1 ~ "meat_poultry_fish",
      eggs == 1 ~ "eggs",
      green_leafy_veg == 1 ~ "green_leafy_veg",
      vita_fruit_veg == 1 ~ "vita_fruit_veg",
      other_veg == 1 ~ "other_veg",
      other_fruit == 1 ~ "other_fruit",
      TRUE ~ "misc"
    )) %>% 
    dplyr::select(item_code, food_group)
}

# For each tibble in the food_consumption_list, de-select the existing food_group
# variable: 
for(i in 1:length(food_consumption_list)) {
  food_consumption_list[[i]] <- food_consumption_list[[i]] %>% 
    dplyr::select(-food_group)
}

# Then attach new food_group variable: 
for (i in 1:length(food_consumption_list)) {
  food_consumption_list[[i]] <- left_join(food_consumption_list[[i]], mddw_list[[i]], by = "item_code")
}

rm(i, mddw_list)

#-------------------------------------------------------------------------------

# Unpackage list elements: 
names(food_consumption_list) <- c("nga_food_consumption", "eth_ess_food_consumption", 
                                  "ind_food_consumption", "eth_hices_food_consumption")

list2env(food_consumption_list, envir = .GlobalEnv)
rm(food_consumption_list)

#-------------------------------------------------------------------------------

# WRITE DATA:
# write_csv(nga_food_consumption, "data_rich/all_base_models/data/current/nga_lss1819_food_consumption.csv")
# write_csv(eth_ess_food_consumption, "data_rich/all_base_models/data/current/eth_ess1819_food_consumption.csv")
# write_csv(ind_food_consumption, "data_rich/all_base_models/data/current/ind_nss1112_food_consumption.csv")
# write_csv(eth_hices_food_consumption, "data_rich/all_base_models/data/current/eth_hices1516_food_consumption.csv")

rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################