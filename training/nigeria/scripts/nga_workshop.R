################################################################################
##################### DATA PREPARATION FOR NIGERIA WORKSHOP ####################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 28-Feb-2024
# Last edited: 01-Mar-2024


# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "readxl")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN REQUIRED DATA: 

household_locations <- read_csv("map_data/nga/new_shapefiles/household_locations.csv")
hh_info <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_hh_info.csv")
base_ai <- read_csv("data_rich/fortification_models/data/nga_lss1819_base_ai.csv")
vehicle_quantities <- read_csv("data_rich/fortification_models/data/nga_lss1819_vehicle_quantities.csv")
tot_consumption <- read_csv("survey_data/nga/totcons.csv")
roster <- read_csv("survey_data/nga/sect1_roster.csv")
food_groups <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_food_consumption.csv")
food_group_codes <- read_excel("training/nigeria/data/food_groups.xlsx")

#-------------------------------------------------------------------------------

# FOOD EXPENDITURE: 

# Extract value of consumption on food from tot_consumption data-frame:
food_consumption <- tot_consumption %>% 
  dplyr::select("hhid", starts_with("food_purch")) %>% 
  mutate(value = round(rowSums(across(starts_with("food_purch")), na.rm = T),
                                  digits = 2)) %>% 
  dplyr::select(hhid, value)

#-------------------------------------------------------------------------------

# ADD MICRONUTRIENT BASE CASE AI:

analysis_df <- base_ai %>% 
  dplyr::left_join(food_consumption, by = "hhid") %>%
  dplyr::select(hhid, value, energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg,
                vitb6_mg, folate_mcg, vitb12_mcg, vitc_mg, vitd_mcg, ca_mg, fe_mg,
                zn_mg)

# Remove objects that are not required further:
rm(list = c("base_ai", "food_consumption", "tot_consumption"))

#-------------------------------------------------------------------------------

# ADD HOUSEHOLD DEMOGRAPHIC INFORMATION: 

# Add state and local government area:
analysis_df <- analysis_df %>% 
  dplyr::left_join((household_locations %>%  dplyr::select(hhid, state, lga)), 
                   by = "hhid")

# Select the other required variables: 
hh_info <- hh_info %>% 
  dplyr::select(hhid, month, res, sex_head, age_head, educ_head, survey_wgt,
         sep_quintile, res_quintile)

# Create an additional variable for the number of children under the age of 5 in 
# the household: 
roster <- roster %>% dplyr::select(hhid, indiv, s01q04a) %>% 
  filter(s01q04a < 5) %>% 
  group_by(hhid) %>% 
  summarise(under_5 = n())

# Merge all these variables to the analaysis dataframe: 
analysis_df <- analysis_df %>% 
  dplyr::left_join(hh_info, by = "hhid") %>% 
  dplyr::left_join(roster, by = "hhid") %>% 
  # If there are na's in the under_5 column, replace with 0:
  mutate(under_5 = ifelse(is.na(under_5), 0, under_5))

# Remove objects that are not required further:
rm(list = c("hh_info", "roster", "household_locations"))

#-------------------------------------------------------------------------------

# ADD DATA ON CONSUMPTION OF FORTIFICATION VEHCILES:

analysis_df <- analysis_df %>% 
  dplyr::left_join((vehicle_quantities %>% 
                      dplyr::select(hhid, rice, rice_100g, wheatflour, 
                                    wheatflour_100g,maizeflour, maizeflour_100g, 
                                    edible_oil, edible_oil_100g, sugar, 
                                    sugar_100g)), 
                   by = "hhid")

rm(vehicle_quantities)

#-------------------------------------------------------------------------------

# ADD DATA ON FOOD GROUPS: 

food_group_codes <- food_group_codes %>% 
  rename("item_code" = "item_cd")

# Merge the food groups data with the food group codes:
food_groups <- food_groups %>% 
  dplyr::select(hhid, item_code) %>% 
  dplyr::left_join(food_group_codes, by = "item_code")

rm(food_group_codes)

food_groups <- food_groups %>% 
  dplyr::select(hhid, food_group)

# Remove duplicates: 
food_groups <- food_groups %>% 
  distinct()

# Create binary columns for food_groups, 1 if consumed, otherwise 0:
food_groups$values <- 1

food_groups_wide <- food_groups %>% 
  pivot_wider(names_from = "food_group", values_from = "values")

food_groups_wide[is.na(food_groups_wide)] <- 0

food_groups_wide$`NA` <- NULL

# Re-order columns: 
food_groups_wide <- food_groups_wide %>% 
  dplyr::select(hhid, cereals, roots_tubers, vegetable, fruits, meat_poultry_offal, eggs,
         fish_seafood, pulses_legumes_nuts, milk_products, oil_fats, sugar_honey,
         misc) %>% 
  rename(vegetables = vegetable)

# Join food groups data to analysis data-frame: 
analysis_df <- analysis_df %>% 
  dplyr::left_join(food_groups_wide, by = "hhid")

rm(list = c("food_groups", "food_groups_wide"))

#-------------------------------------------------------------------------------

# For rice, wheatflour, maizeflour, edible_oil and sugar variables, binarise so 
# that "Yes" == 1 amd "No" == 0:
analysis_df <- analysis_df %>% 
  mutate(rice = ifelse(rice == "Yes", 1, 0),
         wheatflour = ifelse(wheatflour == "Yes", 1, 0),
         maizeflour = ifelse(maizeflour == "Yes", 1, 0),
         edible_oil = ifelse(edible_oil == "Yes", 1, 0),
         sugar = ifelse(sugar == "Yes", 1, 0))

#-------------------------------------------------------------------------------

# Write csv: 
write_csv(analysis_df, "training/nigeria/data/nga_lss1819_analysis_df.csv")

################################################################################
################################ END OF SCRIPT #################################
################################################################################
