################################################################################
######### SCRIPT FOR CREATING BASE MODELS USING ALTERNATIVE NIGERIA FCT ########
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 22-03-2024
# Last edited: 

# In this script, I will create alternative base apparent intake models for Nigeria
# using the alternative food composition matches provided by Katie Adams (UC Davis).

# I will also perform a comparison of these models, compared against our original 
# base models using food composition matches from KT. 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "stringr", "readxl")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

#############################################
## PART 1 - CREATE ALTERNATIVE BASE MODELS ##
#############################################

# READ IN DATA

# FCT: 
fc_table <- read_csv("data_rich/all_base_models/data/KA_nga_lss1819_fct/KAnga_lss1819_fct.csv")

# Nigeria food consumption: 
food_consumption <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_food_consumption.csv")
  
# Household demographic info: 
hh_info <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_hh_info.csv")

# Join zone data to hh_info, this is required for differences in milk nutritional
# value according to zone: 
cover <- read_csv("survey_data/nga/secta_cover.csv")

hh_info <- hh_info %>% 
  dplyr::left_join(cover %>% dplyr::select(hhid, zone), by = "hhid")

rm(cover)

#-------------------------------------------------------------------------------

# APPARENT INTAKE BASE MODEL

# Need to compute apparent intake from milk separately as this is dependent on 
# household "zone".

# Create a milk food composition table:
milk_fct <- fc_table %>% 
  filter(item_code == 110) %>% 
  filter(!is.na(zone))

# Apparent nutrient intake from milk: 
KAmilk_ai <- food_consumption %>% 
  filter(item_code == 110) %>% # Filter to include only fresh milk
  left_join(hh_info %>% select(hhid, zone), 
            by = "hhid") %>%
  left_join(milk_fct, 
            by = c("item_code", "zone")) %>% # Join milk food composition table by item_code and zone
  mutate(across(-c(item_code, hhid, item_name, food_group, quantity_100g, 
                   quantity_g, zone),
      ~.x*quantity_100g)) %>% # Multiply nutrient values by quantity consumed
  group_by(hhid) %>% # Aggregate by household id summing the values of consumption
  summarise(across(-c(item_code, item_name, quantity_100g, quantity_g, 
                      food_group, zone),
           ~sum(., na.rm = T))) %>% 
  left_join(hh_info %>% select(hhid, afe), by = "hhid") %>% # Join afe
  mutate(across(-c(hhid, afe),~.x/afe)) %>% # Divide all nutrient values by afe
  ungroup()

# Remove milk from the main food composition table: 
fc_table <- fc_table %>% 
  filter(item_code != 110) %>% 
  dplyr::select(-zone)

# Apparent nutrient intake from all other foods:
KAbase_ai <- food_consumption %>% 
  filter(item_code != 110) %>%
  left_join(fc_table, by = "item_code") %>% 
  mutate(
    across(
      -c(item_code, hhid,item_name ,food_group, quantity_100g, quantity_g),
      ~.x*quantity_100g
    )
  ) %>% 
  group_by(hhid) %>% 
  summarise(
    across(-c(item_code,item_name,quantity_100g,quantity_g, food_group),
           ~sum(.,na.rm = T))
  ) %>% 
  left_join(hh_info %>% select(hhid, afe), by = "hhid") %>% 
  mutate(
    across(
      -c(hhid,afe),
      ~.x/afe
    )
  ) %>% 
  ungroup()

# Combine nutrient intake from the 2 apparent intake data-frames: 
KAbase_ai <- bind_rows(KAbase_ai, KAmilk_ai) %>% 
  group_by(hhid) %>%
  summarise(across(everything(), ~sum(., na.rm = T))) %>%
  ungroup() %>% 
  # One of the AFE's has been lost as an NA, therefore re-join hh_info:
  dplyr::select(-afe) %>% 
  left_join(hh_info %>% select(hhid, afe), by = "hhid")

#-------------------------------------------------------------------------------

# WRITE DATA: 
# write_csv(KAbase_ai, "data_rich/all_base_models/data/KA_nga_lss1819_fct/KAnga_lss1819_base_ai.csv")

# Remove objects that are not required further: 
rm(list = ls()[!ls() %in% "KAbase_ai"])

#-------------------------------------------------------------------------------

########################################
## PART 2 - COMPARISON OF BASE MODELS ##
########################################

# Read in original base model: 
base_ai <- read_csv("data_rich/fortification_models/data/nga_lss1819_base_ai.csv")

# Quick summary: 
summary(base_ai)
summary(KAbase_ai)
# Summary statistics almost identical for all MN's except Vitamin A

# Calculate the mean of each micronutrient for base_ai and KAbase_ai - list values in a table: 
intake_basemodel <- base_ai %>% 
  summarise(across(-c(hhid, afe), ~mean(., na.rm = T))) %>% 
  pivot_longer(cols = everything(), names_to = "nutrient", values_to = "value")

intake_KAmodel <- KAbase_ai %>%
  summarise(across(-c(hhid, afe), ~mean(., na.rm = T))) %>% 
  pivot_longer(cols = everything(), names_to = "nutrient", values_to = "value")

compare_models <- intake_basemodel %>% 
  left_join(intake_KAmodel, by = "nutrient", suffix = c("_base", "_KA")) %>% 
  mutate(diff = value_KA - value_base) %>% 
  dplyr::select(nutrient, value_base, value_KA, diff) %>% 
  # Round diff to 5 dp: 
  mutate(diff = round(diff, 5))

rm(list = c("intake_basemodel", "intake_KAmodel"))

# Create simple boxplot to compare vita_rae_mcg from each model: 
boxplot(base_ai$vita_rae_mcg, 
        KAbase_ai$vita_rae_mcg, 
        outline = F,
        main = "vita_rae_mcg", 
        ylab = "mcg per day/AFE",
        names = c("Base", "KA"))

# Now compare all micronutrients in a single plot using facet-wrapped box plots: 
base_ai <- base_ai %>% 
  mutate(model = "Base")

KAbase_ai <- KAbase_ai %>%
  mutate(model = "KA")

bind_rows(base_ai, KAbase_ai) %>%
  pivot_longer(cols = -c(hhid, afe, model), names_to = "nutrient", values_to = "value") %>% 
  ggplot(aes(x = model, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~nutrient, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Model", 
       y = "Intake per day/AFE")

# Read in food consumption table for comparison: 
KA_fct <- read_csv("data_rich/all_base_models/data/KA_nga_lss1819_fct/KAnga_lss1819_fct.csv") %>% 
  dplyr::select(item_code, vita_rae_mcg) %>% 
  distinct()

base_fct <- read_csv("data_rich/all_base_models/data/current/nga_lss1819_fct.csv") %>% 
  dplyr::select(item_code, vita_rae_mcg)

# Compare fct's
comparison_fct <- base_fct %>% 
  left_join(KA_fct, by = "item_code", suffix = c("_base", "_KA")) %>% 
  filter(vita_rae_mcg_base != vita_rae_mcg_KA)

rm(list = c("base_fct", "KA_fct"))

# The main difference lies with item_code 50 (Palm oil). 0mcg per 100g in original
# base model, and 572mcg per 100g in the KA model.

# Clear environment:
rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################



