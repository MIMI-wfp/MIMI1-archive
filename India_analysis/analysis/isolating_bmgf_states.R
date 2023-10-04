#
# Gabriel Battcock
# MIMI Project
# Loading in NSSO 2010-12 data and isolating UP, Chat and Bihar before analysis

library(haven)
library(tidyverse)

# For Fabriel local only
path_to_file <- "~/Documents/MIMI/nsso_data/"
  

block_1_2_identification <- read_dta(paste0(path_to_file, "Identification of Sample Household - Block 1 and 2 - Level 1 -  68.dta"))
block_3_level_2_household_char <- read_dta(paste0(path_to_file, "Household Characteristics - Block 3 -  Level 2 -  68.dta"))
block_3_level_3_household_char <- read_dta(paste0(path_to_file, "Household characteristics - Block 3 - Level 3.dta"))
block_4_demog <-  read_dta(paste0(path_to_file, "Demographic and other particulars of household members - Block 4  - Level 4 - 68.dta"))
block_5_6_food_consumption <- read_dta(paste0(path_to_file, "Consumption of cereals-pulses- milk and milk products  during the last 30 days  - Block 5.1- 5.2- 6 - Level 5 - 68.dta"))
block_7_8_clothing_consumption <- read_dta(paste0(path_to_file, "Consumption of clothing, bedding and footwear during last 30 and 365 days - Block 7 and 8  - Level 6 -  68.dta"))
block_9_edu_expenditure <- read_dta(paste0(path_to_file,"Expenditure on Education and Medical (institutional) goods and services -  Block 9 - Level 7 -  68.dta"))
block_10_misc_expenditure <- read_dta(paste0(path_to_file,"Expenditure on miscellaneous goods and services including medical(non-institutional), rents and taxes during the last 30 days. Block 10 - Level 8 -68.dta"))
block_11_construction_expenditure <- read_dta(paste0(path_to_file, "Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use-  Block 11 - Level 9 -  68.dta"))
block_12_consumer_expenditure <- read_dta(paste0(path_to_file, "Summary of Consumer Expenditure - Block 12 - Level 11 - 68.dta"))
block_13_yoga_ayurveda <- read_dta(paste0(path_to_file, "Information on Ayurveda, Yoga, Naturopathy, Unani, Siddha, Homeopathy(ASYUSH) - Block 13 - Level 10 - 68.dta"))

# bihar = 10, UP = 09, chat = 22

dim(block_1_2_identification)
dim(block_3_level_3_household_char)
names(block_4_demog)

dim(block_4_demog)

isolate_states <- function(block){
  
  block <- block %>% 
    {if('State_Code'%in% names(.)) rename(., "State_code" = "State_Code") else .} %>% 
    filter(State_code == "09" |State_code == "10" | State_code == "22")
  block
}

block_1_2_identification <- isolate_states(block_1_2_identification)
block_3_level2_household_char <- isolate_states(block_3_level2_household_char)
block_4_demog <- isolate_states(block_4_demog)
block_5_6_food_consumption <- isolate_states(block_5_6_food_consumption)
block_7_8_clothing_consumption <- isolate_states(block_7_8_clothing_consumption)
block_9_edu_expenditure <- isolate_states(block_9_edu_expenditure)
block_10_misc_expenditure <- isolate_states(block_10_misc_expenditure)
block_11_construction_expenditure <- isolate_states(block_11_construction_expenditure)
block_12_consumer_expenditure <- isolate_states(block_12_consumer_expenditure)
block_13_yoga_ayurveda <- isolate_states(block_13_yoga_ayurveda)


unique(block_5_6_food_consumption$Item_Code)


path_to_save <- "~/Documents/MIMI/"

save(block_1_2_identification, "")

