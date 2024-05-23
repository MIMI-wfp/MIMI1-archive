#
# Gabriel Battcock
# MIMI Project
# Loading in NSSO 2010-12 data and isolating UP, Chat and Bihar before analysis



library(haven)
library(tidyverse)

# For Gabriel local only
path_to_file <- "~/Documents/MIMI/MIMI_data/India/raw_dta_nsso1112/"
  

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

# bihar = 10, UP = 09, chat = 22, odisha = 21, mp = , jk = 20, wb = 19, , hp = 02 ap = 28

dim(block_1_2_identification)
dim(block_3_level_3_household_char)
names(block_4_demog)

dim(block_4_demog)


# isoalting only the states we want to use for analysis
# 
# isolate_states <- function(block){
# 
#   block <- block %>%
#     {if('State_Code'%in% names(.)) rename(., "State_code" = "State_Code") else .} %>%
#     filter(State_code == "09" |State_code == "10" | State_code == "22")
#   # | State_code == "21" )
#              # State_code == "19" |State_code == "20" | State_code == "23" |State_code == "28" | State_code == "02")
#   block
# }
# 
# block_1_2_identification <- isolate_states(block_1_2_identification)
# block_3_level_2_household_char <- isolate_states(block_3_level_2_household_char)
# block_4_demog <- isolate_states(block_4_demog)
# block_5_6_food_consumption <- isolate_states(block_5_6_food_consumption)
# block_7_8_clothing_consumption <- isolate_states(block_7_8_clothing_consumption)
# block_9_edu_expenditure <- isolate_states(block_9_edu_expenditure)
# block_10_misc_expenditure <- isolate_states(block_10_misc_expenditure)
# block_11_construction_expenditure <- isolate_states(block_11_construction_expenditure)
# block_12_consumer_expenditure <- isolate_states(block_12_consumer_expenditure)
# block_13_yoga_ayurveda <- isolate_states(block_13_yoga_ayurveda)

# 
# unique(block_5_6_food_consumption$Item_Code)
# 
# #save isolated states for analysis

# path_to_save <- "~/Documents/MIMI/code/India_analysis/data/raw/"
# path_to_save <- "~/Documents/MIMI/code/data_rich/India/data/raw/extra_states/"
# 
# write_csv(block_1_2_identification, paste0(path_to_save, "block_1_2_identification.csv"))
# write_csv(block_3_level_2_household_char,paste0(path_to_save, "block_3_level_2_household_char.csv"))
# write_csv(block_4_demog,paste0(path_to_save, "block_4_demog.csv"))
# write_csv(block_5_6_food_consumption,paste0(path_to_save, "block_5_6_food_consumption.csv"))
# write_csv(block_7_8_clothing_consumption,paste0(path_to_save, "block_7_8_clothing_consumption.csv"))
# write_csv(block_9_edu_expenditure, paste0(path_to_save, "block_9_edu_expenditure.csv"))
# write_csv(block_10_misc_expenditure, paste0(path_to_save, "block_10_misc_expenditure.csv"))
# write_csv(block_11_construction_expenditure, paste0(path_to_save, "block_11_construction_expenditure.csv"))
# write_csv(block_12_consumer_expenditure, paste0(path_to_save, "block_12_consumer_expenditure.csv"))
# write_csv(block_13_yoga_ayurveda, paste0(path_to_save, "block_13_yoga_ayurveda.csv"))


path_to_save2 <- "~/Documents/MIMI/data_science_code/DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012/"

write_csv(block_1_2_identification,paste0(path_to_save2, "Identification of Sample Household - Block 1 and 2 - Level 1 -  68.csv"))
write_csv(block_3_level_2_household_char,paste0(path_to_save2, "Household Characteristics - Block 3 -  Level 2 -  68.csv"))
write_csv(block_3_level_3_household_char,paste0(path_to_save2, "Household characteristics - Block 3 - Level 3.csv"))
write_csv(block_4_demog,paste0(path_to_save2, "Demographic and other particulars of household members - Block 4  - Level 4 - 68.csv"))
write_csv(block_5_6_food_consumption,paste0(path_to_save2, "Consumption of cereals-pulses- milk and milk products  during the last 30 days  - Block 5.1- 5.2- 6 - Level 5 - 68.csv"))
write_csv(block_7_8_clothing_consumption,paste0(path_to_save2, "Consumption of clothing, bedding and footwear during last 30 and 365 days - Block 7 and 8  - Level 6 -  68.csv"))
write_csv(block_9_edu_expenditure,paste0(path_to_save2,"Expenditure on Education and Medical (institutional) goods and services -  Block 9 - Level 7 -  68.csv"))
write_csv(block_10_misc_expenditure,paste0(path_to_save2,"Expenditure on miscellaneous goods and services including medical(non-institutional), rents and taxes during the last 30 days. Block 10 - Level 8 -68.csv"))
write_csv(block_11_construction_expenditure,paste0(path_to_save2, "Expenditure for purchase and construction (including repair and maintenance) of durable goods for domestic use-  Block 11 - Level 9 -  68.csv"))
write_csv(block_12_consumer_expenditure,paste0(path_to_save2, "Summary of Consumer Expenditure - Block 12 - Level 11 - 68.csv"))
write_csv(block_13_yoga_ayurveda,paste0(path_to_save2, "Information on Ayurveda, Yoga, Naturopathy, Unani, Siddha, Homeopathy(ASYUSH) - Block 13 - Level 10 - 68.csv"))

rm(list = ls())
