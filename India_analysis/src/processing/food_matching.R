# Matching and harmonising NSSO food items
# MIMI project
# Author: Gabriel Battcock
# Date: 25/10/2023


# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("TomCodd/NutritionTools")

# We also need to import some custom functions in another script:
source(here::here("functions.R")) # Loading nutrition functions

# read in the NSSO food data
path_to_file <- "./India_analysis/data/raw/"
block_5_6_food_consumption <- read_csv(paste0(path_to_file, "block_5_6_food_consumption.csv"))
food_item_names <- readxl::read_xlsx(paste0(path_to_file, "food_codes_nsso_to_ifct.xlsx"), sheet = 1) %>% 
  dplyr::rename(Item_Code = `unique(Item_Code)`) %>% 
  dplyr::select(Item_Code,item_name)

# select only the HHID, item codes and quantities
block_5_6_food_consumption <- block_5_6_food_consumption %>% 
  dplyr::select(HHID, 
                Item_Code,
                Total_Consumption_Quantity, 
                Total_Consumption_Value) 
head(block_5_6_food_consumption)

# find any food items in odd units

food_item_names %>% 
  dplyr::mutate(
    food_unit = stringr::str_split(pattern = "(")
    )
