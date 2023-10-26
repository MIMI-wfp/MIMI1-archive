# Matching and harmonising NSSO food items
# MIMI project
# Author: Gabriel Battcock
# Date: 25/10/2023


# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("TomCodd/NutritionTools"

# We also need to import some custom functions in another script:
# source(here::here("functions.R")) # Loading nutrition functions

# read in the NSSO food data
path_to_file <- "./India_analysis/data/raw/"
block_5_6_food_consumption <- read_csv(paste0(path_to_file, "block_5_6_food_consumption.csv"))
#read in xlsx spreadsheet previously matched by hand
food_item_names <- readxl::read_xlsx(paste0(path_to_file, "food_codes_nsso_to_ifct.xlsx"), sheet = 1) %>% 
  dplyr::rename(Item_Code = `unique(Item_Code)`) %>% 
  dplyr::select(Item_Code,item_name, IFCT_code, ifct_name)

# select only the HHID, item codes and quantities
block_5_6_food_consumption <- block_5_6_food_consumption %>% 
  dplyr::select(HHID, 
                Item_Code,
                Total_Consumption_Quantity, 
                Total_Consumption_Value) 
head(block_5_6_food_consumption)

# find any food items in odd units

food_item_names <- food_item_names %>% 
  dplyr::mutate(
    food_unit = stringr::str_split_i(
      stringr::str_split_i(item_name,"\\(", -1), "\\)",1)
    ) %>% 
  dplyr::mutate(
    conversion_factor = ifelse(stringr::str_detect(food_unit,"Kg|kg") == TRUE, 1,
                         ifelse(stringr::str_detect(food_unit,"\\gm")== TRUE, 0.001, 
                                NA))
                                # ifelse(stringr::str_detect(item_name))))
    ) %>% 
  dplyr::filter(
    !(Item_Code>=325 |
      (Item_Code >= 300 & Item_Code <=320)) |#filter out tobacco and fuels
      stringr::str_detect(item_name, "s.t") == FALSE
    )


#convert the non-standard units

non_standard_units <- food_item_names %>% 
  dplyr::filter(is.na(conversion_factor)) %>% 
  dplyr::mutate(
    conversion_factor = 
      dplyr::case_when(
        #by inspection w/o units listed
        Item_Code == 170 ~ 1, #salt, appears to be alerady in kg
        Item_Code == 171 ~ 1, #sugar same as salt
        Item_Code == 172 ~ 1, #sugar same as salt
        Item_Code == 173 ~ 1, #gur
        Item_Code == 174 ~ 1, #candy
        Item_Code == 175 ~ 1, #Honey
        #items with units of no. 
        Item_Code == 190 ~ 0.053,#eggs, USDA match
        Item_Code == 216 ~ 0.054,#lemon, USDA match
        Item_Code == 220 ~ 0.115,#Banana, USDA
        Item_Code == 228 ~ 0.14,#Orange, USDA
        Item_Code == 223 ~ 0.905,#Pineapple, USDA
        Item_Code == 160 ~ 1.05#cow's milk, USDA
      )
  )



block_5_6_food_consumption %>% dplyr::select(HHID,Item_Code, Total_Consumption_Quantity) %>% 
  dplyr::filter(Item_Code == 175)
  
block_4_demog %>% dplyr::select(HHID,Person_sr_no) %>% 
  dplyr::filter(HHID == 741431101) 


# head(non_standard_units)
  #   


# 'Other' items

other_items <- food_item_names %>% 
  dplyr::filter(
    # stringr::str_detect(item_name, "other|Other")==TRUE &
      is.na(IFCT_code)
  )

