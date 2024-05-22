#########################################
#        India base model               #
#        Data validation               #
#########################################

# Author: Gabriel Battcock
# Created: 10 Nov 23
# Last updated: 8 May 24

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2",
                 "ggridges", "gt", "haven","foreign",'here',
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify",'sjmisc')

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

# source(here::here("data_rich/India/src/processing/food_matching.R"))

# # read in cleaned and mathced data
# path_to_data = here::here("data_rich", "India", "data", "processed/")

path_to_data = here::here("data_rich","India", "data", "processed", "extra_states/")

consumption <- read_csv(paste0(path_to_data, "consumption.csv"))

# unique(consumption$State_code)
demographics <- read_csv(paste0(path_to_data,"demographics.csv"))

household_characteristics <- read_csv(paste0(path_to_data, "household_char.csv"))

india_fct <- read_csv(here::here("data_rich","India", "data", "processed", "matched_fct.csv"))
conversion <- read_csv(here::here("data_rich","India", "data", "processed", "conversion_factors.csv"))
hdds <- read_csv(here::here("data_rich", "India", "data", "raw","hdds_nsso.csv"))

sum_or_function <- function(x){
  #### TO DO ## explain the function
  
  #creates OR logic gate
  y = sum(x, na.rm = TRUE)
  y = as.numeric(ifelse(y != 0, 1, 0))
  y
}

# View(household_characteristics)

# working data frame of matched food items to composition
# can now aggregate by HH or by individual item to test and validate
# distributions of key food items and micronutrients


#### Calculate daily consumption per household -----------------------------

daily_food_items_consumed <- consumption %>% 
  dplyr::left_join(
    india_fct, by = c("Item_Code" = "item_code")
  ) %>% 
  dplyr::left_join(
    conversion, by = c("Item_Code", "item_name")
  ) %>% 
  #split the name without the unit of consumption
  dplyr::mutate(
    item_name = stringr::str_split_i(item_name,"\\(", 1)
  ) %>% 
  dplyr::mutate(
    Total_Consumption_Quantity = ifelse(is.na(conversion_factor),
                                        Total_Consumption_Quantity,
                                        Total_Consumption_Quantity*conversion_factor)
  ) %>% 
  dplyr::left_join(
    household_characteristics %>% dplyr::select(HHID, District_code),
    by = "HHID"
  ) %>% 
  dplyr::mutate(
    Total_Consumption_Quantity =
      dplyr::case_when(
        Item_Code == 166 ~ Total_Consumption_Value*80,#https://www.calories.info/food/ice-cream
        Item_Code == 238 ~ Total_Consumption_Value*50,
        Item_Code == 277 ~ Total_Consumption_Value*100,
        Item_Code == 283 ~ Total_Consumption_Value*71, #https://www.eatthismuch.com/food/nutrition/samosa,2144079/,
        Item_Code == 284 ~ Total_Consumption_Value*132, #https://www.eatthismuch.com/food/nutrition/veg-biryani,2502955/
        Item_Code == 290 ~ Total_Consumption_Value*63, # https://www.fatsecret.co.uk/calories-nutrition/generic/ladoo-round-ball-(asian-indian-dessert)?frc=True
        Item_Code == 291 ~ Total_Consumption_Value*12, # https://www.nutritionix.com/i/usda/cookies-chocolate-chip-commercially-prepared-soft-type-1-cookie-average-weight-of-1-cookie/513fceb775b8dbbc21002745
        Item_Code == 296 ~ Total_Consumption_Value*50
      )
  ) %>% 
  #create state name
  dplyr::mutate(
    quantity_100g = Total_Consumption_Quantity/100
    # State_name = dplyr::case_when(
    #   State_code == "09" ~ "Uttar Pradesh", 
    #   State_code == "10" ~ "Bihar",
    #   State_code == "22" ~ "Chhattisgarh",
    #   State_code == "21" ~ "Orissa"
    # )
  ) %>% 
  dplyr::select(
    -c( Home_Produce_Quantity,Home_Produce_Value,Total_Consumption_Quantity,Total_Consumption_Value)
  ) %>% 
  dplyr::mutate(
    dplyr::across(
      -c(item_name, Item_Code, State_Code, District_code, HHID, quantity_100g, conversion_factor),
      ~.x*quantity_100g/30
    )
  ) %>% 
  dplyr::mutate(
    quantity_100g = quantity_100g/30
  )  %>%
  dplyr::filter(
    !is.na(item_name)
  )



## write a csv for household daily consumption, pre separation by afe
 
household_daily <- consumption %>%
  dplyr::left_join(
    india_fct, by = c("Item_Code" = "item_code")
  ) %>%
  dplyr::left_join(
    conversion, by = c("Item_Code", "item_name")
  ) %>%
  #split the name without the unit of consumption
  dplyr::mutate(
    item_name = stringr::str_split_i(item_name,"\\(", 1)
  ) %>%
  dplyr::mutate(
    Total_Consumption_Quantity = ifelse(is.na(conversion_factor),
                                        Total_Consumption_Quantity,
                                        Total_Consumption_Quantity*conversion_factor)
  ) %>%
  dplyr::mutate(
    Total_Consumption_Quantity =
      dplyr::case_when(
        Item_Code == 166 ~ Total_Consumption_Value*80,#https://www.calories.info/food/ice-cream
        Item_Code == 238 ~ Total_Consumption_Value*50,
        Item_Code == 277 ~ Total_Consumption_Value*100,
        Item_Code == 283 ~ Total_Consumption_Value*71, #https://www.eatthismuch.com/food/nutrition/samosa,2144079/,
        Item_Code == 284 ~ Total_Consumption_Value*132, #https://www.eatthismuch.com/food/nutrition/veg-biryani,2502955/
        Item_Code == 290 ~ Total_Consumption_Value*63, # https://www.fatsecret.co.uk/calories-nutrition/generic/ladoo-round-ball-(asian-indian-dessert)?frc=True
        Item_Code == 291 ~ Total_Consumption_Value*12, # https://www.nutritionix.com/i/usda/cookies-chocolate-chip-commercially-prepared-soft-type-1-cookie-average-weight-of-1-cookie/513fceb775b8dbbc21002745
        Item_Code == 296 ~ Total_Consumption_Value*50, 
        .default = Total_Consumption_Quantity
      )
  ) %>% 
  dplyr::left_join(
    household_characteristics %>% dplyr::select(HHID, District_code),
    by = "HHID"
  ) %>%
  #create state name
  dplyr::mutate(
    quantity_100g = Total_Consumption_Quantity/100
    # State_name = dplyr::case_when(
    #   State_code == "09" ~ "Uttar Pradesh",
    #   State_code == "10" ~ "Bihar",
    #   State_code == "22" ~ "Chhattisgarh",
    #   State_code == "21" ~ "Orissa",
    #   State_code == "20" ~ "Jharkhand",
    #   State_code == "23" ~ "Madhya Pradesh",
    #   State_code == "02" ~ "Himachal Pradesh",
    #   State_code == "19" ~ "West Bengal",
    #   State_code == "28" ~ "Andhra Pradesh"
      # )
  ) %>%
  ungroup() %>%
  # dplyr::select(
  #   -c( Home_Produce_Quantity,Home_Produce_Value,Total_Consumption_Quantity,Total_Consumption_Value)
  # ) %>%
  dplyr::mutate(
    #### NOTE
    #### The india data has 30 day recall for some items and 7 day recall for others. 
    #### item code 101 - 179 (cereals, pulses, dairy, salt and sugar) 30 days
    #### item code 180+ 7 day recall 
    
    
    across( c(Total_Consumption_Quantity,Total_Consumption_Value),
    # ~ ifelse(Item_Code<180, .x/30,.x/7)
    ~.x/30
    )) %>%
  dplyr::select(HHID,State_Code,Item_Code,item_name,Total_Consumption_Quantity,Total_Consumption_Value) %>%
  dplyr::filter(
    !is.na(item_name)
  ) %>%
  dplyr::left_join(hdds %>% dplyr::select(-item_name), by = c("Item_Code")) %>%
  dplyr::group_by(Item_Code, item_name) %>%
  tidyr::pivot_longer(cols = c(A_cereals,B_roots_tubers,C_vegetables,
                               D_fruits,E_meat,F_eggs,G_fish,H_pulses,
                               I_milk,J_oil,J_sugar,L_misc)#HDDS food groups
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::select(-value) %>%
  dplyr::rename(hdds_groups = name)


write.csv(household_daily,paste0(path_to_data, "india_daily_consumption.csv"))
# path_to_new_data = "data_rich/India/data/data_052024_7day/"
# write.csv(household_daily,paste0(path_to_new_data, "india_daily_consumption.csv"))




#remove large outliers based on energy
filter_households <- daily_food_items_consumed %>% 
  group_by(HHID) %>% 
  summarise(total_energy_hh = sum(energy_kcal,na.rm = T)) %>% 
  ungroup() %>% 
  filter(total_energy_hh<stats::quantile(total_energy_hh, 0.99, na.rm = TRUE)[[1]]) 

daily_food_items_consumed <- filter_households %>% 
  select(HHID) %>% 
  left_join(daily_food_items_consumed, by = "HHID")

### check the individual(household) intake per month of rice and wheat



#### Calculate adult female equivalent per hh -----------------------------------


##### Assumptions and conditions ##############
# Using the NIN_ICMR energy requirement for Indians
# Requirements for Infants (under 1yo) have been averaged as there is no granularity
# in months.
# All adults are assumed to have moderate energy expenditure
# Men are assumed to have 65 kg and women 55 kg


children_under_2 <- demographics %>% 
  dplyr::group_by(HHID) %>% 
  dplyr::summarise(
    under_2 = factor(ifelse(
      sum(Age < 2) >= 1,
      1,
      0
    )
    )
  )

summary(children_under_2)

adult_female_requirement <- 2130

demographics <- demographics %>%
  dplyr::left_join(children_under_2, by = "HHID") %>% 
  dplyr::mutate(
    energy_requirement = 
      dplyr::case_when(
        Age < 1 ~ 0,
        Age < 4 ~ 1070,
        Age < 7 ~ 1360,
        Age < 10 ~ 1700,
        Age < 13 ~ ifelse(Sex == "Male", 2220, 2060),
        Age < 16 ~ ifelse(Sex == "Male", 2860, 2400),
        Age < 18 ~ ifelse(Sex == "Male", 2860, 2400),
        Age >= 18 ~ ifelse(Sex == "Male", 2710,
                           ifelse(Age<50, 2130,
                            ifelse(under_2 == 0, 
                                  2130,
                                  2690)))
      ) 
  ) %>% 
  dplyr::mutate(
    afe = round(energy_requirement/adult_female_requirement,
                2)
  )

# for the households

household_afe <- 
  demographics %>% 
    dplyr::group_by(
      HHID
    ) %>% 
    dplyr::summarise(
      capita = dplyr::n(),
      afe = sum(afe)
    )

 write.csv(household_afe, paste0(path_to_data, "india_afe.csv"))

# %>% 
  # left_join(household_characteristics %>% select(HHID,HH_Size), by = "HHID")

 
 


