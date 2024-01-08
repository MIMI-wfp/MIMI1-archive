## functions to compile base models

## read in the food consumption and fct and create data frame of 
## each food item 

library(ggplot2)

path_to_file <- here::here("all_base_models/data/current-20240105/")


read_in_survey <- function(name_of_survey){
  # given the name of the survey of country
  # the function reads in each part of the base model into general 
  # object names
  
  afe <<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_afe.csv")))
  food_consumption<<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_food_consumption.csv")))
  fct <<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_fct.csv")))
  # fct causes conflict with fct() function in forcats package, reconsider the name of this object
}


full_item_list <- function(name_of_survey){
  # creates a data frame with a full list of food items for every
  # household. If food item is not consumed, quantity = 0
  # uesful for food group analyses
  

  afe <- read.csv(paste0(path_to_file, paste0(name_of_survey, "_afe.csv")))
  food_consumption<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_food_consumption.csv")))
  fct <- read.csv(paste0(path_to_file, paste0(name_of_survey, "_fct.csv")))
  
  x <- afe %>% 
    select(hhid,afe) %>% 
    cross_join(fct %>% 
                 select(item_code)) %>% 
    left_join(food_consumption, 
              by = c("hhid", "item_code")) %>% 
    select(-food_group) %>% 
    mutate(
      across(
        c(quantity_100g, quantity_g),
        ~replace_na(.,0)
      )
    ) %>% 
    mutate(
      quantity_100g = quantity_100g/afe, 
      quantity_g = quantity_g/afe
    ) %>% 
    left_join(fct, by = "item_code")
  x
}

# afe <- as_tibble(read.csv(paste0(path_to_file, paste0("nga1819", "_afe.csv"))))
# food_consumption<- as_tibble(read.csv(paste0(path_to_file, paste0("nga1819", "_food_consumption.csv"))))
# fct <- as_tibble(read.csv(paste0(path_to_file, paste0("nga1819", "_fct.csv"))))


# full_item_list("nga1819")

apparent_intake <- function(name_of_survey){
  # Estimates apparent intake of nutrients based on consumed food items
  # and adult female equivalent unit of the household
  read_in_survey(name_of_survey)
  
  x <- food_consumption %>% 
    left_join(fct, by = "item_code") %>% 
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
    left_join(afe, by = "hhid") %>% 
    mutate(
      across(
        -c(hhid,afe),
        ~.x/afe
      )
    ) %>% 
    ungroup()
  x
}


household_data <- function(name_of_survey){
  #reads in the household information data
  x <- read.csv(paste0(path_to_file, paste0(name_of_survey, "_hh_info.csv")))
  x
}


nutrient_density <- function(name_of_survey){
  # returns a data frame of nutrient density for each household
  # values are given in unit of mn per 1000kcal 
  #
  x <- food_consumption %>% 
    left_join(fct, by = "item_code") %>% 
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
    mutate(energy_1000kcal = energy_kcal/1000) %>% 
    mutate(
      across(
        -c(hhid),
        ~.x/energy_1000kcal,
        .names ="{.col}_1000kcal"
      )
    ) %>% 
    select(hhid, ends_with("1000kcal")) %>% 
    select(-energy_kcal_1000kcal)
  x
}



