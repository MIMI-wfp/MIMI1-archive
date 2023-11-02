### Validating the intake data

library(tidyr)
library(readr)
library(sf)
library(rmapshaper)
library(readxl)
library(here)
library(ggplot2)
library(hrbrthemes)

source("India_analysis/src/processing/food_matching.R")

# read in cleaned and mathced data
path_to_data = here::here("India_analysis", "data", "processed/")

consumption <- read_csv(paste0(path_to_data, "consumption.csv"))
demographics <- read_csv(paste0(path_to_data,"demographics.csv"))
household_characteristics <- read_csv(paste0(path_to_data, "household_char.csv"))
india_fct <- read_csv(paste0(path_to_data, "matched_fct.csv"))

View(household_characteristics)

# working data frame of matched food items to composition
# can now aggregate by HH or by individual item to test and validate
# distributions of key food items and micronutrients

daily_food_items_consumed <- consumption %>% 
  dplyr::left_join(
    india_fct, by = "Item_Code"
  ) %>% 
  dplyr::mutate(
    quantity_100g = Total_Consumption_Quantity/100
  ) %>% 
  dplyr::select(
    -c( Home_Produce_Quantity,Home_Produce_Value,Total_Consumption_Quantity,Total_Consumption_Value)
  ) %>% 
  dplyr::filter(
    !is.na(item_name) &
      !is.na(quantity_100g)
  ) %>% 
  dplyr::mutate(
    dplyr::across(
      -c(item_name, Item_Code, State_code, HHID),
      ~.x*quantity_100g/30
    )
  )

#### Distributions #############################################################

per_capita_consumption <- daily_food_items_consumed %>% 
  dplyr::group_by(HHID, State_code) %>% 
  dplyr::select(-c(Item_Code, item_name)) %>% 
  dplyr::summarise(
    dplyr::across(
      everything(),
      ~sum(.x, na.rm = TRUE)
    )
  ) %>% 
  #calculate the per capita consumption to play with the data
  dplyr::full_join(
    demographics %>% 
      dplyr::group_by(HHID) %>% 
      dplyr::summarise(capita = dplyr::n()) %>% 
      dplyr::select(HHID, capita), by = c("HHID")
  ) %>% 
  dplyr::mutate(
    dplyr::across(
      -c(State_code),
      ~.x/capita
    )) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    energy_kcal<stats::quantile(energy_kcal, 0.99, na.rm = TRUE)[[1]]
  )

quantile(per_capita_consumption$energy_kcal, 0.99, na.rm = TRUE)[[1]]

#look at the distributions of:
#     # Vit A, B1 2 3 5 6 9 12 Fe Zn Ca kcal
micronutrients <- c("energy_kcal", "vita_mg", "vitb1_mg", "vitb2_mg", "vitb3_mg", "vitb5_mg",
                    "vitb6_mg", "vitb9_ug","vitaminb12_in_mg", "iron_mg", "calcium_mg", "zinc_mg")

for(item in micronutrients){
  per_capita_consumption %>% 
    ggplot(aes(x = vitaminb12_in_mg, fill = factor(State_code))) + 
    geom_histogram() + 
    # xlim(-0.01,5)+
    theme_ipsum()
}

