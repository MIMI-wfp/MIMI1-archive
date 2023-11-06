### Validating the intake data

library(tidyr)
library(readr)
library(sf)
library(rmapshaper)
library(readxl)
library(here)
library(ggplot2)
library(hrbrthemes)
library(wesanderson)
library(srvyr)

source("India_analysis/src/processing/food_matching.R")

# read in cleaned and mathced data
path_to_data = here::here("India_analysis", "data", "processed/")

consumption <- read_csv(paste0(path_to_data, "consumption.csv"))
demographics <- read_csv(paste0(path_to_data,"demographics.csv"))
household_characteristics <- read_csv(paste0(path_to_data, "household_char.csv"))

india_fct <- read_csv(paste0(path_to_data, "matched_fct.csv"))
conversion <- read_csv(paste0(path_to_data, "conversion_factors.csv"))
hdds <- read_csv(here::here("India_analysis", "data", "raw","hdds_nsso.csv"))

# View(household_characteristics)

# working data frame of matched food items to composition
# can now aggregate by HH or by individual item to test and validate
# distributions of key food items and micronutrients

daily_food_items_consumed <- consumption %>% 
  dplyr::left_join(
    india_fct, by = "Item_Code"
  ) %>% 
  dplyr::left_join(
    conversion, by = c("Item_Code", "item_name")
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
      -c(item_name, Item_Code, State_code, District_code, HHID, quantity_100g),
      ~.x*quantity_100g/30
    )
  ) %>% 
  dplyr::mutate(
    quantity_100g = quantity_100g/30
  )



#### Distributions #############################################################

##### Food item distributions --------------------------------------------------

total_households <- dplyr::n_distinct(daily_food_items_consumed$HHID)

food_items_grouped <- daily_food_items_consumed %>% 
  dplyr::full_join(
    demographics %>% 
      dplyr::group_by(HHID) %>% 
      dplyr::summarise(capita = dplyr::n()) %>% 
      dplyr::select(HHID, capita), by = c("HHID")
  ) %>%
  dplyr::mutate(
    dplyr::across(
      -c( HHID, State_code,Item_Code,item_name, District_code),
      ~.x/capita
    )) %>% 
  dplyr::group_by(
    Item_Code,
    item_name,
  ) %>% 
  dplyr::mutate(quantity_g = quantity_100g*100) 



# food_items_grouped %>% 
#   dplyr::ungroup() %>% 
#   dplyr::filter(Item_Code == 252) %>% 
#   dplyr::filter(quantity_g<stats::quantile(quantity_g, 0.99, na.rm = TRUE)[[1]]) %>% 
#   ggplot(aes(x = quantity_g)) +
#   geom_histogram()


# summary of food items consumed
food_item_summary <- food_items_grouped %>% 
  dplyr::left_join(household_characteristics %>% 
                     dplyr::select(HHID, Combined_multiplier), by = "HHID") %>% 
  srvyr::as_survey_design(id = HHID, strata =NULL, weights = Combined_multiplier, nest=T) %>% 
  srvyr::group_by(Item_Code, item_name) %>% 
  srvyr::summarise(mean_g = mean(quantity_g),
                   low_95 = mean(quantity_g)-1.96*sd(quantity_g)/sqrt(dplyr::n()),
                   up_95 = mean(quantity_g)+1.96*sd(quantity_g)/sqrt(dplyr::n()),
                   n_hh_consumed = dplyr::n(),
                   perc_hh_consumed = dplyr::n()/total_households*100
                   )
  
# splitting by food group, look at the individual distributions

x <- food_items_grouped %>% 
  dplyr::left_join(hdds, by = c("Item_Code", "item_name")) %>% 
  tidyr::pivot_longer(cols = c(A_cereals,B_roots_tubers,C_vegetables,
                               D_fruits,E_meat,F_eggs,G_fish,H_pulses,
                               I_milk,J_oil,J_sugar,L_misc),
  )
  




##### Micronutrients per capita -------------------------------------------------
per_capita_consumption <- daily_food_items_consumed %>% 
  dplyr::group_by(HHID, State_code, District_code) %>% 
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
      -c( "District_code"),
      ~.x/capita
    )) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    energy_kcal<stats::quantile(energy_kcal, 0.99, na.rm = TRUE)[[1]]
  ) %>% 
  dplyr::select(
    c("HHID","State_code","District_code", "energy_kcal", "vita_mg", "vitb1_mg", "vitb2_mg", "vitb3_mg", "vitb5_mg",
      "vitb6_mg", "vitb9_ug","vitaminb12_in_mg", "iron_mg", "calcium_mg", "zinc_mg")
  )

# quantile(per_capita_consumption$energy_kcal, 0.99, na.rm = TRUE)[[1]]

#look at the distributions of:
#     # Vit A, B1 2 3 5 6 9 12 Fe Zn Ca kcal
micronutrients <- colnames(per_capita_consumption)
micronutrients <- micronutrients[-c(1:3)]


mn_plots_list <- list()

for(item in micronutrients){
  print(item)
  p1 <- per_capita_consumption %>%
    ggplot(aes(x = !!sym(item), y = , fill = State_code)) +
    geom_histogram(position = "dodge") +
    stat_count(width = 0.5)+
    scale_color_discrete(colors = wes_palette("Zissou1","discrete", n = 3))+
    # xlim(-0.01,5)+
    theme_ipsum()
  mn_plots_list[[item]] <- p1
}

adm1_average_intake <- per_capita_consumption %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(State_code) %>% 
  dplyr::select(-c(HHID, District_code)) %>% 
  dplyr::summarise(
    dplyr::across(
      everything(),
      list(
        mean = mean,
        sd = sd
       )
      )
    )
  
## admin 2
 # per_capita_consumption %>% 
 #  dplyr::ungroup() %>% 
 #  dplyr::group_by(District_code) %>% 
 #  dplyr::select(-c(HHID,State_code) ) %>% 
 #   dplyr::
 #   sryvr::as_survey_design(id = cluster, strata =NULL, weights = weight, nest=T)
 #   dplyr::summarise(
 #    dplyr::across(
 #      everything(),
 #      list(
 #        mean = mean,
 #        sd = sd,
 #        n = dplyr::n_distinct
 #      )
 #    )
 #  ) %>% 
 #   dplyr::mutate(
 #     dplyr::across
 #   )


 
 

  



