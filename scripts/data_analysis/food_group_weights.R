## grams per day of mn foods

setwd("~/Documents/LSHTM/WFP_project/MIMI/scripts")
source("data_extraction/data_loading.R")


## Vitamin A -------------------------------------------------------------------

names(joined)
head(joined)
head(GDQS)
names(GDQS)



#dark greens

weight_food_group <- function(food_group){
  joined %>% 
    group_by(SUBJECT, ADM2_NAME, SEX) %>% 
    mutate(amount_g = ifelse(FOODEX2_INGR_CODE %in%
                               (GDQS %>% filter({{food_group} }== 1) %>% 
                                  select(FOODEX2_INGR_CODE))$FOODEX2_INGR_CODE,
                             FOOD_AMOUNT_REPORTED,0
                             
    )) %>% 
    select(SUBJECT, ADM2_NAME,SEX, amount_g, AGE_YEAR) %>% 
    summarise(sum_g = sum(amount_g)) %>% 
    ungroup()
}



dark_green <-   weight_food_group(g4_dark_leafy_green)

#deep orange fruit
deep_orange_fruit <-  weight_food_group(g2_deep_orange_fruit)

#deep orange veg
deep_orange_veg <-  weight_food_group(g6_deep_orange_veg)

#other veg
other_veg <-  weight_food_group(g7_other_veg ) 

#fish
fish <-  weight_food_group(g13_fish ) 

#poultry
poultry <-  weight_food_group(g14_poultry_game) 

#red meat
red_meat <-  weight_food_group(g18_red_meat ) 

names(GDQS)

#citrus
citrus <- weight_food_group(g1_citrus)

#other fruits
other_fruts <- weight_food_group(g3_other_fruits)

# cruciferous

cruciferous <- weight_food_group(g5_cruiciferous_veg)

#other veg
legumes <- weight_food_group(g8_legumes)

#orange tubers
orange_tubers <- weight_food_group(g9_deep_orange_tubers)

#nuts
nuts_seeds <- weight_food_group(g10_nuts_seeds)

# whole grain

whole_grain <- weight_food_group(g11_whole_grain)

#lf dairy
lf_dairy <- weight_food_group(g15_lf_dairy)

#eggs
eggs <- weight_food_group(g16_eggs)
