#Gabriel Battcock
#playing with the data
# MIMI poject

#packages
library(tidyr)
library(dplyr)
library(readr)
library("readxl")
library(sf)
library(ggmap)

setwd("~/Documents/LSHTM/WFP_project/MIMI")

path_to_data <- "../IND_00062/"


consumption <- read_csv(paste0(path_to_data, "consumption_user.csv"))
user <- read_csv(paste0(path_to_data, "subject_user.csv"))
food_dict <- read_excel(paste0(path_to_data, "FoodEx2_Exposure_dict.xlsx"))


#summarise the data 

nrow(consumption)
nrow(user$SUBJECT)

#39719 individuals each with multiple rows in the consumption table due to many ingredients

names(user)
hist(user$WEIGHT)
hist(user$HEIGHT)
hist(user$AGE_YEAR)
unique(user$ADM1_NAME) #not all states included
unique(user$ADM2_NAME)

#consumption data
names(consumption)
unique(consumption$FOODEX2_INGR_CODE)
unique(consumption$FOODEX2_INGR_DESCR)

length(unique(consumption$FOODEX2_INGR_CODE))
length(unique(consumption$FOODEX2_INGR_DESCR)) ## one food descriptor missing from code

###### joining the data

joined <- full_join(user, consumption, by = "SUBJECT") 
head(joined)
nrow(joined)


# calculate the sum of vitamins for a subject
# maybe write a function to do this 

total_vitA <- joined %>% 
  group_by(SUBJECT, ROUND.x) %>% 
  summarise(sum =  sum(VITA_RAE_mcg)) %>% 
  select(SUBJECT, ROUND.x, sum)

hist(total_vitA$sum)
summary(total_vitA$sum)
which.max(total_vitA$sum)
# some huge values 9222.0 = 9g!


MICRONUT_SUM <- function(data, micronutrient){
  # takes in the data frame and micronutrient wanted and calculates the sum for each subject
  data %>% 
    group_by(SUBJECT) %>% 
    summarise("sum_{{micronutrient}}" := sum({{micronutrient}})) #%>% 
    # select(SUBJECT, ROUND.x, sum)
  # total
}

vit_a <- MICRONUT_SUM(joined, VITA_RAE_mcg)
vit_c <- MICRONUT_SUM(joined, VITC_mg)
thia <- MICRONUT_SUM(joined, THIA_mg)

hist(vit_c$sum_VITC_mg)
hist(thia$sum_THIA_mg)
### this all works, this can help define the target 

grep("A03V",joined$FOODEX2_INGR_CODE)#A03V is where composite foods begin in the classification scheme -- makes grouping foods more difficult
grep("A03X",joined$FOODEX2_INGR_CODE)
grep("A03Y",joined$FOODEX2_INGR_CODE)
joined$FOODEX2_INGR_CODE[297216]

#### how to group codes together to create a covariate
#### what we want is a data frame with one row for each subject, a target variable (yes/no inadequate intake of MN)
#### and a yes/no covarate for different food groups. 
#### Need to search the food ingredient code column for specific indicies and mutate to a new column with 1 or 0 (OR IS IT A COUNT?)


food_list_A <- c("A00GZ", "A001L")     #test food list 
food_list_B <- c("A036X", "A02LV", "")

food_group <- joined %>% group_by(SUBJECT) %>% 
  summarise(
    food_group_A = sum(food_list_A %in% FOODEX2_INGR_CODE),
    food_group_B = sum(food_list_B %in% FOODEX2_INGR_CODE),
  ) %>% 
  select(SUBJECT,food_group_A, food_group_B) 

#join for one data set with the target (vitamin intake) and a covariate (food group)
food_group <- food_group %>% left_join(vit_a, by = "SUBJECT") %>% 
  left_join(vit_c, by = "SUBJECT") %>% 
  left_join(thia, by = "SUBJECT") 
food_group


#### creating code lists using the data dictionary 
food_groups_in_data <- food_dict %>% 
  rename(FOODEX2_INGR_CODE = termCode) %>% 
  inner_join(consumption, by = "FOODEX2_INGR_CODE") %>% 
  select(FOODEX2_INGR_CODE, FC_code, FC_L4) %>% 
  group_by(FC_code) %>% 
  summarise(unique(FC_L4))
  
food_groups_in_data$`unique(FC_L4)`

#### food categories based on MDD-W 

#group 1 seeds, grains and white tubulars
group1 <- data.frame(FC_L4 = c("Whole, broken, or flaked grain",                                                                                                                 
            "Flours",
            "Starches",
            "Breakfast cereals",
            "Bread and rolls",
            "Fine bakery wares"))

group_1_codes <- food_dict %>% 
  inner_join(group1, by = "FC_L4") %>% 
  select(termCode) %>% 
  rename(FOODEX2_INGR_CODE = termCode) 


  
#group 2: PULSES (BEANS, PEAS AND LENTILS) ------- this wont work!! 
##### category "dried fruit and vegetables" includes both pulses (mung beans, kidney beans etc) and dried fruit like mango 
#### need to list specific termCodes... 




food_group <- joined %>% group_by(SUBJECT) %>% 
  summarise(
    food_group_1 = sum(group_1_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE),
    food_group_2 = sum(group_2_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE),
    food_group_3 = sum(group_3_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE),
    food_group_4 = sum(group_4_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE)
  ) %>% 
  filter(food_group_1 != 0) %>% 
  arrange( desc(food_group_1))
