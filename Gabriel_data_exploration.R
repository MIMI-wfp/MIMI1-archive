#Gabriel Battcock
#playing with the data
# MIMI poject

#packages
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(sf)
library(ggmap)

setwd("~/Documents/LSHTM/WFP_project/MIMI")

path_to_data <- "../IND_00062/"


consumption <- read_csv(paste0(path_to_data, "consumption_user.csv"))
user <- read_csv(paste0(path_to_data, "subject_user.csv"))
food_dict <- read_excel(paste0(path_to_data, "FoodEx2_Exposure_dict.xlsx"))
code_list <- read_csv(paste0(path_to_data, "code_lists.csv"))
DQQ <- read_csv(paste0(path_to_data, "DQQ_library.csv"))

#summarise the data 
summary(consumption)
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

user$METHOD_WEIGHT
table(user$BREASTFEEDING)
table(user$SPECIAL_DIET)
table(user$SEX)#a bit skewed towards females
table(consumption$CONSUMPTION_MONTH)#more results in the winter months? bias?



unique(joined$ADM2_NAME)
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

vit_c$sum_VITC_mg <- ifelse(vit_c>200, mean(vit_c$sum_VITC_mg), vit_c$sum_VITC_mg)

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
food_list_B <- c("A036X", "A02LV")

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


white_root_tubers <- data.frame(FOODEX2_INGR_CODE = c("A00ZS", "A00ZT", "A00ZV","A00ZX","A00ZY",
                                                "A00ZZ","A010A","A010B","A010C",
                 "A010D","A010E","A010F","A010G","A010H","A010J","A010K","A010L","A010M",
                 "A010N","A010P","A010Q","A010R","A010T","A010V","A010Y","A010Z","A011B",
                 "A011C","A011D","A011E","A011F","A011G","A011J","A011L","A011M","A011N",
                 "A011P","A011Q","A011R","A011S","A011T","A011V")
)

group_1_codes <- food_dict %>% 
  inner_join(group1, by = "FC_L4") %>% 
  select(termCode) %>% 
  rename(FOODEX2_INGR_CODE = termCode) 

group_1_codes$FOODEX2_INGR_CODE
code_list$grains <- group_1_codes$FOODEX2_INGR_CODE
  
#group 2: PULSES (BEANS, PEAS AND LENTILS) ------- this wont work!! 
##### category "dried fruit and vegetables" includes both pulses (mung beans, kidney beans etc) and dried fruit like mango 
#### need to list specific termCodes... 

group_2_codes <- data.frame(FOODEX2_INGR_CODE = c(
  "A012S","A012T","A012V","A012X","A012Y","A012Z",
  "A013A","A013B","A013C","A013D","A013E","A013F",
  "A013G","A013H","A013J","A013K","A013L","A013M",
  "A013N","A013P","A013Q","A013R","A013S","A013T",
  "A013V","A013X","A013Y","A013Z","A014A","A0DBV",
  "A0DBX","A0DBY","A0DBZ","A0DCA","A0DCB","A0DCC",
  "A0DCD","A0DCE","A0DCF","A0DCG","A0DCH","A0DCJ",
  "A0DCK","A0DCL","A0DCM","A011Y","A011Z","A012A",
  "A012B","A012C","A012D","A012E","A012F","A012G",
  "A012J","A012K","A012L","A012N"
  )
)


#group 3: NUTS AND SEEDS ----------



group3 <- data.frame(FC_L4 = c("Processed nuts"))

group_3_codes <- food_dict %>% 
  inner_join(group3, by = "FC_L4") %>% 
  select(termCode) %>% 
  rename(FOODEX2_INGR_CODE = termCode) %>% 
  add_row(FOODEX2_INGR_CODE = c("A01BL","A01BM","A01BN"))
  
#group 4: DAIRY ---------

group4 <- data.frame(FC_L4 = c(
  "Unflavoured pasteurised and sterilised (including UHT) milk",
  "Unflavoured fermented milk products, including natural unflavoured buttermilk (excluding sterilised buttermilk) non heat-treated after fermentation",
  "Butter and concentrated butter and butter oil and anhydrous milkfat"
))

group_4_codes <- food_dict %>% 
  inner_join(group4, by = "FC_L4") %>% 
  select(termCode) %>% 
  rename(FOODEX2_INGR_CODE = termCode)


#group 5: MEAT, POULTRY AND FISH -----------

group5 <- data.frame(FC_L4 = c(
  "Fresh meat, excluding meat preparations as defined by Regulation (EC) No 853/2004",                                                                  
  "Unprocessed fish",
  "Unprocessed molluscs and crustaceans",
  "Processed fish and fishery products including molluscs and crustaceans" 
))

group_5_codes <- food_dict %>% 
  inner_join(group5, by = "FC_L4") %>% 
  select(termCode) %>% 
  rename(FOODEX2_INGR_CODE = termCode)


#group 6: EGGS ---------
group6 <- data.frame(FC_L4 = c(
  "Unprocessed eggs"  
))

group_6_codes <- food_dict %>% 
  inner_join(group6, by = "FC_L4") %>% 
  select(termCode) %>% 
  rename(FOODEX2_INGR_CODE = termCode)

#group 7: DARK GREEN LEAFY VEGETABLES



#group 8: other VIT A RICH FOODS


#group 9: Other vegetables



#group 10: Other fruits



food_group <- joined %>% group_by(SUBJECT) %>% 
  summarise(
    food_group_1 = sum(group_1_codes$FOODEX2_INGR_CODE[1:5] %in% joined$FOODEX2_INGR_CODE[][1:5]),
    food_group_2 = sum(group_2_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE[1:5]),
    food_group_3 = sum(group_3_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE),
    food_group_4 = sum(group_4_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE),
    food_group_5 = sum(group_5_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE),
    food_group_6 = sum(group_6_codes$FOODEX2_INGR_CODE %in% FOODEX2_INGR_CODE),
    food_group_7 = sum(code_list$leafy_green %in% FOODEX2_INGR_CODE)
   ) #%>% 
  # filter(food_group_6 != 0) #%>% 
  # arrange( desc(food_group_1))
food_group


############## creating code lists from the food dictionary

# code_list_name <- food_dict


consumption %>% unique()

