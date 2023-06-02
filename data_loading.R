#### Gabriel Battcock
#### Data extraction from NIN dataset to machine learning dataframe
####
#### Extracts data from the NIN dataset, calculates the difference in mean MN intake 
#### between the head male and head female of the household. This is matched with binary data
#### of food group intake for the DQQ, GDQD and MDD. 

#packages
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
# library(sf)
library(ggmap)

setwd("~/Documents/LSHTM/WFP_project/MIMI")#set path for myself, please change if you want to use it
path_to_data <- "../IND_00062/"

# read in all the data files 
consumption <- read_csv(paste0(path_to_data, "consumption_user.csv"))
user <- read_csv(paste0(path_to_data, "subject_user.csv"))
food_dict <- read_excel(paste0(path_to_data, "food_groups/FoodEx2_Exposure_dict.xlsx"))
code_list <- read_csv(paste0(path_to_data, "code_lists.csv"))
DQQ <- read_csv(paste0(path_to_data, "food_groups/DQQ_library.csv"))
GDQS <- read_csv(paste0(path_to_data, "food_groups/GDQS_library.csv"))
MDD <- read_csv(paste0(path_to_data, "food_groups/MDD_library.csv"))
vb12_dict <- read_csv(paste0(path_to_data, "dictionaries/vb12_dict.csv"))
  
###### create vb12 variable    - can change if not correct
# consumption <- consumption %>% inner_join(vb12_dict, by = "FOODEX2_INGR_CODE") %>%
#   mutate(VITB12_mcg = (vitaminb12_in_mg*FOOD_AMOUNT_REPORTED)/100)
# write_csv(consumption, paste0(path_to_data, "consumption_user.csv"))

###### Constant variables to be used ###############


consumption$VITB12_mcg


# Using EAR from the NIN to calculate minimum requirements chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7231601/pdf/nmz096.pdf

vita_EAR_men_mcg <- 570
vita_EAR_women_mcg <- 490
folate_EAR_men_mcg  <-  250
folate_EAR_women_mcg <- 250
iron_EAR_men_mg <- 19.2
iron_EAR_women_mg <- 22.4
zinc_EAR_men_mg <- 11
zinc_EAR_women_mg <- 8.9
vitb12_EAR_men_mcg <- 2
vitb12_EAR_women_mcg <- 2

# upper limit taken from Chan. Public Health  <- https://www.hsph.harvard.edu/nutritionsource
iron_UL_mg <- 45
zinc_UL_mg <- 40
folate_UL_mcg <-  1000
vita_UL_mcg <- 3000

######## Functions ###########

###### FUNCTIONS ########

# calculate the sum of vitamins for a subject
# maybe write a function to do this 

MICRONUT_SUM <- function(data, micronutrient){
  # takes in the data frame and micronutrient wanted and calculates the sum for each subject
  data %>%  
    mutate(AGE_GROUP = factor(case_when(
      AGE_YEAR<1 ~ "0-1",
      AGE_YEAR<13 ~ "2-12",
      AGE_YEAR<18 ~ "13-17",
      AGE_YEAR<30 ~ "18-29",
      AGE_YEAR<65 ~ "30-64",
      AGE_YEAR>=65 ~ "65+"
    ),levels = c("0-1", "2-12", "13-17", "18-29", "30-64", "65+"))) %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR,AGE_GROUP,CONSUMPTION_DAY, ADM1_NAME, ADM2_NAME) %>% 
    mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female")))  %>% 
    mutate(ADM1_NAME = str_replace(ADM1_NAME, " ", "_")) %>% 
    summarise("sum_{{micronutrient}}" := sum({{micronutrient}})) #%>% 
    # arrange(HOUSEHOLD) 
  # total
}


######
DIFF_HEAD_OF_HOUSE <- function(data, micronutrient){
  # takes in the data frame and micronutrient wanted and calculates the sum for each subject
  data %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR) %>% 
    summarise(SUM = sum({{micronutrient}})) %>% 
    arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
    mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
    ungroup() %>%
    group_by(HOUSEHOLD, SEX) %>%
    filter(AGE_YEAR == max(AGE_YEAR)) %>% 
    ungroup() %>% 
    group_by(HOUSEHOLD) %>% 
    na.omit() %>% 
    mutate("DIFF_{{micronutrient}}" := SUM - lag(SUM, default = SUM[2])) %>% 
    select(!c(SEX,AGE_YEAR,SUM, ROUND))
}


DIFF_HOUSEHOLD <- function(data, micronutrient){
  # takes in the data frame and micronutrient wanted and calculates the sum for each subject
  # sorts into oldest members of household, takes both the oldest male and oldest female and calculates
  # the difference in observed intake
  
  male <- data %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
    filter(AGE_YEAR>=18) %>% 
    summarise(SUM = sum({{micronutrient}})) %>% 
    arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
    mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
    ungroup() %>%
    group_by(HOUSEHOLD, SEX) %>%
    filter(AGE_YEAR == max(AGE_YEAR)) %>% 
    filter(SEX == "Male") %>% 
    mutate(SUM_MALE = SUM)
    
  
  female <- data %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR,ADM1_NAME, ADM2_NAME) %>% 
    filter(AGE_YEAR>=18) %>% 
    summarise(SUM = sum({{micronutrient}})) %>% 
    arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
    mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
    ungroup() %>%
    group_by(HOUSEHOLD, SEX) %>%
    filter(AGE_YEAR == max(AGE_YEAR)) %>%
    filter(SEX == "Female") %>% 
    mutate(SUM_FEMALE = SUM)
  
  output <- male %>% 
    select(HOUSEHOLD, SUM_MALE, ADM1_NAME, ADM2_NAME) %>% 
    inner_join((female %>% select(HOUSEHOLD, SUM_FEMALE, ADM1_NAME, ADM2_NAME)), by = c("HOUSEHOLD","ADM1_NAME", "ADM2_NAME")) %>% 
    mutate(DIFF = SUM_MALE - SUM_FEMALE)
    
  output
}

DIFF_CHILREN_HOUSEHOLD <- function(data, micronutrient){
  # takes in the data frame and micronutrient wanted and calculates the sum for each subject
  male <- data %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
    summarise(SUM = sum({{micronutrient}})) %>% 
    arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
    mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
    ungroup() %>%
    group_by(HOUSEHOLD, SEX) %>%
    filter(SEX == "Male") %>% 
    filter(AGE_YEAR == max(AGE_YEAR)) %>% 
    mutate(SUM_MALE = SUM)
    
  
  female <- data %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR,ADM1_NAME, ADM2_NAME) %>% 
    summarise(SUM = sum({{micronutrient}})) %>% 
    arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
    mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
    ungroup() %>%
    group_by(HOUSEHOLD, SEX) %>%
    filter(SEX == "Female") %>% 
    filter(AGE_YEAR == max(AGE_YEAR)) %>%
    mutate(SUM_FEMALE = SUM)
  
  output <- male %>% 
    select(HOUSEHOLD, SUM_MALE, ADM1_NAME, ADM2_NAME) %>% 
    inner_join((female %>% select(HOUSEHOLD, SUM_FEMALE, ADM1_NAME, ADM2_NAME)), by = c("HOUSEHOLD","ADM1_NAME", "ADM2_NAME")) %>% 
    mutate(DIFF = SUM_MALE - SUM_FEMALE)
    
  output
}



FOOD_GROUP_LIST <- function(data, food_list){
  # takes take in the consumption data and transforms it into a single row with a 1 or 0 for whether
  # or not a food group was consumed
  
  sum_or_function <- function(x){
    #creates OR logic gate
    y = sum(x)
    y = ifelse(y != 0, 1, 0)
    y
  }
  
  data  %>% 
    select(SUBJECT, FOODEX2_INGR_CODE) %>% 
    full_join(food_list, by = "FOODEX2_INGR_CODE") %>% 
    select(!c(FOODEX2_INGR_CODE, INGREDIENT_ENG)) %>% 
    group_by(SUBJECT) %>% 
    summarise_all(sum_or_function) 
}

# create the initial datasets

#create a joined user and consumption
joined <- full_join(user, consumption, by = c("SUBJECT","ROUND")) 
#micronutrients
vitamin_A_calc <- DIFF_HEAD_OF_HOUSE(joined, VITA_RAE_mcg)
folate_calc <- DIFF_HEAD_OF_HOUSE(joined, FOLDFE_mcg)
iron_calc <- DIFF_HEAD_OF_HOUSE(joined, IRON_mg)
zinc_calc <- DIFF_HEAD_OF_HOUSE(joined, ZINC_mg)

joined$VITB12_mcg
#difference per household
vit_a_household <- DIFF_HOUSEHOLD(joined, VITA_RAE_mcg)
folate_household <- DIFF_HOUSEHOLD(joined, FOLDFE_mcg)
vit_b12_household <- DIFF_HOUSEHOLD(joined, VITB12_mcg)
iron_household <- DIFF_HOUSEHOLD(joined, IRON_mg)
zinc_household <- DIFF_HOUSEHOLD(joined, ZINC_mg)

# Population variables
vit_a_population <- MICRONUT_SUM(joined, VITA_RAE_mcg)
folate_population <- MICRONUT_SUM(joined, FOLDFE_mcg)
vit_b12_population <- MICRONUT_SUM(joined, VITB12_mcg)
iron_population <- MICRONUT_SUM(joined, IRON_mg)
zinc_population <- MICRONUT_SUM(joined, ZINC_mg)


#### rice difference

RICE_men <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED)) %>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(AGE_YEAR == max(AGE_YEAR)) %>% 
  filter(SEX == "Male") 

RICE_women <- joined %>% 
  filter(AGE_YEAR>=18) %>% 
  filter(grepl("RICE",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_RICE_g = sum(FOOD_AMOUNT_REPORTED))%>% 
  arrange(HOUSEHOLD, desc(AGE_YEAR), SEX) %>%
  mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female"))) %>% 
  ungroup() %>%
  group_by(HOUSEHOLD, SEX) %>%
  filter(AGE_YEAR == max(AGE_YEAR)) %>% 
  filter(SEX == "Female") 


RICE_HOUSEHOLD <- RICE_men %>% 
  ungroup() %>% 
  rename(RICE_men_g = sum_RICE_g) %>% 
  select(HOUSEHOLD, ADM2_NAME, RICE_men_g) %>% 
  full_join((RICE_women %>% ungroup() %>% 
               rename(RICE_women_g = sum_RICE_g
                      ) %>% 
               select(HOUSEHOLD, ADM2_NAME, RICE_women_g)),
            by = c("HOUSEHOLD","ADM2_NAME")) %>% 
  mutate(diff_rice_g = RICE_men_g - RICE_women_g) %>% 
  ungroup() %>% 
  group_by(ADM2_NAME) %>% 
  summarise(mean_rice_g = mean(diff_rice_g, na.rm = T)) %>% 
  left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")


RICE_HOUSEHOLD %>% 
  ggplot(aes(x = diff_rice_g)) +
  geom_histogram(color = "#69b3a2",  fill="#404080", alpha = 1, position = 'dodge') +
  theme_ipsum() +
  labs(title = "Rice consumption difference per household",
       x = "Difference in rice consumption (g)")+
  My_Theme

with(RICE_HOUSEHOLD, t.test(RICE_men_g, RICE_women_g))

st_write(RICE_HOUSEHOLD, paste0(path_to_data, "shape_files/rice/rice_household_sp.shp"), append = TRUE)
  

## oil data sets

OIL_men <- joined %>% 
  filter(AGE_YEAR>=18 & SEX == 1) %>% 
  filter(grepl("OIL",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_OIL_g = sum(FOOD_AMOUNT_REPORTED))

OIL_women <- joined %>% 
  filter(AGE_YEAR>=18 & SEX == 2) %>% 
  filter(grepl("OIL",INGREDIENT_ENG.y)) %>% 
  group_by(SUBJECT, HOUSEHOLD,SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
  summarise(sum_OIL_g = sum(FOOD_AMOUNT_REPORTED))



names(joined)

india_adm2


# food lists
# DQQ_list <- FOOD_GROUP_LIST(joined,DQQ)
# GDQS_list <- FOOD_GROUP_LIST(joined,GDQS)
# MDD_list <- FOOD_GROUP_LIST(joined, MDD)
# 
# # usable data frames
# # DQQ
# DQQ_vit_a <- inner_join(vitamin_A_calc, DQQ_list, by = "SUBJECT")
# DQQ_folate <- inner_join(folate_calc, DQQ_list, by = "SUBJECT")
# DQQ_iron <- inner_join(iron_calc, DQQ_list, by = "SUBJECT")
# DQQ_zinc <- inner_join(zinc_calc, DQQ_list, by = "SUBJECT")
# 
# #GDQS
# GDQS_vit_a <- inner_join(vitamin_A_calc, GDQS_list, by = "SUBJECT")
# GDQS_folate <- inner_join(folate_calc, GDQS_list, by = "SUBJECT")
# GDQS_iron <- inner_join(iron_calc, GDQS_list, by = "SUBJECT")
# GDQS_zinc <- inner_join(zinc_calc, GDQS_list, by = "SUBJECT")
# 
# #MDD
# MDD_vit_a <- inner_join(vitamin_A_calc, MDD_list, by = "SUBJECT")
# MDD_folate <- inner_join(folate_calc, MDD_list, by = "SUBJECT")
# MDD_iron <- inner_join(iron_calc, MDD_list, by = "SUBJECT")
# MDD_zinc <- inner_join(zinc_calc, MDD_list, by = "SUBJECT")


# mean intake for all types of demographics within households
# how mean intakes interact between population groups
# estimating intake inadequacy for the different MNs
# 

