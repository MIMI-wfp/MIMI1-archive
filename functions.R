#### Gabriel Battcock
## All the functions for MIMI project

#packages
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
# library(sf)
library(ggmap)


setClass("micronutrient",    slots = list(
        name = "character",
        EAR_men = "numeric",
        EAR_women = "numeric",
        UL = "numeric",
        data = "data.frame",
        value = "character"

))

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
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR,AGE_GROUP, ADM1_NAME, ADM2_NAME) %>% 
    mutate(SEX = factor(ifelse(SEX == 1, "Male", "Female")))  %>% 
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
  male <- data %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR, ADM1_NAME, ADM2_NAME) %>% 
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
    filter(AGE_YEAR == max(AGE_YEAR)) %>% 
    filter(SEX == "Male") %>% 
    mutate(SUM_MALE = SUM)
    
  
  female <- data %>% 
    group_by(SUBJECT, ROUND, HOUSEHOLD, SEX, AGE_YEAR,ADM1_NAME, ADM2_NAME) %>% 
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


inadequacy_MN <- function(micronutrient_object){
    #calclates whether or not they are inadequate based on 
    # MEAN intake and EAR 
    # this is probably not correct on an individual level and 
    # will probably need fixing but gives a good idea
    # at a ADM2 level where there is high burden 
    
  Men <-  new("micronutrient",
    EAR_men = micronutrient_object@EAR_men,
    EAR_women = micronutrient_object@EAR_women,
    UL = micronutrient_object@UL, 
    data = micronutrient_object@data %>% 
      filter(AGE_YEAR>17 & SEX == "Male"),
    value = micronutrient_object@value
  )
  Women <-  new("micronutrient",
    EAR_men = micronutrient_object@EAR_men,
    EAR_women = micronutrient_object@EAR_women,
    UL = micronutrient_object@UL, 
    data = micronutrient_object@data %>% 
      filter(AGE_YEAR>17 & SEX == "Female"),
    value = micronutrient_object@value
  )
    
  #   micronutrient_object@data %>%
  # filter(AGE_YEAR>17 & SEX == "Male")
  # women <- micronutrient_object@data %>%
  # filter(AGE_YEAR>17 & SEX == "Female")

  
  temp_men <- Men@data %>%
    ungroup()  %>%
    rename(SUM = Men@value) %>%
    select(SUBJECT, SUM, ADM2_NAME)  %>% 
    mutate(INADEQUATE = factor(ifelse(SUM<= Men@EAR_men, 1, 0))) %>%
    group_by(ADM2_NAME) %>%
    summarise(percentage =(length( INADEQUATE[ which( INADEQUATE == 1 ) ])/n()))  

  temp_women <- Women@data %>%
    ungroup()  %>%
    rename(SUM = Women@value) %>%
    select(SUBJECT, SUM, ADM2_NAME)  %>% 
    mutate(INADEQUATE = factor(ifelse(SUM<= Women@EAR_women, 1, 0))) %>%
    group_by(ADM2_NAME) %>% 
    summarise(percentage =(length( INADEQUATE[ which( INADEQUATE == 1 ) ])/n()))  

  final  <- temp_men %>% 
    left_join(temp_women, by = "ADM2_NAME") %>% 
    rename(percentage_men = percentage.x,
           percentage_women = percentage.y)

  final <- final  %>% left_join(india_adm2 %>% rename(ADM2_NAME = shapeName), by = "ADM2_NAME")
  final
}
