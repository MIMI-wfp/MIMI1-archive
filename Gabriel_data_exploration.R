#Gabriel Battcock
#playing with the data
# MIMI poject

#packages
library(tidyr)
library(dplyr)
library(readr)

setwd("~/Documents/LSHTM/WFP_project/MIMI")

path_to_data <- "../IND_00062/"

consumption <- read_csv(paste0(path_to_data, "consumption_user.csv"))
user <- read_csv(paste0(path_to_data, "subject_user.csv"))

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
    group_by(SUBJECT, ROUND.x) %>% 
    summarise("sum_{{micronutrient}}" := sum({{micronutrient}})) #%>% 
    # select(SUBJECT, ROUND.x, sum)
  # total
}

vit_a <- MICRONUT_SUM(joined, VITA_RAE_mcg)
vit_c <- MICRONUT_SUM(joined, VITC_mg)
thia <- MICRONUT_SUM(joined, THIA_mg)

hist(vit_c$sum)
hist(thia$sum)
### this all works, this can help define the target 

grep("A03V",joined$FOODEX2_INGR_CODE)#A03V is where composite foods begin in the classification scheme -- makes grouping foods more difficult
grep("A03X",joined$FOODEX2_INGR_CODE)
grep("A03Y",joined$FOODEX2_INGR_CODE)
joined$FOODEX2_INGR_CODE[297216]

#### how to group codes together to create a covariate
#### what we want is a data frame with one row for each subject, a target variable (yes/no inadequate intake of MN)
#### and a yes/no covarate for different food groups. 
#### Need to search the food ingredient code column for specific indicies and mutate to a new column with 1 or 0
