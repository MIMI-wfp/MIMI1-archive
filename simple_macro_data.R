# Script for creating SAS files for simpleMacro running

source("data_loading.R")
library(haven)

#vitamin A

vitamin_a_SAS <- vit_a_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
  mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
  mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
  mutate(adm2_ = factor(ADM2_NAME),
         ADM2_NAME = paste0("adm2_",(factor(ADM2_NAME))))  %>% 
  pivot_wider(names_from = ADM2_NAME, 
              values_from = ADM2_NAME,
              values_fill = 0,
              values_fn = function(x) 1) 
  
write_csv(vitamin_a_SAS,"outputs/SAS_data/vitamin_a_SAS.csv")

folate_SAS <- folate_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
  mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
  mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
  mutate(adm2_ = factor(ADM2_NAME),
         ADM2_NAME = paste0("adm2_",(factor(ADM2_NAME)))) %>% 
  pivot_wider(names_from = ADM2_NAME, 
              values_from = ADM2_NAME,
              values_fill = 0,
              values_fn = function(x) 1) 

write_csv(folate_SAS,"outputs/SAS_data/folate_SAS.csv")

iron_SAS <- iron_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
  mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
  mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
  mutate(adm2_ = factor(ADM2_NAME),
         ADM2_NAME = paste0("adm2_",(factor(ADM2_NAME)))) %>% 
  pivot_wider(names_from = ADM2_NAME, 
              values_from = ADM2_NAME,
              values_fill = 0,
              values_fn = function(x) 1) 

write_csv(iron_SAS,"outputs/SAS_data/iron_SAS.csv")

zinc_SAS <- zinc_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
  mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
  mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
  mutate(adm2_ = factor(ADM2_NAME),
         ADM2_NAME = paste0("adm2_",(factor(ADM2_NAME)))) %>% 
  pivot_wider(names_from = ADM2_NAME, 
              values_from = ADM2_NAME,
              values_fill = 0,
              values_fn = function(x) 1) 

write_csv(zinc_SAS,"outputs/SAS_data/zinc_SAS.csv")


