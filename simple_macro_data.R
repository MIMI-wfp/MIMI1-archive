# Script for creating SAS files for simpleMacro running

source("data_loading.R")


#vitamin A

all_mn <- vit_a_population %>% 
  left_join(folate_population, by = c("SUBJECT", 
                                      "ROUND", 
                                      "HOUSEHOLD",
                                      "SEX",
                                      "AGE_YEAR",
                                      "AGE_GROUP",
                                      "CONSUMPTION_DAY",
                                      "ADM1_NAME",
                                      "ADM2_NAME")) %>% 
  left_join(iron_population, by = c("SUBJECT", 
                                      "ROUND", 
                                      "HOUSEHOLD",
                                      "SEX",
                                      "AGE_YEAR",
                                      "AGE_GROUP",
                                      "CONSUMPTION_DAY",
                                      "ADM1_NAME",
                                      "ADM2_NAME")) %>% 
  left_join(zinc_population, by = c("SUBJECT", 
                                      "ROUND", 
                                      "HOUSEHOLD",
                                      "SEX",
                                      "AGE_YEAR",
                                      "AGE_GROUP",
                                      "CONSUMPTION_DAY",
                                      "ADM1_NAME",
                                      "ADM2_NAME")) %>% 
  left_join(vit_b12_population, by = c("SUBJECT", 
                                    "ROUND", 
                                    "HOUSEHOLD",
                                    "SEX",
                                    "AGE_YEAR",
                                    "AGE_GROUP",
                                    "CONSUMPTION_DAY",
                                    "ADM1_NAME",
                                    "ADM2_NAME"))



create_state <- function(micronutrient, state_name) {
  va_state <- micronutrient %>%
    ungroup() %>%
    filter(AGE_YEAR >= 18) %>%
    filter(ADM1_NAME == state_name) %>%
    select(-c(ROUND, AGE_GROUP, ADM1_NAME)) %>%
    drop_na(ADM2_NAME) %>% 
    mutate(SEX = factor(ifelse(SEX == "Male", 1, 2))) %>%
    mutate(WEEKEND = ifelse(CONSUMPTION_DAY == 1 | CONSUMPTION_DAY == 7, 1, 0)) %>%
    mutate(
      adm2_ = as.numeric(factor(ADM2_NAME)),
      ADM2_NAME = paste0("adm2_", as.numeric(factor(ADM2_NAME)))
    ) %>%
    
    pivot_wider(
      names_from = ADM2_NAME,
      values_from = ADM2_NAME,
      values_fill = 0,
      values_fn = function(x) 1
    )
  
  write_csv(va_state, paste0("outputs/SAS_data/va_", state_name, "_SAS.csv"))
  
  
}

unique(vit_a_population$ADM1_NAME)

va_Maharashtra <- create_state(all_mn,"Maharashtra")
va_West_Bengal <- create_state(all_mn,"West_Bengal")
va_Gujarat <- create_state(all_mn,"Gujarat")
va_Orissa <- create_state(all_mn,"Orissa")
va_Tamil_Nadu <- create_state(all_mn,"Tamil_Nadu")
va_Andhra_Pradesh <- create_state(all_mn,"Andhra_Pradesh")
va_Madhya_Pradesh <- create_state(all_mn,"Madhya_Pradesh" )
va_Kerala <- create_state(all_mn,"Kerala")
va_Karnataka<- create_state(all_mn,"Karnataka")
va_Uttar_Pradesh<- create_state(all_mn,"Uttar_Pradesh")

names(va_West_Bengal)
sum(va_$sum_VITA_RAE_mcg == 0)
sum(va_West_Bengal$WEEKEND)
sum(va_Uttar_Pradesh$WEEKEND)

va_Maharashtra <- vit_a_population %>% 
    ungroup() %>% 
    filter(AGE_YEAR>=18) %>% 
    filter(ADM1_NAME == Maharashtra) %>% 
    select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
    mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
    mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
    mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
           ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
    pivot_wider(names_from = ADM2_NAME, 
                values_from = ADM2_NAME,
                values_fill = 0,
                values_fn = function(x) 1) 
  
  names(vitamin_a_SAS)
    
  write_csv(va_Maharashtra,"outputs/SAS_data/va_Maharashtra_SAS.csv")

va_Maharashtra <- state_sas(Maharashtra)

folate_SAS <- folate_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
  mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
  mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
  mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
         ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
  pivot_wider(names_from = ADM2_NAME, 
              values_from = ADM2_NAME,
              values_fill = 0,
              values_fn = function(x) 1) 
#str_replace(ADM2_NAME, " ", "_")
write_csv(folate_SAS,"outputs/SAS_data/folate_SAS.csv")

iron_SAS <- iron_population %>% 
  ungroup() %>% 
  filter(AGE_YEAR>=18) %>% 
  select(!c(ROUND, AGE_GROUP, ADM1_NAME)) %>% 
  mutate(SEX = factor(ifelse(SEX=="Male", 1,2))) %>% 
  mutate(WEEKEND = ifelse((CONSUMPTION_DAY == 1|CONSUMPTION_DAY == 7),1,0)) %>% 
  mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
         ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
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
  mutate(adm2_ = as.numeric(factor(ADM2_NAME)),
         ADM2_NAME = paste0("adm2_",as.numeric(factor(ADM2_NAME)))) %>% 
  pivot_wider(names_from = ADM2_NAME, 
              values_from = ADM2_NAME,
              values_fill = 0,
              values_fn = function(x) 1) 

write_csv(zinc_SAS,"outputs/SAS_data/zinc_SAS.csv")


