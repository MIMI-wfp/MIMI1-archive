# new India FCT matching check
# Gabriel Battcock
# 22 04 2024


rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))


# read in the old and new fcts
old_ind_fct <- read.csv('/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/base models/data/archive/20231205/nsso1112_fct.csv')
new_ind_fct <- read.csv('/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/base models/data/current/ind_nss1112_fct.csv')
ind_food_consumption <- read.csv('/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/base models/data/current/ind_nss1112_food_consumption.csv')
ind_hh_info <- read.csv('/Users/gabrielbattcock/Library/CloudStorage/OneDrive-WorldFoodProgramme/General - MIMI Project/Nutrition analysis/base models/data/current/ind_nss1112_hh_info.csv')


# create just a vitamin A model -----------------------------------------------

vita_check <- old_ind_fct %>% 
  rename_with(~paste0(., "_old"), -c("item_code", "item_name")) %>% 
  left_join(new_ind_fct, by = c("item_code","item_name")) %>% 
  select(item_code,item_name, vita_rae_mcg,vita_rae_mcg_old) %>% 
  mutate(vita_rae_mcg_old = ifelse(is.na(vita_rae_mcg_old),0,vita_rae_mcg_old)) %>% 
  mutate(difference = vita_rae_mcg - vita_rae_mcg_old)

vita_check %>% 
  filter(difference != 0)


# check the apparent intake change
hh_info_name <- names(ind_hh_info)
vita_intake <- ind_food_consumption %>% 
  left_join(vita_check,by = "item_code") %>% 
  left_join(ind_hh_info %>% 
              select(hhid,afe), by = "hhid") %>% 
  mutate(quantity_100g = quantity_100g/afe,
         quantity_g = quantity_g/afe)  

  
vita_intake %>% 
  mutate(va_intake_old = vita_rae_mcg_old*quantity_100g,
        va_intake_new = vita_rae_mcg*quantity_100g) %>% 
  group_by(hhid) %>% 
  summarise(
    va_ai_old = sum(va_intake_old,na.rm = T),
    va_ai_new = sum(va_intake_new,na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    prev_old = ifelse(va_ai_old<490,1,0),
    prev_new = ifelse(va_ai_new<490,1,0)
  ) %>% 
  left_join(ind_hh_info %>% 
              select(hhid,survey_wgt), by = "hhid") %>% 
  srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::summarise(
    prev_old = (survey_mean(prev_old == 1, proportion = TRUE)) * 100,
    prev_new = (survey_mean(prev_new == 1, proportion = TRUE)) * 100)


# vitamin b12

vitb12_check <- old_ind_fct %>% 
  rename_with(~paste0(., "_old"), -c("item_code", "item_name")) %>% 
  left_join(new_ind_fct, by = c("item_code","item_name")) %>% 
  select(item_code,item_name, vitb12_mcg,vitb12_mcg_old) %>% 
  mutate(vitb12_mcg_old = ifelse(is.na(vitb12_mcg_old),0,vitb12_mcg_old)) %>% 
  mutate(difference = vitb12_mcg - vitb12_mcg_old)

vitb12_check %>% 
  filter(difference != 0)


# check the apparent intake change
hh_info_name <- names(ind_hh_info)
vitb12_intake <- ind_food_consumption %>% 
  left_join(vitb12_check,by = "item_code") %>% 
  left_join(ind_hh_info %>% 
              select(hhid,afe), by = "hhid") %>% 
  mutate(quantity_100g = quantity_100g/afe,
         quantity_g = quantity_g/afe)  


vitb12_intake %>% 
  mutate(vb12_intake_old = vitb12_mcg_old*quantity_100g,
         vb12_intake_new = vitb12_mcg*quantity_100g) %>% 
  group_by(hhid) %>% 
  summarise(
    vb12_ai_old = sum(vb12_intake_old,na.rm = T),
    vb12_ai_new = sum(vb12_intake_new,na.rm = T)
  ) %>%
  ungroup() %>%
  summarise(
    mean(vb12_ai_old),
    mean(vb12_ai_new)
  )


# iron

fe_check <- old_ind_fct %>% 
  rename_with(~paste0(., "_old"), -c("item_code", "item_name")) %>% 
  left_join(new_ind_fct, by = c("item_code","item_name")) %>% 
  select(item_code,item_name, fe_mg,fe_mg_old) %>% 
  mutate(fe_mg_old = ifelse(is.na(fe_mg_old),0,fe_mg_old)) %>% 
  mutate(difference = fe_mg - fe_mg_old)

fe_check %>% 
  filter(difference != 0)


# check the apparent intake change
hh_info_name <- names(ind_hh_info)
fe_intake <- ind_food_consumption %>% 
  left_join(fe_check,by = "item_code") %>% 
  left_join(ind_hh_info %>% 
              select(hhid,afe), by = "hhid") %>% 
  mutate(quantity_100g = quantity_100g/afe,
         quantity_g = quantity_g/afe)  


fe_intake %>% 
  mutate(fe_intake_old = fe_mg_old*quantity_100g,
         fe_intake_new = fe_mg*quantity_100g) %>% 
  group_by(hhid) %>% 
  summarise(
    fe_ai_old = sum(fe_intake_old,na.rm = T),
    fe_ai_new = sum(fe_intake_new,na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    prev_old = ifelse(fe_ai_old<22.4,1,0),
    prev_new = ifelse(fe_ai_new<22.4,1,0)
  ) %>% 
  left_join(ind_hh_info %>% 
              select(hhid,survey_wgt), by = "hhid") %>% 
  srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::summarise(
    prev_old = (survey_mean(prev_old == 1, proportion = TRUE )) * 100,
    prev_new = (survey_mean(prev_new == 1, proportion = TRUE)) * 100)




