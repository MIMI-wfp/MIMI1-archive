### quick analysis for bias paper
library(tidyverse)
library(srvyr)
library(readr)
library(dplyr)
library(ggridges)

source(here::here("all_base_models/scripts/base_model_functions.R"))
source(here::here("dietary_assessment/processing/individual_level_clean.R"))


#read in base models

nga1819 <- apparent_intake("nga1819")
nga1819_all_items <- full_item_list("nga1819")

nsso1112<- apparent_intake("nsso1112")
nsso1112_all_items <- full_item_list("nsso1112")
nsso1112_hh_info <- household_data("nsso1112")

ess1819 <- apparent_intake("ess1819")
ess1819_all_items <- full_item_list("ess1819")

mwi1516 <- apparent_intake("mwi1516")
nsso1112_all_items <-  full_item_list("nsso1112")

hices1516 <- apparent_intake("hices1516")
hices_hh_info <- household_data("hices1516")
hices1516 <- hices1516 %>% 
  left_join(hices_hh_info, by = "hhid")

hices1516_all_items <- full_item_list("hices1516")

#### Overall intake ############################################################

## Ethiopia 

ethiopia_energy <- hices1516 %>% 
  select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
         vitb12_mcg) %>% 
  filter(energy_kcal<5000) %>% 
  mutate(survey = "hices") %>% 
  bind_rows(
    ess1819 %>% 
      select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
             vitb12_mcg) %>% 
      filter(energy_kcal<5000) %>% 
      mutate(survey = "ess") 
  ) %>% 
  bind_rows(
    fcs11_women %>% 
      mutate(hhid = paste0(CLUSTER,HHNO,SUBJECT),
             survey = "fcs") %>% 
      select(hhid, ENERGY_KCAL_sum,
             VITA_sum,
             IRON_MG_sum,
             ZINC_MG_sum, 
             survey) %>% 
      rename(energy_kcal = ENERGY_KCAL_sum,
             vita_rae_mcg = VITA_sum,
             fe_mg =IRON_MG_sum,
             zn_mg = ZINC_MG_sum)  
    
  ) 


ethiopia_energy %>% 
  ggplot(aes(x = energy_kcal, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

ethiopia_energy %>% 
  filter(vita_rae_mcg<1000) %>% 
  ggplot(aes(x = vita_rae_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

ethiopia_energy %>% 
  ggplot(aes(x = fe_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()


ethiopia_energy %>% 
  ggplot(aes(x = zn_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

### we see some clear lower intake values in ethiopia for the individual level
### recall than the household consumption




## India

india_energy <- nsso1112 %>% 
  select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
         vitb12_mcg) %>% 
  mutate(survey = "nsso") %>% 
  bind_rows(
    nnmb_women %>% 
      select(SUBJECT,
             ENERGY_kcal,
             VITA_RAE_mcg,
             IRON_mg,
             ZINC_mg,
             FOLDFE_mcg,
             VITB12_mcg) %>% 
      rename(
        hhid = SUBJECT,
        energy_kcal = ENERGY_kcal,
        vita_rae_mcg = VITA_RAE_mcg,
        fe_mg = IRON_mg,
        zn_mg = ZINC_mg,
        folate_mcg = FOLDFE_mcg,
        vitb12_mcg = VITB12_mcg
      ) %>% 
      mutate(survey = "nnmb") 
  )

india_energy %>% 
  filter(energy_kcal<5000) %>% 
  ggplot(aes(x = energy_kcal, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

india_energy %>% 
  filter(vita_rae_mcg<1000) %>% 
  ggplot(aes(x = vita_rae_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

india_energy %>% 
  filter(fe_mg<50) %>% 
  ggplot(aes(x = fe_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()


india_energy %>% 
  filter(zn_mg<25) %>% 
  ggplot(aes(x = zn_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

india_energy %>% 
  filter(vitb12_mcg<20) %>%
  ggplot(aes(x = vitb12_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

india_energy %>% 
  filter(folate_mcg<1000) %>%
  ggplot(aes(x = folate_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()



## look at coverage of items----------------------------------------------------

# rice - India

nsso1112_all_items %>% 
  filter(item_code == 101 |
           item_code == 102 |
           item_code == 106
         ) %>% 
  select(hhid,
         item_code,
         quantity_g) %>% 
  mutate(consumed_rice = ifelse(quantity_g > 0, 1, 0)) %>% 
  ungroup() %>%
  left_join(nsso1112_hh_info, by = "hhid") %>% 
  group_by(hhid) %>% 
  
  # summarise(consumed_rice) %>% 
  # srvyr::as_survey_design(weights = survey_wgt) %>% 
  dplyr::group_by(adm1) %>% 
  dplyr::summarise(consumed_rice = sum(consumed_rice)/n())

nnmb_all_items %>% 
  filter(FOODEX2_INGR_CODE == "A001D#F10.A07XK") %>% 
  mutate(consumed_rice = ifelse(FOOD_AMOUNT_REPORTED>0, 1,0)) %>% 
  left_join(nnmb_subject %>% select(SUBJECT, ADM1_NAME), 
            by = "SUBJECT") %>% 
  group_by(ADM1_NAME) %>% 
  summarise(rice_percentage = sum(consumed_rice)/n())

# wheat flour - Ethiopia

fcs11_adults %>% 
  mutate(
    subject_id = paste0(CLUSTER,HHNO, SUBJECT)
  ) %>% 
  group_by(subject_id) %>% 
  select(subject_id, WHEAT_G_sum,REGION) %>% 
  ungroup() %>% 
  mutate(wheat = ifelse(WHEAT_G_sum>0,1,0)) %>% 
  # group_by(REGION) %>% 
  summarise(coverage = sum(wheat)/n(),
            se = sqrt(coverage*(1-coverage)/n()))

hices1516_all_items %>% 
  filter(grepl("wheat",item_code)) %>% 
  group_by(hhid) %>% 
  summarise(total_wheat = sum(quantity_g)) %>% 
  mutate(consumed_wheat = ifelse(total_wheat>0,1,0)) %>% 
  summarise(coverage = sum(consumed_wheat)/n(),
            se = sqrt(coverage*(1-coverage)/n()))

# edible oil - Ethiopia
# are butter/other fats fortifiable?

fcs11_adults %>% 
  mutate(
    subject_id = paste0(CLUSTER,HHNO, SUBJECT)
  ) %>% 
  group_by(subject_id) %>% 
  select(subject_id, OIL_G_sum,REGION) %>% 
  ungroup() %>% 
  mutate(oil = ifelse(OIL_G_sum>0,1,0)) %>% 
  # group_by(REGION) %>% 
  summarise(oil_consumed = sum(oil)/n())

hices1516_all_items %>% 
  filter(grepl("Edible oil",item_code)) %>% 
  group_by(hhid) %>% 
  summarise(total_oil = sum(quantity_g)) %>% 
  mutate(consumed_oil = ifelse(total_oil>0,1,0)) %>% 
  summarise(coverage = sum(consumed_oil)/n(),
            se = sqrt(coverage*(1-coverage)/n()))

###
### we are getting different coverage of consumption between types of survey
### However, this is not the positive direction i was exepcting of longer recall
### resulting in higher consumption. It is more likely to do with the food item lists
### I'll use ESS to compare with HCES to see if there is a noticable difference
### between the coverage and food items list (among fortifiable items)

ess1819_all_items %>% 
  filter(grepl("Wheat", item_name)) %>% 
  group_by(hhid) %>% 
  summarise(total_wheat = sum(quantity_g)) %>% 
  mutate(consumed_wheat = ifelse(total_wheat>0,1,0)) %>% 
  summarise(coverage = sum(consumed_wheat)/n(),
            se = sqrt(coverage*(1-coverage)/n()))

ess1819_all_items %>% 
  filter(grepl("Oil", item_name)) %>% 
  group_by(hhid) %>% 
  summarise(total_oil = sum(quantity_g)) %>% 
  mutate(consumed_oil = ifelse(total_oil>0,1,0)) %>% 
  summarise(coverage = sum(consumed_oil)/n(),
              se = sqrt(coverage*(1-coverage)/n()))

# add in a food group variable for each survey from MDD-W
hices_mdd_w <- read.csv(here::here("dietary_assessment/data/hices_mdd_w.csv"))
ess_mdd_w <- read.csv(here::here("dietary_assessment/data/ess_mdd_w.csv"))
nnmb_mdd_w <- read.csv(here::here("dietary_assessment/data/nnmb_mdd_w.csv"))
nsso_mdd_w <- read.csv(here::here("dietary_assessment/data/nsso_mdd_w.csv"))
fcs_mdd_w <- read.csv(here::here("dietary_assessment/data/fcs_mdd_w.csv"))

###### food group analyses #### 
# defining it based on MDD-W because of Vit A food groups and ASF


#leafy green veg - ETH
hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(green_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<1500) %>% 
  ggplot(aes(x = quantity_consumed))+ 
  geom_histogram()

fcs11_women %>% 
  mutate(
    hhid = paste0(CLUSTER,HHNO, SUBJECT) 
    ) %>% 
  select(hhid) %>% 
  left_join(
    fcs11 %>% 
      mutate(hhid = paste0(CLUSTER,HHNO, SUBJECT))
  ) %>% 
  left_join(fcs_mdd_w %>% 
              select(-SHRT_DESC) %>% 
              distinct(CODE, .keep_all = TRUE), by = "CODE") %>% 
  filter(green_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(AMOUNT_GRAMS)) %>% 
  ggplot(aes(x = quantity_consumed))+ 
  geom_histogram()


hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(cereals == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<3000) %>%
  ggplot(aes(x = quantity_consumed))+ 
  geom_histogram() +
  xlim(0,3000)

fcs11_women %>% 
  mutate(
    hhid = paste0(CLUSTER,HHNO, SUBJECT) 
  ) %>% 
  select(hhid) %>% 
  left_join(
    fcs11 %>% 
      mutate(hhid = paste0(CLUSTER,HHNO, SUBJECT))
  ) %>% 
  left_join(fcs_mdd_w %>% 
              select(-SHRT_DESC) %>% 
              distinct(CODE, .keep_all = TRUE), by = "CODE") %>% 
  filter(cereals == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(AMOUNT_GRAMS)) %>% 
  ggplot(aes(x = quantity_consumed))+ 
  geom_histogram()+
  xlim(0,3000)


  




