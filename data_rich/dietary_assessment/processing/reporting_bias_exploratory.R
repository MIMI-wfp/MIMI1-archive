### quick analysis for bias paper
library(tidyverse)
library(srvyr)
library(readr)
library(dplyr)
library(ggridges)
library(treemap)
library(treemapify)
library(hrbrthemes)

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))
source(here::here("data_rich/dietary_assessment/processing/individual_level_clean.R"))


#read in base models

nga1819 <- apparent_intake("nga_lss1819") 
nga1819_all_items <- full_item_list("nga_lss1819")


nsso1112<- apparent_intake("ind_nss1112")
nsso1112_all_items <- full_item_list("ind_nss1112")
nsso1112_hh_info <- household_data("ind_nss1112")

ess1819 <- apparent_intake("eth_ess1819")
ess1819_all_items <- full_item_list("eth_ess1819")

mwi1617 <- apparent_intake("mwi_ihs1617")
mwi1617 <- full_item_list("mwi_ihs1617")

hices1516 <- apparent_intake("eth_hices1516")
hices_hh_info <- household_data("eth_hices1516")


hices1516_all_items <- full_item_list("eth_hices1516")

#### Overall intake ############################################################

## Ethiopia 

ethiopia_energy <- hices1516 %>% 
  select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
         vitb12_mcg) %>% 
  filter(energy_kcal<5000) %>% 
  mutate(survey = "hices") %>% 
  # bind_rows(
  #   ess1819 %>% 
  #     select(hhid, energy_kcal, vita_rae_mcg, folate_mcg,fe_mg, zn_mg,
  #            vitb12_mcg) %>% 
  #     filter(energy_kcal<5000) %>% 
  #     mutate(survey = "ess") 
  # ) %>% 
  bind_rows(
    fcs11_women %>% 
      mutate(hhid = paste0(CLUSTER,HHNO,SUBJECT),
             survey = "fcs") %>% 
      select(hhid, ENERGY_KCAL_sum,
             VITA_sum,
             IRON_MG_sum,
             ZINC_MG_sum, 
             FOL_sum,
             VITB12_sum,
             survey) %>% 
      rename(energy_kcal = ENERGY_KCAL_sum,
             vita_rae_mcg = VITA_sum,
             fe_mg =IRON_MG_sum,
             zn_mg = ZINC_MG_sum,
             folate_mcg = FOL_sum,
             vitb12_mcg = VITB12_sum) 
  ) 




ethiopia_energy %>% 
  ggplot(aes(x = energy_kcal, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported energy intake",
    x = "Energy (kcal)"
  )

ethiopia_energy %>% 
  filter(vita_rae_mcg<1000) %>% 
  ggplot(aes(x = vita_rae_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "Ethiopia reported vitamin A intake",
    x = "Vitmain A RAE (mcg)"
  )
  

ethiopia_energy %>% 
  ggplot(aes(x = fe_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported iron intake",
    x = "Iron (mg)"
  )

ethiopia_energy %>% 
  ggplot(aes(x = zn_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported zinc intake",
    x = "Zinc (mg)"
  )

ethiopia_energy %>% 
  filter(folate_mcg<1200) %>% 
  ggplot(aes(x = folate_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported folate intake",
    x = "Folate (mcg)"
  )

ethiopia_energy %>% 
  filter(folate_mcg<5) %>% 
  ggplot(aes(x = vitb12_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(
    title = "Ethiopia reported vitamin B12 intake",
    x = "Vitamin B12 (mcg)"
  )


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
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported energy intake",
    x = "Energy (kcal)"
  )

india_energy %>% 
  filter(vita_rae_mcg<1000) %>% 
  ggplot(aes(x = vita_rae_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported vitamin A intake",
    x = "Vitmain A RAE (mcg)"
  )

india_energy %>% 
  filter(fe_mg<50) %>% 
  ggplot(aes(x = fe_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported iron intake",
    x = "Iron (mg)"
  )


india_energy %>% 
  filter(zn_mg<25) %>% 
  ggplot(aes(x = zn_mg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported zinc intake",
    x = "Zinc (mg)"
  )

india_energy %>% 
  filter(vitb12_mcg<5) %>%
  ggplot(aes(x = vitb12_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported vitamin B12 intake",
    x = "Vitmain B12 (mcg)"
  )

india_energy %>% 
  filter(folate_mcg<1000) %>%
  ggplot(aes(x = folate_mcg, fill = survey, y = survey))+
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges()+
  labs(
    title = "India reported folate intake",
    x = "Folate (mcg)"
  )



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
  summarise(consumed_rice = max(consumed_rice)) %>% 
  ungroup() %>% 
  # summarise(consumed_rice) %>% 
  # srvyr::as_survey_design(weights = survey_wgt) %>% 
  # dplyr::group_by(adm1) %>% 
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
hices_mdd_w <- read.csv(here::here("data_rich/dietary_assessment/data/hices_mdd_w.csv"))
hices_mdd_w_tubers <- read.csv(here::here("data_rich/dietary_assessment/data/hices_mdd_w_tubers.csv"))
ess_mdd_w <- read.csv(here::here("data_rich/dietary_assessment/data/ess_mdd_w.csv"))
nnmb_mdd_w <- read.csv(here::here("data_rich/dietary_assessment/data/nnmb_mdd_w.csv"))
nsso_mdd_w <- read.csv(here::here("data_rich/dietary_assessment/data/nsso_mdd_w.csv"))
fcs_mdd_w <- read.csv(here::here("data_rich/dietary_assessment/data/fcs_mdd_w.csv"))

###### food group analyses #### 
# defining it based on MDD-W because of Vit A food groups and ASF


sum(hices_mdd_w$green_veg,na.rm = T)
sum(hices_mdd_w$vita_fruit_veg,na.rm = T)
sum(hices_mdd_w$cereals,na.rm = T)

sum(fcs_mdd_w$green_veg,na.rm = T)
sum(fcs_mdd_w$vita_fruit_veg,na.rm = T)
sum(fcs_mdd_w$cereals,na.rm = T)


sum(nsso_mdd_w$green_veg,na.rm = T)
sum(nsso_mdd_w$vita_fruit_veg,na.rm = T)
sum(nsso_mdd_w$cereals,na.rm = T)

sum(nnmb_mdd_w$green_veg,na.rm = T)
sum(nnmb_mdd_w$vita_fruit_veg,na.rm = T)
sum(nnmb_mdd_w$cereals,na.rm = T)

#leafy green veg - ETH
hices_green_quantity <- hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(green_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<1500) %>% 
  mutate(survey = "hices")
# %>% 
  # ggplot(aes(x = quantity_consumed))+ 
  # geom_histogram()+
  # ggtitle("HICES leafy greens") +
  # xlab("Quantity (g)")+
  # xlim(-100,1500)

nsso_green_quantity <- nsso1112_all_items %>% 
  left_join(nsso_mdd_w, by = "item_code") %>% 
  filter(green_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<1500) %>% 
  mutate(survey = "nsso")



nnmb_green_quantity<- nnmb_all_items %>% 
  left_join(nnmb_mdd_w) %>% 
  filter(green_veg == 1) %>% 
  group_by(SUBJECT) %>% 
  summarise(quantity_consumed = sum(FOOD_AMOUNT_REPORTED)) %>% 
  mutate(survey = "nnmb")


bind_rows(nsso_green_quantity,nnmb_green_quantity) %>% 
  filter(quantity_consumed<100) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "India: Leafy greens consumed",
       x = "Quantity of food group consumed (g)")



nsso_cereals_quantity <- nsso1112_all_items %>% 
  left_join(nsso_mdd_w, by = "item_code") %>% 
  filter(cereals == 1 & quantity_g>0) %>% 
  
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<1500) %>% 
  mutate(survey = "nsso")



nnmb_cereals_quantity<- nnmb_all_items %>% 
  left_join(nnmb_mdd_w) %>% 
  filter(cereals == 1) %>% 
  filter(cereals == 1 & FOOD_AMOUNT_REPORTED>0) %>% 
  group_by(SUBJECT) %>% 
  summarise(quantity_consumed = sum(FOOD_AMOUNT_REPORTED)) %>% 
  mutate(survey = "nnmb")


bind_rows(nnmb_cereals_quantity,nsso_cereals_quantity) %>% 
  filter(quantity_consumed<1000) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "India: Cereals consumed",
       x = "Quantity of food group consumed (g)")


nsso_vita_quantity <- nsso1112_all_items %>% 
  left_join(nsso_mdd_w, by = "item_code") %>% 
  filter(vita_fruit_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<300) %>% 
  mutate(survey = "nsso")



nnmb_vita_quantity<- nnmb_all_items %>% 
  left_join(nnmb_mdd_w) %>% 
  filter(vita_fruit_veg == 1) %>% 
  group_by(SUBJECT) %>% 
  summarise(quantity_consumed = sum(FOOD_AMOUNT_REPORTED)) %>%
  filter(quantity_consumed<300) %>% 
  mutate(survey = "nnmb")


bind_rows(nsso_vita_quantity,nnmb_vita_quantity) %>% 
  filter(quantity_consumed<100) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "India: Vitamin A-rich fruits/vegetables consumed",
       x = "Quantity of food group consumed (g)")



fcs_women_greens <- fcs11_women %>% 
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
  mutate(survey = "fcs")
# %>% 
  # ggplot(aes(x = quantity_consumed))+ 
  # geom_histogram()+
  # ggtitle("FSS leafy greens") +
  # xlab("Quantity (g)") +
  # xlim(-100,1500)


bind_rows(fcs_women_greens,hices_green_quantity) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "Ethiopia: Leafy greens consumed",
       x = "Quantity of food group consumed (g)")


hices_cereal_quantity <- hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(cereals == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<3000) %>% 
  mutate(survey = "hices")
 

fcs_cereal_quantity <- fcs11_women %>% 
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
  mutate(survey = "fcs")
  
bind_rows(fcs_cereal_quantity,hices_cereal_quantity) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "Ethiopia: Cereals, roots and tubers reported",
       x = "Quantity of food group consumed (g)")


fcs_women_vita <- fcs11_women %>% 
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
  filter(vita_fruit_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(AMOUNT_GRAMS)) %>% 
  filter(quantity_consumed<200) %>% 
  mutate(survey = "fcs")

hices_vita_quantity <- hices1516_all_items %>% 
  left_join(hices_mdd_w, by = "item_code") %>% 
  filter(vita_fruit_veg == 1) %>% 
  group_by(hhid) %>% 
  summarise(quantity_consumed = sum(quantity_g)) %>% 
  filter(quantity_consumed<200) %>% 
  mutate(survey = "hices")


bind_rows(hices_vita_quantity,fcs_women_vita) %>% 
  ggplot(aes(x = quantity_consumed, fill = survey, y = survey))+ 
  geom_density_ridges(alpha=0.6, stat="density_ridges", bins=20,
                      quantile_lines = T, quantile_fun = median) +
  theme_ridges() + 
  labs(title = "Ethiopia: Vitamin A rich fruits/vegetables",
       x = "Quantity of food group consumed (g)")





#############################
#
# Plan
#
#############################
# We need to separate out some variables to look at 
# - single person recalling consumption for the entire household, (look at hh of house, education level, sex, age (potentially))
# - limited capture of food consumed outside of the home, 
# - variance coefficients in 24HR, 
# - intra household distribution in HCES


################################################################################
# Work for Kevin's EPHI paper

eth_micronutrient_food_groups <- hices1516_all_items %>% 
  dplyr::select(-c(food_group)) %>% 
  left_join(hices_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                       dairy,asf,eggs,green_veg,vita_fruit_veg,
                       other_veg,other_fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::group_by(hhid,food_group) %>% 
  mutate(across(-c(afe,item_code,item_name,quantity_g,value,quantity_100g),
                ~.x*quantity_100g/afe)) %>% 
  dplyr::summarise(
    dplyr::across(
      -c(afe,item_code,item_name,quantity_g,value,quantity_100g),
      ~sum(.x, na.rm = TRUE)
    )
  ) 


eth_micronutrient_food_groups <- eth_micronutrient_food_groups%>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() %>% 
  # dplyr::filter(
  #   energy_kcal<stats::quantile(energy_kcal, 0.99, na.rm = TRUE)[[1]]
  # ) %>% 
  dplyr::group_by(
    food_group
  ) %>% 
  dplyr::summarise(
    dplyr::across(
      -c(hhid),
      ~mean(.)
    )
  ) %>% 
  mutate(across(
    -c(food_group),
    ~.x/sum(.)*100,
    .names = "perc_{.col}"
    
  ))%>% 
  mutate(
    food_group = 
      case_when(
        food_group == "asf" ~ "Meat, poultry, fish",
        food_group == "cereals" ~ "Grains, white roots, tubers",
        food_group == "dairy" ~ "Dairy",
        food_group == 'eggs' ~ "Eggs",
        food_group == "green_veg" ~ "Dark green leafy veg",
        food_group == "nuts_seeds" ~ "Nuts and seeds",
        food_group == "other_fruit" ~ "Other fruits", 
        food_group == "other_veg" ~ "Other vegetables",
        food_group == "pulses" ~ "Pulses",
        food_group == "vita_fruit_veg" ~ "Other VA-rich fruits & veg",
        is.na(food_group)  ~ "Other"
      )
  ) %>% 
  rename(
    "Energy" = energy_kcal,
    "Vitamin A" = vita_rae_mcg,
    "Thiamine" = thia_mg,
    "Riboflavin" = ribo_mg,
    "Niacin" = niac_mg,
    "Vitamin B6" = vitb6_mg,
    "Folate" = folate_mcg,
    "Vitamin B12" = vitb12_mcg,
    "Zinc" = zn_mg
  )

micronutrients <- colnames(eth_micronutrient_food_groups)
micronutrients <- micronutrients[c(2:6,8:10,15)]



mn_fg_plots <- list()
for(item in micronutrients){
  print(item)
  p1 <-  eth_micronutrient_food_groups %>% 
    ggplot(aes(area = !!sym(item), 
               fill = food_group,
                 
               label = 
                 # stringr::str_to_title(" ")
                 
                   food_group,
                                        
                 
    ))+
    geom_treemap() +
    geom_treemap_text( colour = "darkblue", place = "topleft", alpha = 0.6,
                       grow = FALSE,min.size = 9, size = 12)+
    labs(title = gsub("_"," ",item),
           stringr::str_to_title(stringr::str_split_i(item,
                                                      "\\_",
                                                      1)),
         
    )+
    scale_fill_brewer(palette = "Set3")+
    # guides(fill=guide_legend())+
    theme(legend.position="bottom",
          legend.spacing.x = unit(0, 'cm'))+
    guides(fill = guide_legend(title="Food group",label.position = "bottom"))
  # theme(legend.direction = "horizontal", legend.position = "bottom")+
  # guides(fill = "none")+
  theme_ipsum()
  mn_fg_plots[[item]] <- p1
}
ggpubr::ggarrange(plotlist = mn_fg_plots, common.legend = TRUE)

food_group_perc <- eth_micronutrient_food_groups %>% 
  select(food_group,starts_with("perc"))
write_csv(food_group_perc,"eth_foodgroup_perc.csv")

hices_mdd_w

#-------------------------------------------------------------------------------
#create reach and quantity estiamtes for each household and food group
reach_food_group <- hices1516_all_items %>% 
  # dplyr::filter(quantity_g>0) %>% 
  dplyr::select(-c(food_group)) %>% 
  left_join(hices_mdd_w %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other_veg,other_fruit)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::select(hhid, afe, item_code,quantity_g, food_group) %>% 
  dplyr::group_by(hhid,food_group) %>% 
  mutate(quantity_afe = quantity_g) %>% 
  dplyr::summarise(
     qunatity_afe = sum(quantity_afe, na.rm = T)
  ) %>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() 



reach_food_group %>% 
  dplyr::left_join(hices_hh_info %>% 
                     dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                     dplyr::filter(!is.na(res_quintile)),
                   by = "hhid") %>% 
  dplyr::filter(food_group == "cereals") %>% 
  dplyr::mutate(reached = ifelse(qunatity_afe>0, "Yes", "No")) %>% 
  dplyr::group_by(
    food_group, res, res_quintile
  ) %>% 
  dplyr::summarise(
    median_reach = median(qunatity_afe),
    reach = sum(reached == "Yes")/n(),
    n()
  ) 


#create reach and quantity estiamtes for each household and food group

reach_food_group_tubers <- hices1516_all_items %>% 
  # dplyr::filter(quantity_g>0) %>%
  dplyr::select(-c(food_group)) %>% 
  left_join(hices_mdd_w_tubers %>% 
              tidyr::pivot_longer(cols = c(cereals,pulses,nuts_seeds,
                                           dairy,asf,eggs,green_veg,vita_fruit_veg,
                                           other_veg,other_fruit,tubers)) %>%
              
              dplyr::filter(!is.na(value)) %>%
              dplyr::select(-value) %>%
              dplyr::rename(food_group = name),by = "item_code") %>% 
  dplyr::select(hhid, afe, item_code,quantity_g, food_group) %>% 
  dplyr::group_by(hhid,food_group) %>% 
  mutate(quantity_afe = quantity_g) %>% 
  dplyr::summarise(
    qunatity_afe = sum(quantity_afe, na.rm = T)
  ) %>% 
  #calculate the per afe consumption to play with the data
  dplyr::ungroup() 

# plot the distributions

reach_food_group %>% 
  dplyr::filter(food_group == "cereals") %>% 
  dplyr::left_join(hices_hh_info %>% 
                     dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                     dplyr::filter(!is.na(res_quintile)),
                   by = "hhid") %>% 
  ggplot(aes(x = qunatity_afe))+
  geom_histogram()+
    facet_wrap(facets = vars(res, res_quintile), ncol = 5)

reach_food_group_tubers %>% 
  dplyr::filter(food_group == "tubers") %>% 
  dplyr::left_join(hices_hh_info %>% 
                     dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                     dplyr::filter(!is.na(res_quintile)),
                   by = "hhid") %>% 
  filter(qunatity_afe>0) %>% 
  ggplot(aes(x = qunatity_afe))+
  geom_histogram()+
  xlim(0,2000)+
  ylim(0,300)+
  facet_wrap(facets = vars(res, res_quintile), ncol = 5)

# create summary tables for both roots and tubers

x <- reach_food_group_tubers %>% 
  dplyr::filter(qunatity_afe>0) %>% 
  dplyr::left_join(hices_hh_info %>% 
                     dplyr::distinct(hhid, .keep_all = TRUE) %>% 
                     dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                     dplyr::filter(!is.na(res_quintile)),
                   by = "hhid") %>% 
  dplyr::filter(food_group == "cereals"|food_group == "tubers") %>% 
  dplyr::mutate(reached = ifelse(qunatity_afe>0, "Yes", "No")) %>% 
  dplyr::group_by(
    food_group,res,res_quintile
  ) %>% 
  dplyr::summarise(
    q25 = quantile(qunatity_afe,probs = seq(0, 1, 0.25))[2],
    median_intake_g= median(qunatity_afe,na.rm = T),
    q75 = quantile(qunatity_afe,probs = seq(0, 1, 0.25))[4],
    IQR = quantile(qunatity_afe,probs = seq(0, 1, 0.25))[4] - quantile(qunatity_afe,probs = seq(0, 1, 0.25))[2],
    number_consumers = n()
  ) 

tubers_csv <- x %>% 
  dplyr::left_join(reach_food_group_tubers %>% 
  dplyr::left_join(hices_hh_info %>% 
                     distinct(hhid, .keep_all =TRUE) %>% 
                     dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                     dplyr::filter(!is.na(res_quintile)),
                   by = "hhid") %>% 
  dplyr::filter(food_group == "cereals"|food_group == "tubers") %>% 
  dplyr::mutate(reached = ifelse(qunatity_afe>0, "Yes", "No")) %>% 
  dplyr::group_by(
    food_group,res,res_quintile
  ) %>% 
  dplyr::summarise(
    reach = sum(reached == "Yes")/n()
  ), by = c("food_group","res","res_quintile") )

write_csv(tubers_csv,"~/Documents/MIMI/eth_cereals_tubers_reach_dose_20240206.csv")

# now combining roots and tubers!
y <- reach_food_group %>% 
  dplyr::filter(qunatity_afe>0) %>% 
  dplyr::left_join(hices_hh_info %>% 
                     dplyr::distinct(hhid, .keep_all = TRUE) %>% 
                     dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                     dplyr::filter(!is.na(res_quintile)),
                   by = "hhid") %>% 
  dplyr::filter(food_group == "cereals") %>% 
  dplyr::mutate(reached = ifelse(qunatity_afe>0, "Yes", "No")) %>% 
  dplyr::group_by(
    food_group, res, res_quintile
  ) 
# %>% 
#   dplyr::summarise(
#     q25 = quantile(qunatity_afe,probs = seq(0, 1, 0.25))[2],
#     median_intake_g= median(qunatity_afe,na.rm = T),
#     q75 = quantile(qunatity_afe,probs = seq(0, 1, 0.25))[4],
#     IQR = quantile(qunatity_afe,probs = seq(0, 1, 0.25))[4] - quantile(qunatity_afe,probs = seq(0, 1, 0.25))[2]
#   ) 

cereals_csv <- y %>% 
  dplyr::left_join(reach_food_group_tubers %>% 
                     dplyr::left_join(hices_hh_info %>% 
                                        dplyr::distinct(hhid, .keep_all = TRUE) %>% 
                                        dplyr::select(hhid, res, res_quintile,sep_quintile) %>% 
                                        dplyr::filter(!is.na(res_quintile)),
                                      by = "hhid") %>% 
                     dplyr::filter(food_group == "cereals") %>% 
                     dplyr::mutate(reached = ifelse(qunatity_afe>0, "Yes", "No")) %>% 
                     dplyr::group_by(
                       food_group,
                     ) %>% 
                     dplyr::summarise(
                       reach = sum(reached == "Yes")/n(),
                       n()
                     ), by = c("food_group")) 
# %>% 
  # filter(!is.na(res))

write_csv(cereals_csv,"~/Documents/MIMI/eth_cereals_reach_dose_20240206.csv")

