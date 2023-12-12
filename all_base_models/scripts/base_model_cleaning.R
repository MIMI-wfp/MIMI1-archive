### make all countries with the same structure
### MIMI
### 12-12-2023

library(tidyr)
library(readr)
library(dplyr)

path_to_data <- "~/Documents/MIMI/MIMI_data/"
path_to_save <- here::here("all_base_models/data/")

# nsso #########################################################################

nsso_food_consumption <- read.csv(paste0(path_to_data, "India/India_NSSO_2012/india_daily_consumption.csv"))

nsso_food_consumption <- nsso_food_consumption %>% 
  rename(
    hhid = HHID,
    item_code = Item_Code,
    quantity_g = Total_Consumption_Quantity,
    value = Total_Consumption_Value,
    food_group = hdds_groups
  ) %>% 
  select(hhid, item_code, quantity_g, value, food_group) %>% 
  mutate(quantity_100g = quantity_g/100)


write_csv(nsso_food_consumption, paste0(path_to_save,"nsso1112_food_consumption.csv"))


nsso_fct <- read.csv(paste0(path_to_data, "India/India_NSSO_2012/india_matched_fct.csv"))

nsso_fct <- nsso_fct %>% 
  rename(
    item_code = Item_Code,
    vita_rae_mcg = vita_mg,
    folate_mcg = folate_ug,
    vitb12_mcg = vitaminb12_in_mg,
    fe_mg = iron_mg,
    ca_mg = calcium_mg,
    zn_mg = zinc_mg,
    na_mg = sodium_mg,
    thia_mg = vitb1_mg,
    ribo_mg = vitb2_mg,
    niac_mg = vitb3_mg
  )


write_csv(nsso_fct, paste0(path_to_save,"nsso1112_fct.csv"))




# Ethiopia #####################################################################
# HICES ------------------------------------------------------------------------

hices_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/eth_hces1516_foodbev.csv"))

hices_food_consumption
hices_food_consumption <- hices_food_consumption %>% 
  rename(
    item_code = ITEMC,
    quantity_g = QUANTITY,
    value = VALUE,
    food_group = TOP4
  ) %>% 
  select(hhid, item_code, quantity_g, value, food_group) %>% 
  mutate(quantity_g = quantity_g/365,
         value = value/365) %>% 
  mutate(quantity_100g = quantity_g/100)

write_csv(hices_food_consumption, paste0(path_to_save,"hices1516_food_consumption.csv"))

# fct 

hices_fct <- read.csv(paste0(path_to_data, "Ethiopia/eth/hices1516/ETH_HCES1516_fctmatch.csv"))

names(hices_fct)

hices_fct <- hices_fct %>% 
  select(ITEMC,
         energy_in_kcal,
         vitamina_in_rae_in_mcg,
         thiamin_in_mg,
         riboflavin_in_mg,
         niacin_in_mg,
         # pantothenate_in_mg,
         vitamind_in_mcg,
         vitaminb6_in_mg,
         folate_in_mcg,
         vitaminb12_in_mcg,
         vitaminc_in_mg,
         ca_in_mg,
         fe_in_mg,
         se_in_mcg,
         zn_in_mg
         ) %>% 
  rename(
    item_code = ITEMC,
    energy_kcal = energy_in_kcal,
    vita_rae_mcg = vitamina_in_rae_in_mcg,
    thia_mg = thiamin_in_mg,
    ribo_mg = riboflavin_in_mg,
    niac_mg = niacin_in_mg,
    vitb6_mg = vitaminb6_in_mg,
    vitd_mcg = vitamind_in_mcg,
    folate_mcg = folate_in_mcg,
    vitb12_mcg = vitaminb12_in_mcg,
    vitc_mg = vitaminc_in_mg,
    ca_mg = ca_in_mg,
    fe_mg = fe_in_mg,
    se_mcg = se_in_mcg,
    zn_mg = zn_in_mg
  )

write_csv(hices_fct, paste0(path_to_save,"hices1516_fct.csv"))

rm(hices_fct)
rm(hices_food_consumption)
# ESS --------------------------------------------------------------------------


ess_food_consumption <- read.csv(paste0(path_to_data, "Ethiopia/eth/ess41819/eth_ess4_food_cons.csv"))

ess_food_consumption
ess_food_consumption <- ess_food_consumption %>% 
  filter(g_d_nep>0) %>% 
  rename(
    hhid = HHID,
    item_code = fcode,
    quantity_g = g_d_nep,
    quantity_100g = g100_d_nep,
    # value = VALUE,
    food_group = foodgroup_hhdd_ext
  ) %>% 
  select(hhid, item_code, quantity_g,quantity_100g, food_group) 


write_csv(ess_food_consumption, paste0(path_to_save,"ess1819_food_consumption.csv"))

# fct 

ess_fct <- read.csv(paste0(path_to_data, "Ethiopia/eth/ess41819/eth_ess4_fct_full_complete.csv"))

names(ess_fct)

ess_fct <- ess_fct %>% 
  select(fcode,
         ref_fooditem,
         energy_in_kcal,
         vitamina_in_rae_in_mcg,
         thiamin_in_mg,
         riboflavin_in_mg,
         niacin_in_mg,
         pantothenate_in_mg,
         # vitamind_in_mcg,
         vitaminb6_in_mg,
         folate_in_mcg,
         vitaminb12_in_mcg,
         vitaminc_in_mg,
         ca_in_mg,
         fe_in_mg,
         se_in_mcg,
         zn_in_mg
  ) %>% 
  rename(
    item_code = fcode,
    item_name = ref_fooditem,
    energy_kcal = energy_in_kcal,
    vita_rae_mcg = vitamina_in_rae_in_mcg,
    thia_mg = thiamin_in_mg,
    ribo_mg = riboflavin_in_mg,
    niac_mg = niacin_in_mg,
    vitb6_mg = vitaminb6_in_mg,
    vitb5_mg = pantothenate_in_mg,
    folate_mcg = folate_in_mcg,
    vitb12_mcg = vitaminb12_in_mcg,
    vitc_mg = vitaminc_in_mg,
    ca_mg = ca_in_mg,
    fe_mg = fe_in_mg,
    se_mcg = se_in_mcg,
    zn_mg = zn_in_mg
  )

write_csv(ess_fct, paste0(path_to_save,"ess1819_fct.csv"))

rm(ess_fct)
rm(ess_food_consumption)

# MWI ##########################################################################

mwi_base_model<- read.csv(paste0(path_to_data, "mwi/hh_mod_g_final.csv"))

mwi_food_consumption <- mwi_base_model %>% 
  select(
     item_code,
     HHID,
     g100_d_nep,
     food.group
     ) %>% 
  rename(
    hhid = HHID,
    quantity_100g = g100_d_nep,
    food_group = food.group
  ) %>% 
  mutate(
    quantity_g = quantity_100g*100
  ) %>% 
  filter(
    quantity_g>0
  )


write_csv(mwi_food_consumption, paste0(path_to_save,"mwi1516_food_consumption.csv"))


mwi_fct <- mwi_base_model %>% 
  select(-c(
    X,
    HHID,
    g100_d_nep,
    food.group
  )) %>% 

  distinct(item_code, .keep_all = TRUE) %>% 
  rename(
    energy_kcal = energy.kcal,
    vita_rae_mcg = vitarae.mcg,
    thia_mg = thia.mg,
    ribo_mg = ribo.mg,
    niac_mg = niac.mg,
    vitb6_mg = vitb6.mg,
    folate_mcg = fol.mcg, 
    vitb12_mcg = vb12.mcg,
    se_mcg = se.mcg,
    vitc_mg = vc.mg, 
    ca_mg = ca.mg, 
    fe_mg = fe.mg,
    zn_mg = zn.mg
  ) %>% 
  select(
    item_code,
    energy_kcal,
    vita_rae_mcg,
    thia_mg,
    ribo_mg,
    niac_mg,
    vitb6_mg,
    folate_mcg, 
    vitb12_mcg,
    se_mcg,
    vitc_mg, 
    ca_mg, 
    fe_mg,
    zn_mg
  )
names(mwi_fct)

# fct 


write_csv(mwi_fct, paste0(path_to_save,"mwi1516_fct.csv"))

rm(mwi_fct)
rm(mwi_food_consumption)

# nga ##########################################################################

nga_food_consumption<- read.csv(paste0(path_to_data, "nga/sect6b_food_cons_final.csv"))

nga_food_consumption <- nga_food_consumption %>% 
  select(
    hhid,
    item_code,
    g100_d_nep,
    food_group
  ) %>% 
  rename(
    quantity_100g = g100_d_nep,
  ) %>% 
  mutate(
    quantity_g = quantity_100g*100
  ) %>% 
  filter(
    quantity_g>0
  )


write_csv(nga_food_consumption, paste0(path_to_save,"nga1819_food_consumption.csv"))


nga_fct <- read.csv(paste0(path_to_data, "nga/fct_nga_v4.0_full.csv"))


nga_fct <- nga_fct %>% 
  select(
   item_code,
   energy_in_kcal,
   vitamina_in_rae_in_mcg,
   thiamine_in_mg,
   riboflavin_in_mg,
   niacin_equivalent_in_mg,
   vitaminb6_in_mg,
   vitamind_in_mcg,
   folate_total_in_mcg,
   vitaminb12_in_mcg,
   vitaminc_in_mg,
   ca_in_mg,
   fe_in_mg,
   zn_in_mg
  ) %>% 
  rename(
    energy_kcal = energy_in_kcal,
    vita_rae_mcg = vitamina_in_rae_in_mcg,
    thia_mg = thiamine_in_mg,
    ribo_mg = riboflavin_in_mg,
    niac_mg = niacin_equivalent_in_mg,
    vitb6_mg = vitaminb6_in_mg,
    vitd_mcg = vitamind_in_mcg,
    folate_mcg = folate_total_in_mcg,
    vitb12_mcg = vitaminb12_in_mcg,
    vitc_mg = vitaminc_in_mg,
    ca_mg = ca_in_mg,
    fe_mg = fe_in_mg,
    zn_mg = zn_in_mg
  ) %>% 
  filter(!is.na(item_code))
names(mwi_fct)

# fct 


write_csv(nga_fct, paste0(path_to_save,"nga1819_fct.csv"))

rm(nga_fct)
rm(nga_food_consumption)
