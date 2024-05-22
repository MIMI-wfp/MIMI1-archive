###

library(tidyverse)
library(srvyr)
library(readr)
library(dplyr)
library(ggridges)

source(here::here("all_base_models/scripts/base_model_functions.R"))
source(here::here("dietary_assessment/processing/individual_level_clean.R"))

ess1819 <- apparent_intake("eth_ess1819")
ess1819_all_items <- full_item_list("eth_ess1819")

#harmonised average requirements
allen_har <- data.frame(
  energy_kcal = 2200,#who
  vita_rae_mcg  = 490, 
  thia_mg = 0.9,
  ribo_mg = 1.3, 
  niac_mg = 11, 
  vitb6_mg = 1.3, 
  folate_mcg = 250, 
  vitb12_mcg = 2, 
  fe_mg_low = 22.4, #low absorption
  fe_mg_mod = 9.6,
  fe_mg_high = 7,
  ca_mg = 860, 
  zn_mg_su = 8.9,#semi unrefined
  zn_mg_u = 10.2
)



ess1819_binary_adequacy <- ess1819 %>% 
  select(hhid, vita_rae_mcg, folate_mcg, vitb12_mcg,
         fe_mg, zn_mg) %>% 
  mutate(vita = ifelse(vita_rae_mcg< allen_har$vita_rae_mcg,0, 1 ),
         folate = ifelse(folate_mcg < allen_har$folate_mcg, 0, 1),
         vitb12 = ifelse(vitb12_mcg < allen_har$vitb12_mcg, 0, 1),
         fe = ifelse(fe_mg < allen_har$fe_mg_low, 0, 1),
         zn = ifelse(zn_mg < allen_har$zn_mg_u, 0, 1)) %>% 
  select(-c(vita_rae_mcg, folate_mcg, vitb12_mcg,
         fe_mg, zn_mg))

write.csv(ess1819_binary_adequacy, "data_requests/fraym/ess_binary.csv")  

