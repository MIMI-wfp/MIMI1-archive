
#########################################
#          NAR and MAR Ethiopia          #
#########################################


# Author: Gabriel Battcock
# Created: 26 Aug 24
# Last updated: 

rq_packages <- c("tidyverse","dplyr","readr","srvyr","ggplot2",
                 "ggridges", "gt", "haven","foreign",
                 "tmap","sf","rmapshaper","readxl","hrbrthemes",
                 "wesanderson","treemap","treemapify")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))

# Read in data ----------------------------------------------------------------


eth_hices1516 <- apparent_intake("/eth_hices1516")
eth_hices1516_hh_info <- household_data("/eth_hices1516")
# nga_lga_dict <- read.csv("../nigeria_mapping/data_dictionary/lga.csv")
# hh_to_lga <-as_tibble(read.csv("../nigeria_mapping/hh_to_lga.csv"))
# eth_adm2 <- st_read(here::here("../MIMI_data/nga/map_data/new_shapefiles/nigeria_2.shp"))
# eth_adm1 <- st_read(here::here("../MIMI_data/nga/map_data/new_shapefiles/nigeria_1.shp"))

# Nigeria ----------------------------------------------------------------------



allen_ear <- data.frame(
  energy_kcal = 2100,#who
  vita_rae_mcg   = 490, 
  thia_mg = 0.9,
  ribo_mg = 1.3, 
  niac_mg = 11, 
  vitb6_mg = 1.3, 
  folate_mcg = 250, 
  vitb12_mcg = 2, 
  fe_mg = 22.4, #moderate absorption
  ca_mg = 860, 
  zn_mg = 10.2#unrefined
)


library(dplyr)



eth_hices1516_hh_info <- eth_hices1516_hh_info %>% 
  dplyr::select(hhid,survey_wgt,ea)


eth_sw_mean_intake <- eth_hices1516 %>% 
  dplyr::left_join(
    eth_hices1516_hh_info %>% distinct(hhid, .keep_all = T) , by = "hhid"
  ) %>% 
  dplyr::mutate(
    vita_rae_mcg  = ifelse(vita_rae_mcg <allen_ear$vita_rae_mcg, 
                           vita_rae_mcg /allen_ear$vita_rae_mcg,
                          1),
    thia_mg = ifelse(thia_mg<allen_ear$thia_mg, 
                     thia_mg/allen_ear$thia_mg,
                     1),
    ribo_mg = ifelse(ribo_mg<allen_ear$ribo_mg, 
                     ribo_mg/allen_ear$ribo_mg,
                     1),
    niac_mg = ifelse(niac_mg<allen_ear$niac_mg, 
                     niac_mg/allen_ear$niac_mg,
                     1),
    vitb6_mg = ifelse(vitb6_mg<allen_ear$vitb6_mg, 
                      vitb6_mg/allen_ear$vitb6_mg,
                      1),
    folate_mcg = ifelse(folate_mcg<allen_ear$folate_mcg, 
                        folate_mcg/allen_ear$folate_mcg,
                        1),
    vitb12_mcg = ifelse(vitb12_mcg<allen_ear$vitb12_mcg, 
                        vitb12_mcg/allen_ear$vitb12_mcg,
                        1),
    fe_mg = ifelse(fe_mg<allen_ear$fe_mg, 
                   fe_mg/allen_ear$fe_mg,
                   1),
    ca_mg= ifelse(ca_mg<allen_ear$ca_mg, 
                  ca_mg/allen_ear$ca_mg,
                  1),
    zn_mg = ifelse(zn_mg<allen_ear$zn_mg, 
                   zn_mg/allen_ear$zn_mg,
                   1)
    
  ) %>%
  dplyr::mutate(mar = (vita_rae_mcg+thia_mg+ribo_mg+
                         niac_mg+vitb6_mg+folate_mcg+
                         vitb12_mcg+zn_mg)/8) %>% 
  srvyr::as_survey_design(id = hhid, strata = res,
                          weights = survey_wgt, nest=T)





eth_adm1_mar <- eth_sw_mean_intake %>% 
  group_by(group = adm1) %>% 
  srvyr::summarise(mar = survey_mean(mar))

eth_res_mar <- eth_sw_mean_intake %>% 
  group_by(group = res) %>% 
  srvyr::summarise(mar = survey_mean(mar))

eth_res_quintole_mar <- eth_sw_mean_intake %>% 
  group_by(res,res_quintile) %>% 
  
  srvyr::summarise(mar = survey_mean(mar)) %>% 
  ungroup() %>% 
  mutate(group = paste(res, res_quintile)) %>% 
  select(-c(res,res_quintile))


eth_nat_mar = eth_sw_mean_intake %>% 
  group_by(group = "national") %>% 
  srvyr::summarise(mar = survey_mean(mar))

ethiopia_mar_base <- bind_rows(eth_adm1_mar,eth_res_mar, eth_res_quintole_mar, eth_nat_mar)
write_csv(ethiopia_mar_base, here::here("ethiopia_base_mar.csv"))
