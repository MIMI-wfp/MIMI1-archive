#########################################
#          making shapefiles            #
#########################################


# Author: Gabriel Battcock
# Created: 29 May 24
# Last updated: 29 May 24

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

# ETH HICES      -------------------------------------------------------------




eth_adm2 <- st_read(here::here("../MIMI_data/Ethiopia/gadm41_ETH_shp/gadm41_ETH_2.shp"))
eth_hices_adm2 <- read.xlsx("data_rich/all_base_models/data/ETH_HCES1516_district_codes.xlsx", 1)

eth_hices_sp <- eth_adm2 %>% 
  left_join(eth_hices_adm2 %>% select(GID_2, district_code), 
            by = "GID_2")

st_write(eth_hices_sp,"~/../General - MIMI Project/Nutrition analysis/shapefiles/eth_hices1516_adm2.shp")




# NGA --------------------------------------------------------------------------

nga1819 <- apparent_intake("nga_lss1819")
nga1819_hh_info <- household_data("nga_lss1819")
nga_lga_dict <- read.csv("../nigeria_mapping/data_dictionary/lga.csv")
hh_to_lga <-as_tibble(read.csv("../nigeria_mapping/hh_to_lga.csv"))
nga_adm2 <- st_read(here::here("../MIMI_data/nga/map_data/new_shapefiles/nigeria_2.shp"))
nga_adm1 <- st_read(here::here("../MIMI_data/nga/map_data/new_shapefiles/nigeria_1.shp"))


plot(nga_adm1$geometry)
allen_ear <- data.frame(
  energy_kcal = 2200,#who
  vita_rae_mcg  = 490, 
  thia_mg = 0.9,
  ribo_mg = 1.3, 
  niac_mg = 11, 
  vitb6_mg = 1.3, 
  folate_mcg = 250, 
  vitb12_mcg = 2, 
  fe_mg = 9.6, #moderate absorption
  ca_mg = 860, 
  zn_mg = 11#semi unrefined
)


library(dplyr)



nga1819_adm2_sp <- nga1819_hh_info %>% 
  left_join(hh_to_lga %>% dplyr::select(hhid, lga), by ="hhid") %>% 
  dplyr::select(hhid,adm2,survey_wgt,lga,ea) %>% 
  distinct(adm2,lga) %>% 
  full_join(nga_adm2, by = "lga")




st_write(nga1819_adm2_sp ,"~/../General - MIMI Project/Nutrition analysis/shapefiles/nga_lss1819_adm2.shp")


# INDIA ------------------------------------------------------------------------

adm2_shrug <- read.csv(here::here("data_rich/India/data/processed/lsff/nss_shrug_adm2.csv"))

shrug_adm2_sp <- st_read(here::here("../MIMI_data/shapefile_data/raw_shapefiles/shrug-pc11dist-poly-shp/district.shp"))


ind_district <- adm2_shrug %>% 
  select(-c(pc11_s_id,d_name)) %>% 
  inner_join(shrug_adm2_sp %>% mutate(pc11_d_id = as.integer(pc11_d_id)), by = "pc11_d_id") %>% 
  group_by(adm2_code) %>% 
  mutate(geometry = sf::st_union(geometry)) %>%
  slice(1)

st_write(ind_district ,"~/../General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp")

