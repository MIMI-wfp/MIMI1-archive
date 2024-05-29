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

