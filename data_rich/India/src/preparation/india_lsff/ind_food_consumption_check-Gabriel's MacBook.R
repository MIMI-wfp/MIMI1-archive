#########################################
#        create mapping function        #
#########################################

# Author: Gabriel Battcock
# Created: 23 Apr 24
# Last updated: 8 May 24

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

# source the base models function
source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))
source(here::here("data_rich/India/src/preparation/india_lsff/ind_mapping.R"))



# create path files to two models -------------------------------------------


ind_7day_path <- "data_rich/India/data/data_052024_7day/"
ind_30day_path <-  here::here("data_rich/India/data/processed/lsff/")

ind_7day <- apparent_intake("ind_nss1112", ind_7day_path)
ind_nsso1112_hh_info

ind_30day <- apparent_intake("ind_nss1112",ind_30day_path)

## check the summary stats for the intake variables



# energy 

summary(ind_7day$energy_kcal)
summary(ind_30day$energy_kcal)


x <- ind_nsso1112_hh_info %>% left_join(ind_30day, by = 'hhid')

tapply(x$energy_kcal, x$adm1, summary)

ind_7day %>% 
  filter(energy_kcal <= quantile(energy_kcal,.99, na.rm =TRUE)) %>% 
  ggplot(aes(x  = energy_kcal)) + 
  geom_histogram()


ind_30day %>% 
  filter(energy_kcal <= quantile(energy_kcal,.99, na.rm =TRUE)) %>% 
  ggplot(aes(x  = energy_kcal)) + 
  geom_histogram()

# vitamin A


summary(ind_7day$vita_rae_mcg)
summary(ind_30day$vita_rae_mcg)

ind_7day %>% 
  filter(energy_kcal <= quantile(energy_kcal,.99, na.rm =TRUE)) %>% 
  ggplot(aes(x  = vita_rae_mcg)) + 
  geom_histogram()


ind_30day %>% 
  filter(energy_kcal <= quantile(energy_kcal,.99, na.rm =TRUE)) %>% 
  ggplot(aes(x  = vita_rae_mcg)) + 
  geom_histogram()





summary(ind_7day$fe_mg)
summary(ind_30day$fe_mg)

ind_7day %>% 
  filter(energy_kcal <= quantile(energy_kcal,.99, na.rm =TRUE)) %>% 
  ggplot(aes(x  = vitb12_mcg)) + 
  geom_histogram()


ind_30day %>% 
  filter(energy_kcal <= quantile(energy_kcal,.99, na.rm =TRUE)) %>% 
  ggplot(aes(x  = vitb12_mcg)) + 
  geom_histogram()

summary(ind_7day$vitb12_mcg)
summary(ind_30day$vitb12_mcg)