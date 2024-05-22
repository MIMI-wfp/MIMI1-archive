#########################################
#        Identify the PDS hhs           #
#########################################

# Author: Gabriel Battcock
# Created: 8 May 24
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

# ------------------------------------------------------------------------------       
# read in nsso clean data
path_to_file <- here::here("data_rich/India/data/processed/lsff/")

ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("ind_nss1112", "_hh_info.csv")))
food_consumption<- read.csv(paste0(path_to_file, paste0("ind_nss1112", "_food_consumption.csv")))
fc_table <- read.csv(paste0(path_to_file, "ind_nss1112_fct.csv"))
ind_expenditure <- read.csv("/Users/gabrielbattcock/Documents/MIMI/data_science_code/DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012/Summary of Consumer Expenditure - Block 12 - Level 11 - 68.csv")

india_adm2 <- st_read(here::here("data_rich/India/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))


# ------------------------------------------------------------------------------
# income criteria
# "annual income of below 150,000 Rs a uear can apply" 
############# Srl_no = 48 is monthly per capita expenditure urp



annual_limit <- 150000
cpi_2024 = 156.972
cpi_2012 =	80.077
inflation = 	cpi_2024/cpi_2012

eligible <- ind_expenditure %>% 
  rename(hhid = HHID) %>% 
  filter(Srl_no >= 20) 
# %>% 
#   select(hhid, Value) %>% 
#   group_by(hhid) %>% 
#   summarise(Value = sum(Value)) 
# # %>% 

ind_expenditure %>% 
  rename(hhid = HHID) %>% 
  filter(Srl_no %in% c(46,49)) %>% 
  select(hhid, Srl_no, Value) %>% 
  pivot_wider(names_from = Srl_no, values_from = Value) %>%
  rename(pc = `46`,
         Value = `49`) %>% 
  mutate(Value = Value*inflation*pc) %>%
  mutate(eligible = ifelse(Value<annual_limit, 1, 0 )) %>%
  summarise(tot = sum(eligible),
            n = n())







ind_expenditure %>% 
  select(Srl_no) %>% 
  summary()

eligible %>% 
  # mutate(Value = Value/100)%>% 
    mutate(eligible = ifelse(Value<annual_limit, 1, 0 )) %>% 
  summarise(sum(eligible)/n())

eligible %>% 
  ggplot(aes(Value))+ geom_histogram()+
  xlim(0,2000000)


### ----------------------------------------------------------------------------
# scheduled tribe

