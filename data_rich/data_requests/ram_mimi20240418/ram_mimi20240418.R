# Data request for MIMI targets in India
# Date create 18/04/2024
# Date modified
# Author: Gabriel Battcock 


rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))
# source(here::here("data_rich/dietary_assessment/processing/individual_level_clean.R"))

# create some data frame
ind_targets <- ind_nsso1112 %>% 
  select(
       hhid,
       vita_rae_mcg,
       vitb12_mcg,
       folate_mcg,
       fe_mg,
       zn_mg
            ) %>% 
  mutate(
        hhid = as.character(hhid),
         va_ref = 490,
         fol_ref = 250,
         vb12_ref = 2, 
         fe_ref = 22.4,
         zn_ref = 10.2,
         va_nar = ifelse(vita_rae_mcg>va_ref, 1, vita_rae_mcg/va_ref),
         fol_nar = ifelse(folate_mcg>fol_ref, 1, folate_mcg/fol_ref),
         vb12_nar = ifelse(vitb12_mcg>vb12_ref, 1, vitb12_mcg/vb12_ref),
         fe_nar = ifelse(fe_mg>fe_ref, 1, fe_mg/fe_ref),
         zn_nar = ifelse(zn_mg>zn_ref, 1, zn_mg/zn_ref),
         mimi_simple = (va_nar+fol_nar+vb12_nar+fe_nar+zn_nar)/5
    ) %>% 
  rename(
    va_ai = vita_rae_mcg,
    fol_ai = folate_mcg,
    vb12_ai = vitb12_mcg,
    fe_ai = fe_mg,
    zn_ai = zn_mg) %>% 
  select(
        hhid,
        va_ai,
         fol_ai,
         vb12_ai,
         fe_ai,
         zn_ai,
         va_ref,
      fol_ref,
      vb12_ref,
      fe_ref,
      zn_ref,
      mimi_simple
      ) %>% 
  mutate(survey_id = "DDI-IND-MOSPI-NSSO-68Rnd-Sch2.0-July2011-June2012")


# read in the other data

ethiopia_nigeria_targets <- read.csv("~/Documents/MIMI/code/data_rich/data_requests/ram_mimi20240418/ethiopia_nigeria_targets_toaddindia.csv")

eth_nga_ind_targets <- ethiopia_nigeria_targets %>% 
  bind_rows(ind_targets) %>% 
  select(-X)


# save the data

write.csv(eth_nga_ind_targets,
          file = "~/Documents/MIMI/code/data_rich/data_requests/ram_mimi20240418/ethiopia_nigeria_india_targets.csv")

#### test some of the data 


ind_targets %>% 
  mutate(vita_inad = ifelse(va_ai>va_ref,1,0),
         iron_inad = ifelse(fe_ai>fe_ref,1,0),
         vitb12_inad = ifelse(vb12_ai>vb12_ref,1,0)) %>% 
  summarise(sum(vitb12_inad))



ind_nsso1112 %>% 
  summarise(mean(vita_rae_mcg))
