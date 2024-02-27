# Author: Gabriel Battcock
# Collaborators: Mo Osman & Kevin Tang
# Date created: 14-Feb-2024
# Last edited: 26-Feb-2024

#  Data request for MIMI for hunger map
rm(list = ls())

rq_packages <- c("tidyverse","srvyr","readr","dplyr",
                 "ggridges", "gt", "haven","foreign", "readxl")
installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list= c("rq_packages", "installed_packages"))

source(here::here("data_rich/all_base_models/scripts/base_model_functions.R"))
source(here::here("universal_functions/iron_full_probability/src/iron_inad_prev.R"))

# read in apparent intake data
eth_hices1516 <- apparent_intake("eth_hices1516") 
nga_lss1819 <- apparent_intake("nga_lss1819")

# household data

eth_hices1516_hh_info <- household_data("eth_hices1516") 
nga_lss1819_hh_info <- household_data("nga_lss1819")
unique(nga_lss1819_hh_info$adm2)
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


##

#ethiopia
# iron full probability
eth_fe <-
  eth_hices1516_hh_info %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  select(hhid,adm1,adm2,afe,survey_wgt) %>% 
  left_join(eth_hices1516, by = c('hhid','afe')) %>% 
  mutate(fe_inad = 
               case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                 fe_mg <= 15 ~ "1",
                 fe_mg <= 16.7 & fe_mg > 15 ~ "0.96",
                 fe_mg <= 18.7 & fe_mg > 16.7 ~ "0.93",
                 fe_mg <= 21.4 & fe_mg > 18.7 ~ "0.85",
                 fe_mg <= 23.6 & fe_mg > 21.4 ~ "0.75",
                 fe_mg <= 25.7 & fe_mg > 23.6 ~ "0.65",
                 fe_mg <= 27.8 & fe_mg > 25.7 ~ "0.55",
                 fe_mg <= 30.2 & fe_mg > 27.8 ~ "0.45",
                 fe_mg <= 33.2 & fe_mg > 30.2 ~ "0.35",
                 fe_mg <= 37.3 & fe_mg > 33.2 ~ "0.25",
                 fe_mg <= 45.0 & fe_mg > 37.3 ~ "0.15",
                 fe_mg <= 53.5 & fe_mg > 45.0 ~ "0.08",
                 fe_mg <= 63.0 & fe_mg > 53.5 ~ "0.04",
                 fe_mg > 63 ~ "0")
  ) %>% 
  as_survey_design(id = hhid, strata = ,
                   weights = survey_wgt) %>% 
  group_by(fe_inad, adm2) %>%     # Counting number of observations that fall into each probability category
  summarise(
    fe_prop = n()
  )%>%
  ungroup() %>%
  pivot_wider(names_from = c(adm2) , #this can be the name of the 'group by" group
              values_from = fe_prop)  %>%
  mutate(across(everything(),
                ~replace_na(.,0))) %>%
  summarise(
    across(-fe_inad,
           ~sum(.x*as.numeric(fe_inad))/sum(.x)*100
    )
  ) %>%
  pivot_longer(cols = everything()) %>%
  rename(subpopulation = name, fe_ai = value)


# nigeria iron full probability model
nga_fe <-
  nga_lss1819_hh_info %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  select(hhid,adm1,adm2,afe,survey_wgt) %>% 
  left_join(nga_lss1819, by = c('hhid','afe')) %>% 
  mutate(fe_inad = 
           case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
             fe_mg <= 15 ~ "1",
             fe_mg <= 16.7 & fe_mg > 15 ~ "0.96",
             fe_mg <= 18.7 & fe_mg > 16.7 ~ "0.93",
             fe_mg <= 21.4 & fe_mg > 18.7 ~ "0.85",
             fe_mg <= 23.6 & fe_mg > 21.4 ~ "0.75",
             fe_mg <= 25.7 & fe_mg > 23.6 ~ "0.65",
             fe_mg <= 27.8 & fe_mg > 25.7 ~ "0.55",
             fe_mg <= 30.2 & fe_mg > 27.8 ~ "0.45",
             fe_mg <= 33.2 & fe_mg > 30.2 ~ "0.35",
             fe_mg <= 37.3 & fe_mg > 33.2 ~ "0.25",
             fe_mg <= 45.0 & fe_mg > 37.3 ~ "0.15",
             fe_mg <= 53.5 & fe_mg > 45.0 ~ "0.08",
             fe_mg <= 63.0 & fe_mg > 53.5 ~ "0.04",
             fe_mg > 63 ~ "0")
  ) %>% 
  as_survey_design(id = hhid, strata = ,
                   weights = survey_wgt) %>% 
  group_by(fe_inad, adm2) %>%     # Counting number of observations that fall into each probability category
  summarise(
    fe_prop = n()
  )%>%
  ungroup() %>%
  pivot_wider(names_from = c(adm2) , #this can be the name of the 'group by" group
              values_from = fe_prop)  %>%
  mutate(across(everything(),
                ~replace_na(as.numeric(.),0))) %>%
  summarise(
    across(-fe_inad,
           ~sum(.x*as.numeric(fe_inad))/sum(.x)*100
    )
  ) %>%
  pivot_longer(cols = everything()) %>%
  rename(subpopulation = name, fe_ai = value)
  
# ethiopaia

eth_ai <- eth_hices1516_hh_info %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  select(hhid,adm1,adm2,afe,survey_wgt) %>% 
  left_join(eth_hices1516, by = c('hhid','afe')) %>% 
  mutate(iso = "ETH",
         va_ai = ifelse(vita_rae_mcg<allen_har$vita_rae_mcg,0,1),
         fo_ai = ifelse(folate_mcg < allen_har$folate_mcg, 0,1),
         vb12_ai = ifelse(vitb12_mcg < allen_har$vitb12_mcg, 0,1),
         zn_ai = ifelse(zn_mg < allen_har$zn_mg_u, 0,1)
         ) %>% 
  select(iso,hhid,survey_wgt,adm1,adm2,va_ai,fo_ai,vb12_ai,zn_ai)

#nigeria 
nga_ai <- nga_lss1819_hh_info %>% 
  distinct(hhid, .keep_all = TRUE) %>% 
  select(hhid,adm1,adm2,afe,survey_wgt) %>% 
  left_join(nga_lss1819, by = c('hhid','afe')) %>% 
  mutate(iso = "NGA",
         va_ai = ifelse(vita_rae_mcg<allen_har$vita_rae_mcg,0,1),
         fo_ai = ifelse(folate_mcg < allen_har$folate_mcg, 0,1),
         zn_ai = ifelse(zn_mg < allen_har$zn_mg_u, 0,1),
         vb12_ai = ifelse(vitb12_mcg < allen_har$vitb12_mcg, 0,1)
  ) %>% 
  select(iso,hhid,survey_wgt,adm1,adm2,va_ai,fo_ai,vb12_ai,zn_ai)


# create survey objects

eth_ai <- 
  eth_ai %>% 
  srvyr::as_survey_design(id = hhid, strata = ,
                          weights = survey_wgt) %>% 
  srvyr::group_by(iso,adm2) %>%
  srvyr::summarise(
    across(
      c(va_ai,fo_ai,vb12_ai,zn_ai),
      
      ~(survey_mean(.x == 0, proportion = TRUE, vartype = "ci",na.rm = T)*100)
    )
    )%>% 
  left_join(eth_fe, by = c("adm2" = "subpopulation")) %>% 
    select(iso,adm2,va_ai,fo_ai,vb12_ai,fe_ai,zn_ai)




  
nga_ai <- nga_ai %>% 
  srvyr::as_survey_design(id = hhid, strata = ,
                          weights = survey_wgt) %>% 
  srvyr::group_by(iso,adm2) %>% 
  srvyr::summarise(
    across(
      c(va_ai,fo_ai,vb12_ai,zn_ai),
      ~(survey_mean(.x == 0, proportion = TRUE, vartype = "ci",na.rm = T)) * 100)
  ) %>% 
  left_join(nga_fe %>% mutate(adm2 = as.numeric(subpopulation)),
            by = 'adm2') %>% 
  select(iso,adm2,va_ai,fo_ai,vb12_ai,fe_ai,zn_ai)


# bind the columns into single data frame
kev_eth_codes <- read.csv("/Users/gabrielbattcock/Documents/MIMI/ETH_HCES1516_district_codes 1.csv")
nga_lga_dict <- read.csv("../nigeria_mapping/data_dictionary/lga.csv")
hh_to_lga <-as_tibble(read.csv("../nigeria_mapping/hh_to_lga.csv"))
nga_ram <- read_xlsx(here::here("data_rich/data_requests/ram_mimi20240226/wfp_ram.xlsx"), sheet= "nga")

sum((nga_ram$lga%in%nga_ai$adm2  ) == TRUE)

nga_final <- nga_ai %>% 
  left_join(nga_ram, 
            by = c("adm2" = "lga")
            )%>% 
  select(-c(adm2,Category,comments)) %>% 
  rename(admin2 = wfp_admin2)


write_csv(nga_final, here::here("data_rich/data_requests/ram_mimi20240226/nga_mimi_20240227.csv"))

              
