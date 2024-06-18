#########################################
#        INDIA LSFF RESEARCH            #
#########################################


# Author: Gabriel Battcock
# Created: 22 Feb 24
# Last updated: 6 June 24

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
source(here::here("data_rich/India/india_lsff/ind_elig_classification.R"))
# -----------------------------------------------------------------------------
# read in the base case and read in the fortifiable food vehicles 
ind_ai <- apparent_intake("ind_nss1112", here::here("data_rich/India/data/processed/lsff//"))
ind_hh_info <- household_data("ind_nss1112", here::here("data_rich/India/data/processed/lsff//"))
ind_all_items <- full_item_list("ind_nss1112", here::here("data_rich/India/data/processed/lsff//"))
ind_vehicles <- read.csv(here::here("C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Nutrition analysis/fortification scenarios/data/current/ind_nss1112_vehicle_quantities.csv"))

base_model <- read_csv(here::here("data_rich/India/data/final/extra_states/base_model.csv"))
india_adm2 <- st_read(here::here("~/../General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))

plot(india_adm2$geometry)
plot(india_adm1$geometry)

x <- india_adm1 %>% filter(State_code != 1)
plot(x$geometry)
ind_nss1112_consumption <- food_consumption


# Fortification all rice ----------------------------------------------------------------

# assume that all rice consumed will be fortified 
pds_rice <- 
  ind_all_items %>% 
  filter(item_code %in% c("101","102")) %>% 
  group_by(hhid,item_code) %>% 
  summarise(quantity_g = sum(quantity_g),
            quantity_100g = sum(quantity_100g)) %>%  # just to make sure an hh with multiple items are summed
  ungroup() %>% 
  group_by(hhid) %>% 
  summarise(quantity_g = sum(quantity_g),
            quantity_100g = sum(quantity_100g)) 

# Define the amount of contributions from PDS rice fortification

ind_rice_fort <- 
  data.frame(
    # all per kg
    # from compednium 
    fe_pyro_low_mg = 28,
    fe_pyro_hi_mg = 42.5,
    fe_na_low_mg = 14,
    fe_na_hi_mg = 21.25,
    folic_low_mcg = 75,
    folic_hi_mcg = 125,
    vb12_low_mcg = .75,
    vb12_hi_mcg = 1.25,
    
    #optional
    zn_low_mg = 10,
    zn_hi_mg = 15,
    va_low_mcg = 500,
    va_hi_mcg = 750,
    vb1_low_mg = 1,
    vb1_hi_mg = 1.5,
    vb2_low_mg = 1.25,
    vb2_hi_mg = 1.75,
    vb3_low_mg = 12.5,
    vb3_hi_mg = 20,
    vb6_low_mg = 1.5,
    vb6_hi_mg = 2.5
  )

# test out for folate
pds_rice_low <- pds_rice%>% 
  # 
  mutate(vb12_fort = ind_rice_fort$vb12_hi_mcg/10*quantity_100g)


fort_model <- ind_ai %>% 
  left_join(ind_hh_info, by = 'hhid') %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  left_join(pds_rice_low, by = "hhid") %>% 
  mutate(
    vb12_fort = ifelse(is.na(vb12_fort),0,vb12_fort),
    vb12_fort = vitb12_mcg + vb12_fort)



fort_model <- fort_model %>% 
  dplyr::mutate(
    vb12_fort_inad = ifelse(vb12_fort<allen_ear$ear_value[allen_ear$nutrient == "vitb12_mcg"], 
                         1,
                         0),
    vb12_inad = ifelse(vitb12_mcg<allen_ear$ear_value[allen_ear$nutrient == "vitb12_mcg"], 
                              1,
                              0
                              
    )) %>% 
  select(hhid, adm2,survey_wgt,vitb12_mcg, vb12_fort,vb12_fort_inad,folate_inad) %>% 
  # dplyr::mutate(mar = (vita_mg+vitb1_mg+vitb2_mg+
  #                        vitb3_mg+vitb6_mg+folate_ug+
  #                        vitaminb12_in_mg+ calcium_mg+iron_mg+zinc_mg)/10) %>%
  srvyr::as_survey_design(id = hhid, strata = adm2,
                          weights = survey_wgt, nest=T)




sw_mean_intake_district_tot <- fort_model %>%
  srvyr::group_by(adm2) %>%
  srvyr::summarise(
    fort_inad_prev = srvyr::survey_mean(folate_fort_inad == 1, proportion = TRUE),
    inad_prev = srvyr::survey_mean(folate_inad == 1, proportion = TRUE)
  ) %>%
  dplyr::mutate(adm2 = as.numeric(adm2)) %>%
  dplyr::left_join(
    india_adm2,
    by = c("adm2" = "adm2_code")
  ) %>%
  dplyr::ungroup() %>%
  st_as_sf()



#  risk maps

# folate_mcg_adm2<- 
  tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "inad_prev", style = "cont", breaks = seq(0,1,.10),
          
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of inadequacy")+
  tm_layout(main.title =  "Folate no fortification", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)
  # tm_shape(states_sp) + 
  # # tm_fill(col = "state") +
  # tm_borders(col = "black", lwd = 1.5)+
  # tm_legend(show = T)


# folate_mcg_adm2<- 
  tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "fort_inad_prev", style = "cont", breaks = seq(0,1,.10),
          # c(1000,1550,2100,2650,3200),
          palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "Prevalence of inadequacy")+
  tm_layout(main.title =  "Folate WITH fortification", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)


################################################################################
################################################################################
  
  
# remove pds rice receive and add back new amounts
  
ind_pds_eligible_pl <- get_eligible_households(pl = TRUE)

  
ind_pds_items <- ind_all_items %>% 
    filter(hhid %in% ind_pds_eligible_pl$hhid) %>% 
    select(hhid,afe, item_code,item_name, quantity_g, quantity_100g) %>% 
    mutate(quantity_g = ifelse(item_code %in% c(101,107), 0, quantity_g),
           quantity_100g = ifelse(item_code %in% c(101,107), 0, quantity_100g)) %>% 
    left_join(ind_pds_eligible_pl %>% select(hhid,adm1,eligible), by = 'hhid') %>% 
    mutate(
      #elgibile household get a certain amount of grain
      quantity_g = ifelse(eligible ==1, case_when(
        adm1 == "Punjab" & item_code == 107 ~ 35000/(afe*30),
        adm1 == "Haryana"& item_code == 107 ~ 35000/(afe*30),
        adm1 == "Rajasthan"& item_code == 107 ~ 35000/(afe*30),
        adm1 == "Chhattisgarh"& item_code == 101 ~ 35000/(afe*30),
        adm1 == "Bihar"& item_code == 101 ~ 28000/(afe*30),
        adm1 == "Bihar"& item_code == 107 ~ 7000/(afe*30),
        adm1 == "UP"& item_code == 101 ~ 21000/(afe*30),
        adm1 == "UP"& item_code == 107 ~ 14000/(afe*30),
        .default = quantity_g
      ), quantity_g)
    ) %>% 
    filter(eligible==1)

# match to fct with fortificant added ------------------------------------------
  

  
  
  
  
  

