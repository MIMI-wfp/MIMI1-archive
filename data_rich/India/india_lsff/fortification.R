#########################################
#        INDIA LSFF RESEARCH            #
#########################################


# Author: Gabriel Battcock
# Created: 22 Feb 24
# Last updated: 27 Feb 24

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

# -----------------------------------------------------------------------------
# read in the base case and read in the fortifiable food vehicles 
ind_ai <- apparent_intake("ind_nss1112", here::here("data_rich/India/data/processed/lsff//"))

hh_info

base_model <- read_csv(here::here("data_rich/India/data/final/extra_states/base_model.csv"))
india_adm2 <- st_read(here::here("data_rich/India/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))


#isolate the rice PDS for now

pds_rice <- 
  ind_nss1112_consumption %>% 
  filter(item_code == "101") %>% 
  group_by(hhid,item_code) %>% 
  summarise(quantity_g = sum(quantity_g),
            quantity_100g = sum(quantity_100g))# just to make sure an hh with multiple items are summed


# Define the amount of contributions from PDS rice fortification

ind_rice_fort <- 
  data.frame(
    #all per kg
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
  mutate(folate_fort_mcg = ind_rice_fort$folic_hi_mcg/10*quantity_100g)

fort_model <- ind_nsso1112 %>% 
  left_join(ind_nsso1112_hh_info, by = 'hhid') %>% 
  filter(adm1 %in% c(09,10,22)) %>% 
  left_join(pds_rice_low, by = "hhid") %>% 
  mutate(
    folate_fort_mcg = ifelse(is.na(folate_fort_mcg),0,folate_fort_mcg),
    folate_fort_mcg = folate_mcg + folate_fort_mcg)



fort_model <- fort_model %>% 
  dplyr::mutate(
    folate_fort = ifelse(folate_fort_mcg<nin_ear$folate_ug, 
                         folate_fort_mcg/nin_ear$folate_ug,
                         1
    
  )) %>% 
  select(hhid, adm2,survey_wgt,folate_mcg,folate_fort_mcg) %>% 
  # dplyr::mutate(mar = (vita_mg+vitb1_mg+vitb2_mg+
  #                        vitb3_mg+vitb6_mg+folate_ug+
  #                        vitaminb12_in_mg+ calcium_mg+iron_mg+zinc_mg)/10) %>%
  srvyr::as_survey_design(id = hhid, strata = adm2,
                          weights = survey_wgt, nest=T)


sw_mean_intake_district_tot <- fort_model %>%
  srvyr::group_by(adm2) %>%
  srvyr::summarise(
    srvyr::across(-c(hhid,survey_wgt),
                  ~mean(.))
  ) %>%
  dplyr::mutate(adm2 = as.numeric(adm2)) %>%
  dplyr::left_join(
    india_adm2,
    by = c("adm2" = "Dstrct_c")
  ) %>%
  dplyr::ungroup() %>%
  st_as_sf()



#  risk maps

folate_mcg_adm2<- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "folate_mcg", style = "cont", breaks = c(200,300,400,500),
          # c(1000,1550,2100,2650,3200),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Folate (mcg)")+
  tm_layout(main.title =  "Folate no fortification", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)


folate_mcg_adm2_fort <- 
  tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "folate_fort_mcg", style = "cont", breaks = c(200,300,400,500),
          # c(1000,1550,2100,2650,3200),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Folate (mcg)")+
  tm_layout(main.title =  "Folate fortifcation (minimum fortificant) ", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = T)



