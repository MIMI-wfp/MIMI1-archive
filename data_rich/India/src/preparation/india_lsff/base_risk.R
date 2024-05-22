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
       
# ------------------------------------------------------------------------------       
# read in nsso clean data
path_to_file <- here::here("data_rich/India/data/processed/lsff/")

ind_nsso1112_hh_info <-  read.csv(paste0(path_to_file, paste0("ind_nss1112", "_hh_info.csv")))
food_consumption<- read.csv(paste0(path_to_file, paste0("ind_nss1112", "_food_consumption.csv")))
fc_table <- read.csv(paste0(path_to_file, "ind_nss1112_fct.csv"))

food_consumption %>% distinct(hhid)

ind_nsso1112_all_items <- ind_nsso1112_hh_info %>% 
  select(hhid,afe) %>% 
  cross_join(fc_table %>% 
               distinct(item_code, .keep_all = TRUE) %>% 
               select(item_code)) %>% 
  left_join(food_consumption %>% 
              group_by(hhid, item_code, food_group) %>% 
              summarise(across(
                everything(),
                ~sum(., na.rm = TRUE)
              )) %>% 
              ungroup(), 
            by = c("hhid", "item_code")) %>% 
  select(-food_group,value) %>% 
  mutate(
    across(
      -c(hhid,afe, item_code),
      ~ifelse(is.na(.), 0,.)
    )
  ) %>% 
  mutate(
    quantity_100g = quantity_100g/afe, 
    quantity_g = quantity_g/afe
  ) %>% 
  left_join(fc_table %>% 
              distinct(item_code,.keep_all = TRUE), by = "item_code") %>% 
  inner_join(food_consumption %>% 
               select(item_code, food_group) %>% 
               distinct(item_code, food_group),
             by = c('item_code'))


ind_nsso1112 <- food_consumption %>% 
  left_join(fc_table %>% 
              distinct(item_code,.keep_all =T), by = "item_code") %>% 
  mutate(
    across(
      -c(item_code, hhid,item_name ,food_group, quantity_100g, quantity_g),
      ~.x*quantity_100g
    )
  ) %>% 
  group_by(hhid) %>% 
  summarise(
    across(-c(item_code,item_name,quantity_100g,quantity_g, food_group),
           ~sum(.,na.rm = T))
  ) %>% 
  left_join(ind_nsso1112_hh_info %>% select(hhid, afe), by = "hhid") %>% 
  mutate(
    across(
      -c(hhid,afe),
      ~.x/afe
    )
  ) %>% 
  ungroup()
# ------------------------------------------------------------------------------

ind_nsso1112 %>% 
  left_join(ind_nsso1112_hh_info, by = "hhid") %>% 
  ggplot(aes(x = energy_kcal,y = factor(adm1),fill = factor(adm1)))+
  geom_density_ridges(alpha = 0.5) +
  xlim(0,3500)

# -----------------------------------------------------------------------------


base_model <- read_csv(here::here("data_rich/India/data/final/extra_states/base_model.csv"))
india_adm2 <- st_read(here::here("data_rich/India/data/processed/extra_states/district_shape.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))

household_characteristics <- read_csv(paste0("data_rich/India/data/processed/extra_states/household_char.csv"))
# x <-  india_adm2%>% dplyr::anti_join(base_model, by = c("Dstrct_c" = "District_code"))

# create average intakes at adm2 level 
nin_ear <- data.frame(
  energy_kcal = 2130,
  vita_mg  = 390, 
  vitb1_mg = 1.4,
  vitb2_mg = 2.0, 
  vitb3_mg = 12, 
  vitb5_mg = 4,#from allen 2020
  vitb6_mg = 1.6, 
  folate_ug = 180, 
  vitaminb12_in_mg = 2, 
  iron_mg = 15, 
  calcium_mg = 800, 
  zinc_mg = 11
)
unique(base_model$State_code)

sw_mean_intake <- base_model %>% 
  dplyr::left_join(
    household_characteristics %>% 
      dplyr::select(
        HHID, Combined_multiplier, HH_Type_code
      ), by = "HHID"
  ) %>% 
  dplyr::mutate(
    energy_kcal = ifelse(energy_kcal<nin_ear$energy_kcal, 
                         energy_kcal/nin_ear$energy_kcal,
                         1),
    vita_mg = ifelse(vita_mg<nin_ear$vita_mg, 
                     vita_mg/nin_ear$vita_mg,
                     1),
    vitb1_mg = ifelse(vitb1_mg<nin_ear$vitb1_mg, 
                      vitb1_mg/nin_ear$vitb1_mg,
                      1),
    vitb2_mg = ifelse(vitb2_mg<nin_ear$vitb2_mg, 
                      vitb2_mg/nin_ear$vitb2_mg,
                      1),
    vitb3_mg = ifelse(vitb3_mg<nin_ear$vitb3_mg, 
                      vitb3_mg/nin_ear$vitb3_mg,
                      1),
    vitb5_mg = ifelse(vitb5_mg<nin_ear$vitb5_mg, 
                      vitb5_mg/nin_ear$vitb5_mg,
                      1),
    vitb6_mg = ifelse(vitb6_mg<nin_ear$vitb6_mg, 
                      vitb6_mg/nin_ear$vitb6_mg,
                      1),
    folate_ug = ifelse(folate_ug<nin_ear$folate_ug, 
                       folate_ug/nin_ear$folate_ug,
                       1),
    vitaminb12_in_mg = ifelse(vitaminb12_in_mg<nin_ear$vitaminb12_in_mg, 
                              vitaminb12_in_mg/nin_ear$vitaminb12_in_mg,
                              1),
    iron_mg = ifelse(iron_mg<nin_ear$iron_mg, 
                     iron_mg/nin_ear$iron_mg,
                     1),
    calcium_mg= ifelse(calcium_mg<nin_ear$calcium_mg, 
                       calcium_mg/nin_ear$calcium_mg,
                       1),
    zinc_mg = ifelse(zinc_mg<nin_ear$zinc_mg, 
                     zinc_mg/nin_ear$zinc_mg,
                     1)
    
  ) %>% 
  dplyr::mutate(mar = (vita_mg+vitb1_mg+vitb2_mg+
                         vitb3_mg+vitb6_mg+folate_ug+
                         vitaminb12_in_mg+ calcium_mg+iron_mg+zinc_mg)/10) %>%
  srvyr::as_survey_design(id = HHID, strata = District_code,
                          weights = Combined_multiplier, nest=T)
# 
# intake <- base_model %>%
#   dplyr::left_join(
#     household_characteristics %>% 
#       dplyr::select(
#         HHID, Combined_multiplier, HH_Type_code
#       ), by = "HHID"
#   ) %>% 
#   srvyr::as_survey_design(id = HHID, strata = District_code,
#                           weights = Combined_multiplier, nest=T) %>% 
#   srvyr::group_by(District_code) %>% 
#   srvyr::summarise(
#     srvyr::across(-c(HHID,State_code, State_name,Combined_multiplier,HH_Type_code),
#                   ~mean(.))
#   ) %>% 
#   dplyr::mutate(District_code = as.numeric(District_code)) %>% 
#   dplyr::left_join(
#     india_adm2, 
#     by = c("District_code" = "Dstrct_c")
#   ) %>% 
#   dplyr::ungroup() %>% 
#   st_as_sf()

# sw_mean_intake_district_tot <- sw_mean_intake %>% 
#   srvyr::group_by(District_code) %>% 
#   srvyr::summarise(
#     srvyr::across(-c(HHID,State_code, State_name,Combined_multiplier,HH_Type_code),
#                   ~median(.))
#   ) %>% 
#   dplyr::mutate(District_code = as.numeric(District_code)) %>% 
#   dplyr::left_join(
#     india_adm2, 
#     by = c("District_code" = "Dstrct_c")
#   ) %>% 
#   dplyr::ungroup() %>% 
#   st_as_sf()



# showing which states we are using

india_sp <- india_adm1 %>%
  dplyr::mutate(
    state = ifelse(NAME_1%in%c("Uttar Pradesh", "Bihar", "Chhattisgarh"
                               # "Odisha"
    ),
    1,
    0
    )) %>% 
  st_as_sf()



states_sp <- india_sp %>% 
  dplyr::filter(state == 1)

# Create maps: 
india_co <- tm_shape(india_sp) + 
  tm_fill(col = "state") +
  tm_layout(main.title = "", frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_legend(show = F)

state_outline <- tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1) +
  tm_legend(show = F)



energy_adm2 <- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "energy_kcal", style = "cont", breaks = seq(0,1,by=.10),
          # c(1000,1550,2100,2650,3200),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Energy (kcal)")+
  tm_layout(main.title =  "Energy", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

energy_adm2

# fortifcation scenario

folate_mcg_adm2<- tm_shape(sw_mean_intake_district_tot) + 
  tm_fill(col = "folate_ug", style = "cont", breaks = seq(0,1,by=.10),
          # c(1000,1550,2100,2650,3200),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Folate")+
  tm_layout(main.title =  "Folate no fortification", 
            frame = F,
            main.title.size = 0.8) +
  tm_borders(col = "black", lwd = 0)+
  tm_shape(states_sp) + 
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

folate_mcg_adm2
