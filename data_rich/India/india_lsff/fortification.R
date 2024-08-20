#########################################
#        INDIA LSFF RESEARCH            #
#########################################


# Author: Gabriel Battcock
# Created: 22 Feb 24
# Last updated: 10 July 24

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
# ------------------------------------------------------------------------------
# read in the base case and read in the fortifiable food vehicles 

ind_ai <- apparent_intake("ind_nss1112", here::here("data_rich/India/data/processed/lsff//"))
ind_hh_info <- household_data("ind_nss1112", here::here("data_rich/India/data/processed/lsff//"))
ind_all_items <- full_item_list("ind_nss1112", here::here("data_rich/India/data/processed/lsff//"))
ind_vehicles <- read.csv(here::here("../ind_nss1112_vehicle_quantities.csv"))
ind_fct <- fc_table

base_model <- read_csv(here::here("data_rich/India/data/final/extra_states/base_model.csv"))
india_adm2 <- st_read(here::here("~/../General - MIMI Project/Nutrition analysis/shapefiles/ind_lss1819_adm2.shp"))
india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))

# plot(india_adm2$geometry)
# plot(india_adm1$geometry)

ind_nss1112_consumption <- food_consumption


# india specific requirement data data

nin_ear <- data.frame(
  nutrient = c("energy_kcal",
               "vita_rae_mcg",
               "thia_mg",
               "ribo_mg",
               "niac_mg",
               "vitb6_mg",
               "folate_mcg",
               "vitb12_mcg",
               "fe_mg",
               "ca_mg",
               "zn_mg"
  ),
  ear_value = c(
    2130,
    390,
    1.4,
    2.0,
    12,
    1.6,
    180,
    2,
    15,
    800,
    11
  ),
  tul_value = c(
    NA,
    3000,
    NA,
    NA,
    35,
    100,
    1000,
    NA,
    45,
    2500,
    40
  )
)


# filter only the states we are interested in
ind_vehicles <- ind_vehicles %>% 
  left_join(ind_hh_info) %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28))


# Current Indian fortification specification matched to PDS 
ind_fort_spec <- 
  data.frame(
    # items all per 100g
    item_code = c(101,107),
    vita_rae_mcg_fort = c(62.5,62.5),
    thia_mg_fort = c(0.125,0.125),
    ribo_mg_fort = c(0.15,0.15),
    niac_mg_fort = c(1.575,1.575),
    vitb6_mg_fort = c(0.2,0.2),
    folate_mcg_fort = c(10,10),
    vitb12_mcg_fort = c(0.1,0.1),
    fe_mg_fort = c(3.525,3.525),
    zn_mg_fort = c(1.25,1.25)
    
  )

#WHO and WFP fortification specs

who_fort_spec <- 
  data.frame(
    # items all per 100g
    item_code = c(101,107),
    vita_rae_mcg_fort = c(150,300),
    thia_mg_fort = c(0.5,0.3),
    ribo_mg_fort = c(NA,0.2),
    niac_mg_fort = c(7,4),
    vitb6_mg_fort = c(0.6,0.2),
    folate_mcg_fort = c(130,260),
    vitb12_mcg_fort = c(1,0.1),
    fe_mg_fort = c(4,4),
    zn_mg_fort = c(6,5.5)
    
  )

# Functions 


ind_fort_prev <- function(data,group) {
  #create a data frame summarising proportion of a group that is inadequate
  output <- data %>% 
    srvyr::group_by({{group}}) %>% 
    srvyr::summarise(
      srvyr::across(
        contains("_inad"),
        ~srvyr::survey_mean(
          .x == 1, proportion = TRUE, na.rm = TRUE
        )
      )
    )
  return(output)
}

ind_prev_maps <- function(data, micronutrient, adm){
  #creates inadequacy maps at either adm1 or adm2
  
  inad_prev_adm <- ind_fort_prev(data, {{adm}})
  # print(inad_prev_adm)
  if("adm1" %in% colnames(inad_prev_adm)){
    shapefile = inad_prev_adm %>%
      left_join(india_adm1, by = c('adm1'='State_code'))
  }
  if("adm2" %in% colnames(inad_prev_adm) ){
    shapefile = inad_prev_adm %>%
      left_join(india_adm2, by = c('adm2'='adm2_code'))
  }
  
  shapefile %>%
    st_as_sf() %>%
    tm_shape() +
    tm_fill(col = {{micronutrient}}, style = "cont",
            breaks = seq(0,1,by=.10),
            palette = (wesanderson::wes_palette("Zissou1Continuous")),
            title = "" ,
            legend.is.portrait = FALSE
    ) +
    tm_layout(main.title = , frame = F,
              main.title.size = 0.8,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35
    ) +
    tm_borders(col = "black", lwd = 0) +
    tm_shape(india_adm1) +
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = F)
  
}



fortification_inadequacy <- function(fortification_model ){
  output_df <- fortification_model %>% 
    mutate(vita_ear = nin_ear$ear_value[nin_ear$nutrient == "vita_rae_mcg"],
           thia_ear = nin_ear$ear_value[nin_ear$nutrient == "thia_mg"],
           ribo_ear = nin_ear$ear_value[nin_ear$nutrient == "ribo_mg"],
           niac_ear = nin_ear$ear_value[nin_ear$nutrient == "niac_mg"],
           vitb6_ear = nin_ear$ear_value[nin_ear$nutrient == "vitb6_mg"],
           folate_ear = nin_ear$ear_value[nin_ear$nutrient == "folate_mcg"],
           vitb12_ear = nin_ear$ear_value[nin_ear$nutrient == "vitb12_mcg"],
           fe_ear = nin_ear$ear_value[nin_ear$nutrient == "fe_mg"],
           zn_ear  = nin_ear$ear_value[nin_ear$nutrient == "zn_mg"],
           #upper limit
           vita_tul = nin_ear$tul_value[nin_ear$nutrient == "vita_rae_mcg"],
           niac_tul = nin_ear$tul_value[nin_ear$nutrient == "niac_mg"],
           vitb6_tul = nin_ear$tul_value[nin_ear$nutrient == "vitb6_mg"],
           folate_tul = nin_ear$tul_value[nin_ear$nutrient == "folate_mcg"],
           fe_tul = nin_ear$tul_value[nin_ear$nutrient == "fe_mg"],
           zn_tul  = nin_ear$tul_value[nin_ear$nutrient == "zn_mg"],
           
           vita_above = ifelse(vita_rae_mcg>vita_tul,1,0),
           niac_above = ifelse(niac_mg>niac_tul, 1,0),
           vitb6_above= ifelse(vitb6_mg>vitb6_tul,1,0),
           folate_above = ifelse(folate_mcg>folate_tul,1,0),
           fe_above = ifelse(fe_mg>fe_tul,1,0),
           zn_above = ifelse(zn_mg>zn_tul,1,0),
           
           #un_fortified
           vita_inad = ifelse(vita_rae_mcg<vita_ear,1,0),
           thia_inad = ifelse(thia_mg<thia_ear,1,0),
           ribo_inad = ifelse(ribo_mg<ribo_ear,1,0),
           niac_inad = ifelse(niac_mg<niac_ear,1,0),
           vitb6_inad = ifelse(vitb6_mg<vitb6_ear,1,0),
           folate_inad = ifelse(folate_mcg<folate_ear,1,0),
           vitb12_inad = ifelse(vitb12_mcg<vitb12_ear,1,0),
           zn_inad = ifelse(zn_mg<zn_ear,1,0),
           
           #fortified
           vita_fort_inad = ifelse(vita_rae_mcg_fort<vita_ear,1,0),
           thia_fort_inad = ifelse(thia_mg_fort<thia_ear,1,0),
           ribo_fort_inad = ifelse(ribo_mg_fort<ribo_ear,1,0),
           niac_fort_inad = ifelse(niac_mg_fort<niac_ear,1,0),
           vitb6_fort_inad = ifelse(vitb6_mg_fort<vitb6_ear,1,0),
           folate_fort_inad = ifelse(folate_mcg_fort<folate_ear,1,0),
           vitb12_fort_inad = ifelse(vitb12_mcg_fort<vitb12_ear,1,0),
           zn_fort_inad = ifelse(zn_mg_fort<zn_ear,1,0),
           
           #nutrient adequacy ratio unfort
           vita_nar = ifelse(vita_rae_mcg<vita_ear,vita_rae_mcg/vita_ear,1),
           thia_nar = ifelse(thia_mg<thia_ear,thia_mg/thia_ear,1),
           ribo_nar = ifelse(ribo_mg<ribo_ear,ribo_mg/ribo_ear,1),
           niac_nar = ifelse(niac_mg<niac_ear,niac_mg/niac_ear,1),
           vitb6_nar = ifelse(vitb6_mg<vitb6_ear,vitb6_mg/vitb6_ear,1),
           folate_nar = ifelse(folate_mcg<folate_ear,folate_mcg/folate_ear,1),
           vitb12_nar = ifelse(vitb12_mcg<vitb12_ear,vitb12_mcg/vitb12_ear,1),
           zn_nar = ifelse(zn_mg<zn_ear,zn_mg/zn_ear,1),
           fe_nar = ifelse(fe_mg<fe_ear,fe_mg/fe_ear,1),
           
           #nutrient adequacy ratio fort
           vita_fort_nar = ifelse(vita_rae_mcg_fort<vita_ear,vita_rae_mcg_fort/vita_ear,1),
           thia_fort_nar = ifelse(thia_mg_fort<thia_ear,thia_mg_fort/thia_ear,1),
           ribo_fort_nar = ifelse(ribo_mg_fort<ribo_ear,ribo_mg_fort/ribo_ear,1),
           niac_fort_nar = ifelse(niac_mg_fort<niac_ear,niac_mg_fort/niac_ear,1),
           vitb6_fort_nar = ifelse(vitb6_mg_fort<vitb6_ear,vitb6_mg_fort/vitb6_ear,1),
           folate_fort_nar = ifelse(folate_mcg_fort<folate_ear,folate_mcg_fort/folate_ear,1),
           vitb12_fort_nar = ifelse(vitb12_mcg_fort<vitb12_ear,vitb12_mcg_fort/vitb12_ear,1),
           zn_fort_nar = ifelse(zn_mg_fort<zn_ear,zn_mg_fort/zn_ear,1),
           fe_fort_nar = ifelse(fe_mg_fort<fe_ear,fe_mg_fort/fe_ear,1),
           
           # mean adequacy ratio non-fort
           mar = (vita_nar+thia_nar+ribo_nar+niac_nar+vitb6_nar+folate_nar+vitb12_nar+
                    fe_nar+zn_nar)/9,
           
           # mean adequacy ratio fort
           mar_fort = (vita_fort_nar+thia_fort_nar+ribo_fort_nar+niac_fort_nar+
                         vitb6_fort_nar+folate_fort_nar+vitb12_fort_nar+fe_fort_nar+
                         zn_fort_nar)/9
           
    ) %>% 
    as_survey_design(ids = hhid, weights = survey_wgt, strata = adm2) 
  
  return(output_df)
}


# Fortification all rice ----------------------------------------------------------------

ind_ai %>% left_join(ind_hh_info) %>% 
  
  
  ################################################################################
################################################################################

ind_all_fort_add <- ind_vehicles %>%
  # read in commercial fortifiable rice
  select(hhid,rice_100g,wheatflour_100g) %>% 
  pivot_longer(cols = c(rice_100g,wheatflour_100g)) %>% 
  rename(item_code = name,
         quantity_100g = value) %>% 
  mutate(item_code = case_when(
    # match the grains to fortifiable food items in the FCT
    
    item_code ==  "rice_100g" ~ 101,
    item_code == "wheatflour_100g"~ 107
  )) %>% 
  left_join(
    ind_fort_spec, by= "item_code"
  ) %>% 
  mutate(across(
    -c(hhid,item_code,quantity_100g),
    ~.x*quantity_100g
    
  ))


#### RICE fortification 

fort_model_rice <- ind_ai %>% 
  left_join(ind_hh_info, by = c('hhid','afe')) %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  left_join(ind_all_fort_add %>% 
              filter(item_code == 101) %>% # rice item code
              select(-c(item_code,quantity_100g)),
            by = "hhid") %>% 
  mutate(across(everything(),~ifelse(is.na(.), 0,.))) %>% 
  mutate(vita_rae_mcg_fort = vita_rae_mcg+vita_rae_mcg_fort,
         thia_mg_fort = thia_mg+thia_mg_fort,
         ribo_mg_fort = ribo_mg+ribo_mg_fort,
         niac_mg_fort = niac_mg+niac_mg_fort,
         vitb6_mg_fort = vitb6_mg+vitb6_mg_fort,
         folate_mcg_fort = folate_mcg+folate_mcg_fort,
         vitb12_mcg_fort = vitb12_mcg+vitb12_mcg_fort,
         fe_mg_fort = fe_mg+fe_mg_fort,
         zn_mg_fort = zn_mg+zn_mg_fort)%>% 
  filter(energy_kcal <= quantile(energy_kcal,0.99)) 


# wheat flour fortification

fort_model_wheat <- ind_ai %>% 
  left_join(ind_hh_info, by = c('hhid','afe')) %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  left_join(ind_all_fort_add %>% 
              filter(item_code == 107) %>%
              select(-c(item_code,quantity_100g)),
            by = "hhid") %>% 
  mutate(across(everything(),~ifelse(is.na(.), 0,.))) %>% 
  mutate(vita_rae_mcg_fort = vita_rae_mcg+vita_rae_mcg_fort,
         thia_mg_fort = thia_mg+thia_mg_fort,
         ribo_mg_fort = ribo_mg+ribo_mg_fort,
         niac_mg_fort = niac_mg+niac_mg_fort,
         vitb6_mg_fort = vitb6_mg+vitb6_mg_fort,
         folate_mcg_fort = folate_mcg+folate_mcg_fort,
         vitb12_mcg_fort = vitb12_mcg+vitb12_mcg_fort,
         fe_mg_fort = fe_mg+fe_mg_fort,
         zn_mg_fort = zn_mg+zn_mg_fort)%>% 
  filter(energy_kcal <= quantile(energy_kcal,0.99)) 


# both vehicles fortification

fort_model_all <- ind_ai %>% 
  left_join(ind_hh_info, by = c('hhid','afe')) %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  left_join(ind_all_fort_add %>% 
              select(-c(item_code,quantity_100g)) %>% 
              group_by(hhid) %>% 
              summarise(across(everything(),
                               ~sum(.,na.rm = TRUE))),
            by = "hhid") %>% 
  mutate(across(everything(),~ifelse(is.na(.), 0,.))) %>% 
  mutate(vita_rae_mcg_fort = vita_rae_mcg+vita_rae_mcg_fort,
         thia_mg_fort = thia_mg+thia_mg_fort,
         ribo_mg_fort = ribo_mg+ribo_mg_fort,
         niac_mg_fort = niac_mg+niac_mg_fort,
         vitb6_mg_fort = vitb6_mg+vitb6_mg_fort,
         folate_mcg_fort = folate_mcg+folate_mcg_fort,
         vitb12_mcg_fort = vitb12_mcg+vitb12_mcg_fort,
         fe_mg_fort = fe_mg+fe_mg_fort,
         zn_mg_fort = zn_mg+zn_mg_fort) %>% 
  filter(energy_kcal <= quantile(energy_kcal,0.99)) 


# prevelence of inadequacy maps
# if a household takes a proportion of hosueholds who do not meet the estimated
# average requirement

fort_model_rice <-fortification_inadequacy(fort_model_rice)

fort_model_wheat <- fortification_inadequacy(fort_model_wheat)

fort_model_all <- fortification_inadequacy(fort_model_all) 


# base risk
vita_map <- ind_prev_maps(fort_model_rice,"vita_inad") + tm_layout(main.title = "Vitamin A - No fortification")
thia_map <- ind_prev_maps(fort_model_rice,"thia_inad")+ tm_layout(main.title = "Thiamin - No fortification")
ribo_map <- ind_prev_maps(fort_model_rice,"ribo_inad")+ tm_layout(main.title = "Riboflavin - No fortification")
niac_map <- ind_prev_maps(fort_model_rice,"niac_inad")+ tm_layout(main.title = "Niacin - No fortification")
vb6_map <- ind_prev_maps(fort_model_rice,"vitb6_inad")+ tm_layout(main.title = "Vitamin B6 - No fortification")
fol_map <- ind_prev_maps(fort_model_rice,"folate_inad")+ tm_layout(main.title = "Folate - No fortification")
vb12_map <- ind_prev_maps(fort_model_rice,"vitb12_inad")+ tm_layout(main.title = "Vitamin B12 - No fortification")
zn_map <- ind_prev_maps(fort_model_rice,"zn_inad")+ tm_layout(main.title = "Zinc - No fortification")

# rice fort map
vita_rice_fort_map <- ind_prev_maps(fort_model_rice,"vita_fort_inad")+ tm_layout(main.title = "Vitamin A - Rice fortification")
thia_rice_fort_map <- ind_prev_maps(fort_model_rice,"thia_fort_inad")+ tm_layout(main.title = "Thiamin - Rice fortification")
ribo_rice_fort_map <- ind_prev_maps(fort_model_rice,"ribo_fort_inad")+ tm_layout(main.title = "Riboflavin - Rice fortification")
niac_rice_fort_map <- ind_prev_maps(fort_model_rice,"niac_fort_inad")+ tm_layout(main.title = "Niacin - Rice fortification")
vb6_rice_fort_map <- ind_prev_maps(fort_model_rice,"vitb6_fort_inad")+ tm_layout(main.title = "Vitamin B6 - Rice fortification")
fol_rice_fort_map <- ind_prev_maps(fort_model_rice,"folate_fort_inad")+ tm_layout(main.title = "Folate - Rice fortification")
vb12_rice_fort_map <- ind_prev_maps(fort_model_rice,"vitb12_fort_inad")+ tm_layout(main.title = "Vitamin B12 - Rice fortification")
zn_rice_fort_map <- ind_prev_maps(fort_model_rice,"zn_fort_inad")+ tm_layout(main.title = "Zinc - Rice fortification")

vita_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"vita_fort_inad")+ tm_layout(main.title = "Vitamin A - Wheat flour fortification")
thia_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"thia_fort_inad")+ tm_layout(main.title = "Thiamin - Wheat flour fortification")
ribo_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"ribo_fort_inad")+ tm_layout(main.title = "Riboflavin - Wheat flour fortification")
niac_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"niac_fort_inad")+ tm_layout(main.title = "Niacin - Wheat flour fortification")
vb6_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"vitb6_fort_inad")+ tm_layout(main.title = "Vitamin B6 - Wheat flour fortification")
fol_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"folate_fort_inad")+ tm_layout(main.title = "Folate- Wheat flour fortification")
vb12_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"vitb12_fort_inad")+ tm_layout(main.title = "Vitamin B12 - Wheat flour fortification")
zn_wheat_fort_map <- ind_prev_maps(fort_model_wheat,"zn_fort_inad")+ tm_layout(main.title = "Zinc - Wheat flour fortification")

vita_all_fort_map <- ind_prev_maps(fort_model_all,"vita_fort_inad")+ tm_layout(main.title = "Vitamin A - Both vehicles")
thia_all_fort_map <- ind_prev_maps(fort_model_all,"thia_fort_inad")+ tm_layout(main.title = "Thiamin - Both vehicles")
ribo_all_fort_map <- ind_prev_maps(fort_model_all,"ribo_fort_inad")+ tm_layout(main.title = "Riboflavin - Both vehicles")
niac_all_fort_map <- ind_prev_maps(fort_model_all,"niac_fort_inad")+ tm_layout(main.title = "Niacin - Both vehicles")
vb6_all_fort_map <- ind_prev_maps(fort_model_all,"vitb6_fort_inad")+ tm_layout(main.title = "Vitamin B6 - Both vehicles")
fol_all_fort_map <- ind_prev_maps(fort_model_all,"folate_fort_inad")+ tm_layout(main.title = "Folate - Both vehicles")
vb12_all_fort_map <- ind_prev_maps(fort_model_all,"vitb12_fort_inad")+ tm_layout(main.title = "Vitamin B12 - Both vehicles")
zn_all_fort_map <- ind_prev_maps(fort_model_all,"zn_fort_inad")+ tm_layout(main.title = "Zinc - Both vehicles")



path_to_save <- "../Presentations/inda_lsff/figures/"

tmap_save(vita_map, paste0(path_to_save, "vita_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_map, paste0(path_to_save, "thia_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(ribo_map, paste0(path_to_save, "ribo_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_map, paste0(path_to_save, "niac_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb6_map, paste0(path_to_save, "vitb6_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(fol_map, paste0(path_to_save, "folate_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb12_map, paste0(path_to_save, "vitb12_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zn_map, paste0(path_to_save, "zn_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)



tmap_save(vita_rice_fort_map, paste0(path_to_save, "vita_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_rice_fort_map, paste0(path_to_save, "thia_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(ribo_rice_fort_map, paste0(path_to_save, "ribo_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_rice_fort_map, paste0(path_to_save, "niac_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb6_rice_fort_map, paste0(path_to_save, "vitb6_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(fol_rice_fort_map, paste0(path_to_save, "folate_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb12_rice_fort_map, paste0(path_to_save, "vitb12_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zn_rice_fort_map, paste0(path_to_save, "zn_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(vita_wheat_fort_map, paste0(path_to_save, "vita_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_wheat_fort_map, paste0(path_to_save, "thia_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(ribo_wheat_fort_map, paste0(path_to_save, "ribo_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_wheat_fort_map, paste0(path_to_save, "niac_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb6_wheat_fort_map, paste0(path_to_save, "vitb6_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(fol_wheat_fort_map, paste0(path_to_save, "folate_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb12_wheat_fort_map, paste0(path_to_save, "vitb12_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zn_wheat_fort_map, paste0(path_to_save, "zn_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)



tmap_save(vita_all_fort_map, paste0(path_to_save, "vita_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_all_fort_map, paste0(path_to_save, "thia_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(ribo_all_fort_map, paste0(path_to_save, "ribo_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_all_fort_map, paste0(path_to_save, "niac_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb6_all_fort_map, paste0(path_to_save, "vitb6_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(fol_all_fort_map, paste0(path_to_save, "folate_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vb12_all_fort_map, paste0(path_to_save, "vitb12_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zn_all_fort_map, paste0(path_to_save, "zn_all.png"),
          width = 6, height = 6, units = "in", dpi = 600)
# 

mar_com = fortification_inadequacy(fort_model_rice) %>% 
  group_by(adm2) %>% 
  summarise(survey_mean(mar)) %>% 
  left_join(india_adm2, by = c("adm2" = "adm2_code")) %>% 
  st_as_sf() %>%
  tm_shape() +
  tm_fill(col = "coef", style = "cont",
          breaks = seq(0.6,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "",
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Base case", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(india_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)


tmap_save(mar_com, paste0(path_to_save, "mar_com.png"),
          width = 6, height = 6, units = "in", dpi = 600)





# calculate the difference in prevalence of inadequacy 


diff_prev <- ind_fort_prev(fort_model_rice, adm1) %>% 
  rename_with(
    .fn = function(x) paste0(x, "_rice"),
    .cols = contains("fort")
  ) %>% 
  select(-contains("_se")) %>% 
  left_join(
    ind_fort_prev(fort_model_wheat, adm1) %>% 
      rename_with(
        .fn = function(x) paste0(x, "_wf"),
        .cols = contains("fort")
      ) %>% 
      select(-contains("_se"),
             -ends_with("_inad")) ,
    by = "adm1"
    
  ) %>% 
  left_join(
    ind_fort_prev(fort_model_all, adm1) %>% 
      rename_with(
        .fn = function(x) paste0(x, "_all"),
        .cols = contains("fort")
      ) %>% 
      select(-contains("_se"),
             -ends_with("_inad")) ,
    by = "adm1"
  )



create_dumbell_mn_plot <- function(micronutrient){
  diff_db <- diff_prev %>% 
    select(
      adm1,
      starts_with({{micronutrient}})
    ) %>% 
    pivot_longer(
      cols = 2:5
    )
  
  db_range <- diff_db %>% 
    group_by(adm1,name) %>% 
    summarise(
      db_low = min(value),
      db_high = max(value)
    )
  
  final_plot <- diff_db %>% 
    left_join(db_range) %>% 
    mutate(
      adm1 = case_when(
        adm1 == 2 ~ "Himachal Pradesh",
        adm1 == 3 ~ "Punjab",
        adm1 == 6 ~ "Haryana",
        adm1 == 8 ~ "Rajasthan",
        adm1 == 9 ~ "Uttar Pradesh",
        adm1 == 10 ~ "Bihar",
        adm1 == 19 ~ "West Bengal",
        adm1 == 22 ~ "Chhattisgarh",
        adm1 == 20 ~ "Jharkhand",
        adm1 == 21 ~ "Orissa",
        adm1 == 23 ~ "Madhya Pradesh",
        adm1 == 28 ~ "Andhra Pradesh"
      ),
      Scenario = case_when(
        
        grepl( "_rice",name) ~ "Rice fortification",
        grepl( "_wf",name)~ "Wheat flour fortification",
        grepl("_all",name) ~ "Both vehicles", 
        .default = "Base case"
      )
      
    ) %>% 
    ggplot() +
    geom_pointrange(aes(x = adm1, y = value*100, 
                        ymin = db_low*100, ymax = db_high*100, 
                        colour = Scenario,
                        shape = Scenario
    ))+ 
    theme_bw() +
    #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
    coord_flip(ylim = c(0, 100)) +
    ylim(0,100)+
    theme(legend.position = "bottom")+
    ylab("Inadequate intake (%) ")+
    xlab("State")
  return(final_plot)
  
}

vita_db = create_dumbell_mn_plot("vita")+labs(title = "Vitamin A")
thia_db = create_dumbell_mn_plot("thia")+labs(title = "Thiamin")
# ribo_db = create_dumbell_mn_plot("ribo")+labs(title = "Riboflavin")
niac_db = create_dumbell_mn_plot("niac")+labs(title = "Niacin")
vitb6_db = create_dumbell_mn_plot("vitb6")+labs(title = "Vitamin B6")
folate_db = create_dumbell_mn_plot("fol")+labs(title = "Folate")
vitb12_db = create_dumbell_mn_plot("vitb12")+labs(title = "Vitamin B12")
zn_db = create_dumbell_mn_plot("zn")+labs(title = "Zinc")

ggsave(paste0(path_to_save, "vita_db.png") ,plot = vita_db,
       width = 7, height = 5, units = "in", dpi = 600)
ggsave(paste0(path_to_save, "thia_db.png") ,plot = thia_db,
       width = 7, height = 5, units = "in", dpi = 600)
ggsave(paste0(path_to_save, "niac_db.png") ,plot = niac_db,
       width = 7, height = 5, units = "in", dpi = 600)
ggsave(paste0(path_to_save, "vitb6_db.png") ,plot = vitb6_db,
       width = 7, height = 5, units = "in", dpi = 600)
ggsave(paste0(path_to_save, "folate_db.png") ,plot = folate_db,
       width = 7, height = 5, units = "in", dpi = 600)
ggsave(paste0(path_to_save, "vitb12_db.png") ,plot = vitb12_db,
       width = 7, height = 5, units = "in", dpi = 600)
ggsave(paste0(path_to_save, "zn_db.png") ,plot = zn_db,
       width = 7, height = 5, units = "in", dpi = 600)


diff_prev_res <- ind_fort_prev(fort_model_rice, res) %>% 
  rename_with(
    .fn = function(x) paste0(x, "_rice"),
    .cols = contains("fort")
  ) %>% 
  select(-contains("_se")) %>% 
  left_join(
    ind_fort_prev(fort_model_wheat, adm1) %>% 
      rename_with(
        .fn = function(x) paste0(x, "_wf"),
        .cols = contains("fort")
      ) %>% 
      select(-contains("_se"),
             -ends_with("_inad")) ,
    by = "adm1"
    
  ) %>% 
  left_join(
    ind_fort_prev(fort_model_all, adm1) %>% 
      rename_with(
        .fn = function(x) paste0(x, "_all"),
        .cols = contains("fort")
      ) %>% 
      select(-contains("_se"),
             -ends_with("_inad")) ,
    by = "adm1"
  )

## reach of wheat and rice -----------------------------------------------------

# create reach 
ind_reach <- ind_vehicles %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  as_survey_design(ids= hhid, weights = survey_wgt, strata = adm2) %>% 
  group_by(adm1) %>% 
  summarise(
    rice = survey_mean(rice == "Yes", proportion = TRUE),
    wheatflour = survey_mean(wheatflour=="Yes", proportion = TRUE)
  )

#isolate rice FINISH
ind_rice_adm1 <- ind_reach %>% 
  left_join(india_adm1, by = c('adm1'='State_code')) %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_fill(col = "rice", style = "cont",
          breaks = seq(0,1,by=.10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Rice reach", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(india_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

ind_rice_adm1 <- ind_reach %>% 
  left_join(india_adm1, by = c('adm1'='State_code')) %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_fill(col = "rice", style = "cont",
          breaks = seq(0,1,by=.10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Rice reach", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(india_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

# isolate wheat flour (atta)
ind_wf_adm1 <- ind_reach %>% 
  left_join(india_adm1, by = c('adm1'='State_code')) %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_fill(col = "wheatflour", style = "cont",
          breaks = seq(0,1,by=.10),
          # palette = (wesanderson::wes_palette("Zissou1Continuous")),
          title = "" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "Wheat flour reach", frame = F,
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(india_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)

# save the maps

tmap_save(ind_rice_adm1, paste0(path_to_save, "rice_reach_adm1_small.png"),
          width = 2, height = 2, units = "in", dpi = 600)
tmap_save(ind_wf_adm1, paste0(path_to_save, "wheatflour_adm1_small.png"),
          width = 2, height = 2, units = "in", dpi = 600)



# plot of reach and pc consumption

rice_reach <- ind_vehicles %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  as_survey_design(ids= hhid, weights = survey_wgt, strata = adm2) %>% 
  group_by(res,res_quintile) %>% 
  summarise(
    rice_100g = srvyr::survey_mean(rice_100g, na.rm =T)*100,
    wheatflour_100g =  srvyr::survey_mean(wheatflour_100g, na.rm =T)*100,
    rice_reach = srvyr::survey_mean(rice == "Yes", proportion = TRUE)*100,
    wheat_reach = srvyr::survey_mean(wheatflour == "Yes", proportion = TRUE)*100,
  ) %>% 
  mutate(res_quintile = paste(res,res_quintile)) %>% 
  ggplot(aes(x = rice_reach, y = res_quintile, size = rice_100g, col = res))+
  geom_point()+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlim(50,100) +
  xlab("Reach (%)")+
  ylab("Wealth Quintile")+
  labs(
    col  = "Residence",
    size = "Consumption (g)",
    title = "Rice"
  )+
  scale_color_brewer(palette = "Set1") 


ggsave(paste0(path_to_save, "rice_reach.png") ,plot = rice_reach,
       width = 5, height = 5, units = "in", dpi = 600)


wf_reach <- ind_vehicles %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  as_survey_design(ids= hhid, weights = survey_wgt, strata = adm2) %>% 
  group_by(res,res_quintile) %>% 
  summarise(
    rice_100g = srvyr::survey_mean(rice_100g, na.rm =T)*100,
    wheatflour_100g =  srvyr::survey_mean(wheatflour_100g, na.rm =T)*100,
    rice_reach = srvyr::survey_mean(rice == "Yes", proportion = TRUE)*100,
    wheat_reach = srvyr::survey_mean(wheatflour == "Yes", proportion = TRUE)*100,
  ) %>% 
  mutate(res_quintile = paste(res,res_quintile)) %>% 
  ggplot(aes(x = wheat_reach, y = res_quintile, size = wheatflour_100g, col = res))+
  geom_point()+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlim(50,100) +
  xlab("Reach (%)")+
  ylab("Wealth Quintile")+
  labs(
    col  = "Residence",
    size = "consumption (g)",
    title = "Wheat flour"
  )+
  scale_color_brewer(palette = "Set1") 

ggsave(paste0(path_to_save, "wf_reach.png") ,plot = wf_reach,
       width = 5, height = 5, units = "in", dpi = 600)


ind_vehicles %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  as_survey_design(ids= hhid, weights = survey_wgt, strata = adm2) %>% 
  group_by(adm1) %>% 
  summarise(
    rice_100g = srvyr::survey_mean(rice_100g, na.rm =T)*100,
    wheatflour_100g =  srvyr::survey_mean(wheatflour_100g, na.rm =T)*100,
    rice_reach = srvyr::survey_mean(rice == "Yes", proportion = TRUE)*100,
    wheat_reach = srvyr::survey_mean(wheatflour == "Yes", proportion = TRUE)*100,
  ) %>% 
  # mutate(res_quintile = paste(res,res_quintile)) %>% 
  ggplot(aes(x = rice_reach, y = adm1, size = rice_100g, col = adm1))+
  geom_point()+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlim(50,100)

################################################################################
################################################################################
##                        PDS FORTIFICATION                                   ##         
################################################################################
################################################################################


# remove pds rice receive and add back new amounts

# ind_pds_eligible_pl <- get_eligible_households(pl = FALSE)


# this block of code deletes any pds rice and wheat from item list
# Then adds in the new amount of rice and wheat flour from state pds 

# ind_remove_pds<- ind_all_items %>% 
#     filter(hhid %in% ind_pds_eligible_pl$hhid) %>% 
#     select(hhid,afe, item_code,item_name, quantity_g, quantity_100g) %>% 
#     mutate(quantity_g = ifelse(item_code %in% c(101,107), 0, quantity_g),
#            quantity_100g = ifelse(item_code %in% c(101,107), 0, quantity_100g)) %>% 
#     left_join(ind_pds_eligible_pl %>% select(hhid,adm1,eligible), by = 'hhid')

# ind_pds_items <-ind_remove_pds %>% 
#     mutate(
#       #elgibile household get a certain amount of grain
#       quantity_g = ifelse(eligible == 1, case_when(
#         adm1 == "Punjab" & item_code == 107 ~ 35000/(afe*30),
#         adm1 == "Haryana"& item_code == 107 ~ 35000/(afe*30),
#         adm1 == "Rajasthan"& item_code == 107 ~ 35000/(afe*30),
#         adm1 == "Chhattisgarh"& item_code == 101 ~ 35000/(afe*30),
#         adm1 == "Bihar"& item_code == 101 ~ 28000/(afe*30),
#         adm1 == "Bihar"& item_code == 107 ~ 7000/(afe*30),
#         adm1 == "UP"& item_code == 101 ~ 21000/(afe*30),
#         adm1 == "UP"& item_code == 107 ~ 14000/(afe*30),
#         .default = quantity_g
#       ), quantity_g)
#     ) %>% 
#     mutate(quantity_100g = ifelse(item_code %in% c(101,107), quantity_g/100,quantity_100g))
#   # filter(item_code == 101)




# match to fct with fortificant added ------------------------------------------

# look at only the states we're interested in 
ind_hh_info <- ind_hh_info %>% 
  filter(adm1 %in% c(3,6,8,9,10,22))

ind_all_items <- ind_all_items %>% 
  filter(hhid %in% ind_hh_info$hhid)

# find out the percentage of people in each state receiving PDS according to 
# the 2012 NSSO data

ind_recieved_pds <- ind_all_items %>% 
  filter(item_code %in% c(101,107)) %>% 
  select(hhid, afe, item_code,item_name, quantity_g, quantity_100g) %>% 
  mutate(recieved_pds = ifelse(quantity_g != 0, 1, 0)) %>% 
  left_join(ind_hh_info, by= c("hhid",'afe')) 


# overall percentage of pds
any_pds <- ind_recieved_pds %>% 
  group_by(hhid,adm2, survey_wgt, adm1,afe) %>% 
  summarise(
    recieved_pds = ifelse(sum(recieved_pds)>0, 1,0),
    quantity_g = sum(quantity_g)
  )%>%
  ungroup() %>% 
  srvyr::as_survey_design(ids = hhid, weights = survey_wgt, strata = adm2) %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    received_pds = survey_mean(recieved_pds ==1, proportion = TRUE)*100,
  ) %>% 
  left_join(
    #mean consumption of pds beneficiaries
    ind_recieved_pds %>% 
      group_by(hhid,adm2, survey_wgt, adm1,afe) %>% 
      summarise(
        recieved_pds = ifelse(sum(recieved_pds)>0, 1,0),
        quantity_g = sum(quantity_g)
      )%>%
      filter(recieved_pds ==1) %>% 
      ungroup() %>% 
      srvyr::as_survey_design(ids = hhid, weights = survey_wgt, strata = adm2) %>% 
      srvyr::group_by(adm1) %>% 
      srvyr::summarise(
        mean_consumption_g = survey_mean(quantity_g),
      ) , by= "adm1"
  )

rice_pds <- ind_recieved_pds %>% 
  filter(item_code == 101) %>% 
  group_by(hhid,adm2, survey_wgt, adm1,afe) %>% 
  summarise(
    recieved_pds = ifelse(sum(recieved_pds)>0, 1,0),
    quantity_g = sum(quantity_g)
  )%>%
  ungroup() %>% 
  srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    received_pds = survey_mean(recieved_pds ==1, proportion = TRUE)*100,
  ) %>% 
  left_join(
    #mean consumption of pds beneficiaries
    ind_recieved_pds %>% 
      filter(item_code == 101) %>% 
      group_by(hhid,adm2, survey_wgt, adm1,afe) %>% 
      summarise(
        recieved_pds = ifelse(sum(recieved_pds)>0, 1,0),
        quantity_g = sum(quantity_g)
      )%>%
      filter(recieved_pds ==1) %>% 
      ungroup() %>% 
      
      srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
      srvyr::group_by(adm1) %>% 
      srvyr::summarise(
        mean_consumption_g = survey_mean(quantity_g),
      ) , by= "adm1"
  )


wf_pds <- ind_recieved_pds %>% 
  filter(item_code == 107) %>% 
  group_by(hhid,adm2, survey_wgt, adm1,afe) %>% 
  summarise(
    recieved_pds = ifelse(sum(recieved_pds)>0, 1,0),
    quantity_g = sum(quantity_g)
  )%>%
  ungroup() %>% 
  srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    received_pds = survey_mean(recieved_pds ==1, proportion = TRUE)*100,
  ) %>% 
  left_join(
    #mean consumption of pds beneficiaries
    ind_recieved_pds %>% 
      filter(item_code == 107) %>% 
      group_by(hhid,adm2, survey_wgt, adm1,afe) %>% 
      summarise(
        recieved_pds = ifelse(sum(recieved_pds)>0, 1,0),
        quantity_g = sum(quantity_g)
      )%>%
      filter(recieved_pds ==1) %>% 
      ungroup() %>% 
      
      srvyr::as_survey_design(ids = hhid, weights = survey_wgt) %>% 
      srvyr::group_by(adm1) %>% 
      srvyr::summarise(
        mean_consumption_g = survey_mean(quantity_g),
      ) , by= "adm1"
  )





# create a new fct with pds items 
# we update the fct adding in contributions from fortification



create_pds_fct <- function(fortification_item = 3, fort_standards){
  # choose whether to fortify rice = 1, wheatflour =2, both = 3
  item_codes <-  case_when(
    fortification_item == 0 ~ NA,
    fortification_item == 1 ~ 101,
    fortification_item == 2 ~ 107,
    fortification_item == 3 ~ c(101,107)
  )
  
  # set fortification standards 
  
  
  # create a new fct with fortification items 
  new_fct <- ind_fct %>% 
    # filter(item_code %in% c(101,107)) %>% 
    left_join(fort_standards %>% 
                filter(item_code %in% item_codes), by = 'item_code') %>% 
    mutate(across(everything(),~ifelse(is.na(.), 0,.))) %>% 
    mutate(vita_rae_mcg_fort = vita_rae_mcg+vita_rae_mcg_fort,
           thia_mg_fort = thia_mg+thia_mg_fort,
           ribo_mg_fort = ribo_mg+ribo_mg_fort,
           niac_mg_fort = niac_mg+niac_mg_fort,
           vitb6_mg_fort = vitb6_mg+vitb6_mg_fort,
           folate_mcg_fort = folate_mcg+folate_mcg_fort,
           vitb12_mcg_fort = vitb12_mcg+vitb12_mcg_fort,
           fe_mg_fort = fe_mg+fe_mg_fort,
           zn_mg_fort = zn_mg+zn_mg_fort)
  
  return(new_fct)
}


ind_pds_fort_fct <- create_pds_fct(1, ind_fort_spec)

create_hh_inad <- function(fortification_item, fort_standards){
  # creates a survey object with every household having an
  # inadequacy bin variable for each MN
  
  ind_pds_fort_fct <- create_pds_fct(fortification_item, fort_standards)
  # create a sub total of each micronutrient hhid
  
  
  ind_pds_fort_intake <- ind_all_items %>% 
    select(hhid,afe, item_code,item_name, quantity_g, quantity_100g) %>% 
    inner_join(ind_pds_fort_fct, by =c( "item_code","item_name" )) %>% 
    mutate(
      across(
        -c(hhid,afe,item_name,item_code,quantity_g,quantity_100g),
        ~.x*quantity_100g
      )
    ) %>% 
    group_by(hhid) %>% 
    summarise(
      across(
        -c(afe,item_name,item_code,quantity_g,quantity_100g),
        ~sum(.,na.rm = TRUE)
      )
    ) %>% 
    ungroup() %>% 
    filter(energy_kcal <= quantile(energy_kcal,0.99)) 
  
  
  # pds fortification prevalence
  survey_inad <- ind_pds_fort_intake %>% 
    mutate(vita_ear = nin_ear$ear_value[nin_ear$nutrient == "vita_rae_mcg"],
           thia_ear = nin_ear$ear_value[nin_ear$nutrient == "thia_mg"],
           ribo_ear = nin_ear$ear_value[nin_ear$nutrient == "ribo_mg"],
           niac_ear = nin_ear$ear_value[nin_ear$nutrient == "niac_mg"],
           vitb6_ear = nin_ear$ear_value[nin_ear$nutrient == "vitb6_mg"],
           folate_ear = nin_ear$ear_value[nin_ear$nutrient == "folate_mcg"],
           vitb12_ear = nin_ear$ear_value[nin_ear$nutrient == "vitb12_mcg"],
           fe_ear = nin_ear$ear_value[nin_ear$nutrient == "fe_mg"],
           zn_ear  = nin_ear$ear_value[nin_ear$nutrient == "zn_mg"],
           
           #upper limit
           vita_tul = nin_ear$tul_value[nin_ear$nutrient == "vita_rae_mcg"],
           niac_tul = nin_ear$tul_value[nin_ear$nutrient == "niac_mg"],
           vitb6_tul = nin_ear$tul_value[nin_ear$nutrient == "vitb6_mg"],
           folate_tul = nin_ear$tul_value[nin_ear$nutrient == "folate_mcg"],
           fe_tul = nin_ear$tul_value[nin_ear$nutrient == "fe_mg"],
           zn_tul  = nin_ear$tul_value[nin_ear$nutrient == "zn_mg"],
           
           vita_above = ifelse(vita_rae_mcg>vita_tul,1,0),
           niac_above = ifelse(niac_mg>niac_tul, 1,0),
           vitb6_above= ifelse(vitb6_mg>vitb6_tul,1,0),
           folate_above = ifelse(folate_mcg>folate_tul,1,0),
           fe_above = ifelse(fe_mg>fe_tul,1,0),
           zn_above = ifelse(zn_mg>zn_tul,1,0),
           
           #un_fortified
           vita_inad = ifelse(vita_rae_mcg<vita_ear,1,0),
           thia_inad = ifelse(thia_mg<thia_ear,1,0),
           ribo_inad = ifelse(ribo_mg<ribo_ear,1,0),
           niac_inad = ifelse(niac_mg<niac_ear,1,0),
           vitb6_inad = ifelse(vitb6_mg<vitb6_ear,1,0),
           folate_inad = ifelse(folate_mcg<folate_ear,1,0),
           vitb12_inad = ifelse(vitb12_mcg<vitb12_ear,1,0),
           zn_inad = ifelse(zn_mg<zn_ear,1,0),
           
           #fortified
           vita_fort_inad = ifelse(vita_rae_mcg_fort<vita_ear,1,0),
           thia_fort_inad = ifelse(thia_mg_fort<thia_ear,1,0),
           ribo_fort_inad = ifelse(ribo_mg_fort<ribo_ear,1,0),
           niac_fort_inad = ifelse(niac_mg_fort<niac_ear,1,0),
           vitb6_fort_inad = ifelse(vitb6_mg_fort<vitb6_ear,1,0),
           folate_fort_inad = ifelse(folate_mcg_fort<folate_ear,1,0),
           vitb12_fort_inad = ifelse(vitb12_mcg_fort<vitb12_ear,1,0),
           zn_fort_inad = ifelse(zn_mg_fort<zn_ear,1,0),
           
           #nutrient adequacy ratio unfort
           vita_nar = ifelse(vita_rae_mcg<vita_ear,vita_rae_mcg/vita_ear,1),
           thia_nar = ifelse(thia_mg<thia_ear,thia_mg/thia_ear,1),
           ribo_nar = ifelse(ribo_mg<ribo_ear,ribo_mg/ribo_ear,1),
           niac_nar = ifelse(niac_mg<niac_ear,niac_mg/niac_ear,1),
           vitb6_nar = ifelse(vitb6_mg<vitb6_ear,vitb6_mg/vitb6_ear,1),
           folate_nar = ifelse(folate_mcg<folate_ear,folate_mcg/folate_ear,1),
           vitb12_nar = ifelse(vitb12_mcg<vitb12_ear,vitb12_mcg/vitb12_ear,1),
           zn_nar = ifelse(zn_mg<zn_ear,zn_mg/zn_ear,1),
           fe_nar = ifelse(fe_mg<fe_ear,fe_mg/fe_ear,1),
           
           #nutrient adequacy ratio fort
           vita_fort_nar = ifelse(vita_rae_mcg_fort<vita_ear,vita_rae_mcg_fort/vita_ear,1),
           thia_fort_nar = ifelse(thia_mg_fort<thia_ear,thia_mg_fort/thia_ear,1),
           ribo_fort_nar = ifelse(ribo_mg_fort<ribo_ear,ribo_mg_fort/ribo_ear,1),
           niac_fort_nar = ifelse(niac_mg_fort<niac_ear,niac_mg_fort/niac_ear,1),
           vitb6_fort_nar = ifelse(vitb6_mg_fort<vitb6_ear,vitb6_mg_fort/vitb6_ear,1),
           folate_fort_nar = ifelse(folate_mcg_fort<folate_ear,folate_mcg_fort/folate_ear,1),
           vitb12_fort_nar = ifelse(vitb12_mcg_fort<vitb12_ear,vitb12_mcg_fort/vitb12_ear,1),
           zn_fort_nar = ifelse(zn_mg_fort<zn_ear,zn_mg_fort/zn_ear,1),
           fe_fort_nar = ifelse(fe_mg_fort<fe_ear,fe_mg_fort/fe_ear,1),
           
           # mean adequacy ratio non-fort mandatory
           mar_mand= (folate_nar+vitb12_nar+fe_nar)/3,
           
           # mean adequacy ratio fort
           mar_mand_fort = (folate_fort_nar+vitb12_fort_nar+fe_fort_nar)/3
           
    ) %>% 
    left_join(ind_hh_info, by = 'hhid') %>% 
    # left_join(rice_rec_pds_hh, by = "hhid") %>%
    as_survey_design(ids = hhid, weights = survey_wgt, strata = adm2) 
  
  return(survey_inad)
}

# PDS MAPS 

vita_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vita_inad", adm1)+tm_layout(main.title = "Vitamin A - Base case")
vita_pds_rice_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vita_fort_inad",adm1)+tm_layout(main.title = "Vitamin A - Rice fortification")
vita_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "vita_fort_inad",adm1)+tm_layout(main.title = "Vitamin A - Wheat flour fortification")
vita_pds_both_adm1 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "vita_fort_inad",adm1)+tm_layout(main.title = "Vitamin A - All fortification")
vita_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vita_inad", adm2)+tm_layout(main.title = "Vitamin A - Base case")
vita_pds_rice_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vita_fort_inad",adm2)+tm_layout(main.title = "Vitamin A - Rice fortification")
vita_pds_wf_adm2 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "vita_fort_inad",adm2)+tm_layout(main.title = "Vitamin A - Wheat flour fortification")
vita_pds_both_adm2 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "vita_fort_inad",adm2)+tm_layout(main.title = "Vitamin A - All fortification")

thia_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "thia_inad",adm1)+tm_layout(main.title = "Thiamin - Base case")
thia_pds_rice_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "thia_fort_inad",adm1)+tm_layout(main.title = "Thiamin - Rice fortification ")
thia_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "thia_fort_inad",adm1)+tm_layout(main.title = "Thiamin - Wheat flour fortification")
thia_pds_both_adm1 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "thia_fort_inad",adm1)+tm_layout(main.title = "Thiamin - All fortification")
thia_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "thia_inad",adm2)+tm_layout(main.title = "Thiamin - Base case")
thia_pds_rice_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "thia_fort_inad",adm2)+tm_layout(main.title = "Thiamin - Rice fortification ")
thia_pds_wf_adm2 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "thia_fort_inad",adm2)+tm_layout(main.title = "Thiamin - Wheat flour fortification")
thia_pds_both_adm2 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "thia_fort_inad",adm2)+tm_layout(main.title = "Thiamin - All fortification ")

ribo_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "ribo_inad",adm1)+tm_layout(main.title = "Riboflavin - Base")
# vita_pds_rice <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vita_fort_inad")+tm_layout(main.title = "Riboflavin - Rice fortification case")
ribo_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "ribo_fort_inad",adm1)+tm_layout(main.title = "Riboflavin - Wheat flour fortification ")
# vita_pds_both <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "vita_fort_inad")+tm_layout(main.title = "Riboflavin - Rice fortification case")
ribo_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "ribo_inad",adm2)+tm_layout(main.title = "Riboflavin - Base case")
# vita_pds_rice <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vita_fort_inad")
ribo_pds_wf_adm2<- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "ribo_fort_inad",adm2)+tm_layout(main.title = "Riboflavin - Wheat flour fortification")

niac_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "niac_inad",adm1)+tm_layout(main.title = "Niacin - Base case")
niac_pds_rice_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "niac_fort_inad",adm1)+tm_layout(main.title = "Niacin - Rice fortification")
niac_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "niac_fort_inad",adm1)+tm_layout(main.title = "Niacin - Wheat flour fortification")
niac_pds_both_adm1 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "niac_fort_inad",adm1)+tm_layout(main.title = "Niacin - All fortification")
niac_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "niac_inad",adm2)+tm_layout(main.title = "Niacin - Base case")
niac_pds_rice_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "niac_fort_inad",adm2)+tm_layout(main.title = "Niacin - Rice fortification")
niac_pds_wf_adm2 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "niac_fort_inad",adm2)+tm_layout(main.title = "Niacin - Wheat flour fortification")
niac_pds_both_adm2 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "niac_fort_inad",adm2)+tm_layout(main.title = "Niacin - All fortification")

vitb6_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitb6_inad",adm1)+tm_layout(main.title = "Vitamin B6 - Base case")
vitb6_pds_rice_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitab6_fort_inad",adm1)+tm_layout(main.title = "Vitamin B6 - Rice fortification")
vitb6_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "vitb6_fort_inad",adm1)+tm_layout(main.title = "Vitamin B6 - Wheat flour fortification")
vitb6_pds_both_adm1 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "vitb6_fort_inad",adm1)+tm_layout(main.title = "Vitamin B6 - All fortification")
vitb6_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitb6_inad",adm2)+tm_layout(main.title = "Vitamin B6 - Base case")
vitb6_pds_rice_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitab6_fort_inad",adm2)+tm_layout(main.title = "Vitamin B6 - Rice fortification")
vitb6_pds_wf_adm2 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "vitb6_fort_inad",adm2)+tm_layout(main.title = "Vitamin B6 - Wheat flour fortification")
vitb6_pds_both_adm2 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "vitb6_fort_inad",adm2)+tm_layout(main.title = "Vitamin B6 - All fortification")

folate_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "folate_inad",adm1)+tm_layout(main.title = "Folate - Base base")
folate_pds_rice_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "folate_fort_inad",adm1)+tm_layout(main.title = "Zinc - Rice fortification")
folate_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "folate_fort_inad",adm1)+tm_layout(main.title = "Zinc - Wheat flour fortification")
folate_pds_both_adm1 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "folate_fort_inad",adm1)+tm_layout(main.title = "Zinc - All fortificaiton")
folate_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "folate_inad",adm2)+tm_layout(main.title = "Fplate - Base base")
folate_pds_rice_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "folate_fort_inad",adm2)+tm_layout(main.title = "Zinc - Rice fortification")
folate_pds_wf_adm2<- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "folate_fort_inad",adm2)+tm_layout(main.title = "Zinc - Wheat flour fortification")
folate_pds_both_adm2 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "folate_fort_inad",adm2)+tm_layout(main.title = "Zinc - All fortification")

vitb12_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitb12_inad",adm1)+tm_layout(main.title = "Vitamin B12 - Base base")
vitb12_pds_rice_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitb12_fort_inad",adm1)+tm_layout(main.title = "Vitamin B12 - Rice fortification")
vitb12_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "vitb12_fort_inad",adm1)+tm_layout(main.title = "Vitamin B12 - Wheat flour fortification")
vitb12_pds_both_adm1 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "vitb12_fort_inad",adm1)+tm_layout(main.title = "Vitamin B12 - All fortification")
vitb12_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitb12_inad",adm2)+tm_layout(main.title = "Vitamin B12 - Base case")
vitb12_pds_rice_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "vitb12_fort_inad",adm2)+tm_layout(main.title = "Vitamin B12 - Rice fortification")
vitb12_pds_wf_adm2 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "vitb12_fort_inad",adm2)+tm_layout(main.title = "Vitamin B12 - Wheat flour fortification")
vitb12_pds_both_adm2 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "vitb12_fort_inad",adm2)+tm_layout(main.title = "Vitamin B12 - All fortification")

zinc_pds_base_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "zn_inad",adm1)+tm_layout(main.title = "Zinc - Base base")
zinc_pds_rice_adm1 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "zn_fort_inad",adm1)+tm_layout(main.title = "Zinc - Rice fortification")
zinc_pds_wf_adm1 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "zn_fort_inad",adm1)+tm_layout(main.title = "Zinc - Wheat flour fortification")
zinc_pds_both_adm1 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "zn_fort_inad",adm1)+tm_layout(main.title = "Zinc - All fortification")
zinc_pds_base_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "zn_inad",adm2)+tm_layout(main.title = "Zinc - Base base")
zinc_pds_rice_adm2 <- ind_prev_maps(create_hh_inad(1, ind_fort_spec), "zn_fort_inad",adm2)+tm_layout(main.title = "Zinc - Rice fortification")
zinc_pds_wf_adm2 <- ind_prev_maps(create_hh_inad(2, ind_fort_spec), "zn_fort_inad",adm2)+tm_layout(main.title = "Zinc - Wheat flour fortification")
zinc_pds_both_adm2 <- ind_prev_maps(create_hh_inad(3, ind_fort_spec), "zn_fort_inad",adm2)+tm_layout(main.title = "Zinc - All fortification")

# save the maps
path_to_save <- "../Presentations/inda_lsff/figures/pds_fort/adm1/"

tmap_save(vita_pds_base_adm1, paste0(path_to_save, "vita_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vita_pds_wf_adm1, paste0(path_to_save, "vita_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vita_pds_rice_adm1, paste0(path_to_save, "vita_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vita_pds_both_adm1, paste0(path_to_save, "vita_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(thia_pds_base_adm1, paste0(path_to_save, "thia_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_pds_wf_adm1, paste0(path_to_save, "thia_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_pds_rice_adm1, paste0(path_to_save, "thia_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_pds_both_adm1, paste0(path_to_save, "thia_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(ribo_pds_base_adm1, paste0(path_to_save, "ribo_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(ribo_pds_wf_adm1, paste0(path_to_save, "ribo_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(niac_pds_base_adm1, paste0(path_to_save, "niac_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_pds_wf_adm1, paste0(path_to_save, "niac_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_pds_rice_adm1, paste0(path_to_save, "niac_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_pds_both_adm1, paste0(path_to_save, "niac_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(vitb6_pds_base_adm1, paste0(path_to_save, "vitb6_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb6_pds_wf_adm1, paste0(path_to_save, "vitb6_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb6_pds_rice_adm1, paste0(path_to_save, "vitb6_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb6_pds_both_adm1, paste0(path_to_save, "vitb6_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(vitb12_pds_base_adm1, paste0(path_to_save, "vitb12_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb12_pds_wf_adm1, paste0(path_to_save, "vitb12_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb12_pds_rice_adm1, paste0(path_to_save, "vitb12_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb12_pds_both_adm1, paste0(path_to_save, "vitb12_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(folate_pds_base_adm1, paste0(path_to_save, "folate_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(folate_pds_wf_adm1, paste0(path_to_save, "folate_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(folate_pds_rice_adm1, paste0(path_to_save, "folate_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(folate_pds_both_adm1, paste0(path_to_save, "folate_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(zinc_pds_base_adm1, paste0(path_to_save, "zinc_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zinc_pds_wf_adm1, paste0(path_to_save, "zinc_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zinc_pds_rice_adm1, paste0(path_to_save, "zinc_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zinc_pds_both_adm1, paste0(path_to_save, "zinc_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

path_to_save <- "../Presentations/inda_lsff/figures/pds_fort/adm2/"
tmap_save(vita_pds_base_adm2, paste0(path_to_save, "vita_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vita_pds_wf_adm2, paste0(path_to_save, "vita_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vita_pds_rice_adm2, paste0(path_to_save, "vita_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vita_pds_both_adm2, paste0(path_to_save, "vita_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(thia_pds_base_adm2, paste0(path_to_save, "thia_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_pds_wf_adm2, paste0(path_to_save, "thia_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_pds_rice_adm2, paste0(path_to_save, "thia_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(thia_pds_both_adm2, paste0(path_to_save, "thia_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(ribo_pds_base_adm2, paste0(path_to_save, "ribo_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(ribo_pds_wf_adm2, paste0(path_to_save, "ribo_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(niac_pds_base_adm2, paste0(path_to_save, "niac_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_pds_wf_adm2, paste0(path_to_save, "niac_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_pds_rice_adm2, paste0(path_to_save, "niac_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(niac_pds_both_adm2, paste0(path_to_save, "niac_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(vitb6_pds_base_adm2, paste0(path_to_save, "vitb6_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb6_pds_wf_adm2, paste0(path_to_save, "vitb6_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb6_pds_rice_adm2, paste0(path_to_save, "vitb6_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb6_pds_both_adm2, paste0(path_to_save, "vitb6_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(vitb12_pds_base_adm2, paste0(path_to_save, "vitb12_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb12_pds_wf_adm2, paste0(path_to_save, "vitb12_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb12_pds_rice_adm2, paste0(path_to_save, "vitb12_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(vitb12_pds_both_adm2, paste0(path_to_save, "vitb12_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(folate_pds_base_adm2, paste0(path_to_save, "folate_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(folate_pds_wf_adm2, paste0(path_to_save, "folate_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(folate_pds_rice_adm2, paste0(path_to_save, "folate_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(folate_pds_both_adm2, paste0(path_to_save, "folate_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)

tmap_save(zinc_pds_base_adm2, paste0(path_to_save, "zinc_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zinc_pds_wf_adm2, paste0(path_to_save, "zinc_wf.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zinc_pds_rice_adm2, paste0(path_to_save, "zinc_rice.png"),
          width = 6, height = 6, units = "in", dpi = 600)
tmap_save(zinc_pds_both_adm2, paste0(path_to_save, "zinc_both.png"),
          width = 6, height = 6, units = "in", dpi = 600)



# iron intake 
iron_intake <- function(fortification_item = 3, fort_standards){
  ind_pds_fort_fct <- create_pds_fct(fortification_item, fort_standards)
  
  iron_intake <- ind_all_items %>% 
    select(hhid,afe, item_code,item_name, quantity_g, quantity_100g) %>% 
    inner_join(ind_pds_fort_fct %>% select(item_code,item_name,
                                           energy_kcal,fe_mg,fe_mg_fort), by =c( "item_code","item_name" )) %>% 
    mutate(
      across(
        -c(hhid,afe,item_name,item_code,quantity_g,quantity_100g),
        ~.x*quantity_100g
      )
    ) %>% 
    group_by(hhid) %>% 
    summarise(
      across(
        -c(afe,item_name,item_code,quantity_g,quantity_100g),
        ~sum(.,na.rm = TRUE)
      )
    ) %>% 
    ungroup() %>% 
    filter(energy_kcal <= quantile(energy_kcal,0.99)) 
  return(iron_intake)
}


##
fe_adm1 = fe_full_prob(iron_intake(1, ind_fort_spec) %>% 
                         rename(ai_afe = fe_mg_fort) %>% 
                         left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm1,bio_avail = 5) %>% 
  rename(fe_inad_rice = prev_inad) %>% 
  left_join(
    fe_full_prob(iron_intake(2, ind_fort_spec) %>% 
                   rename(ai_afe = fe_mg_fort) %>% 
                   left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm1,bio_avail = 5) %>% 
      rename(fe_inad_wf = prev_inad),
    by = 'subpopulation'
  ) %>% 
  left_join(
    fe_full_prob(iron_intake(3, ind_fort_spec) %>% 
                   rename(ai_afe = fe_mg_fort) %>% 
                   left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm1,bio_avail = 5) %>% 
      rename(fe_inad_both = prev_inad),
    by = 'subpopulation'
  ) %>% 
  left_join(
    fe_full_prob(iron_intake(1, ind_fort_spec) %>% 
                   rename(ai_afe = fe_mg) %>% 
                   left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm1,bio_avail = 5) %>% 
      rename(fe_inad_base = prev_inad),
    by = 'subpopulation'
  ) %>% 
  rename(adm1 = subpopulation)



fe_adm2 = fe_full_prob(iron_intake(1, ind_fort_spec) %>% 
                         rename(ai_afe = fe_mg_fort) %>% 
                         left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm2,bio_avail = 5) %>% 
  rename(fe_inad_rice = prev_inad) %>% 
  left_join(
    fe_full_prob(iron_intake(2, ind_fort_spec) %>% 
                   rename(ai_afe = fe_mg_fort) %>% 
                   left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm2,bio_avail = 5) %>% 
      rename(fe_inad_wf = prev_inad),
    by = 'subpopulation'
  ) %>% 
  left_join(
    fe_full_prob(iron_intake(3, ind_fort_spec) %>% 
                   rename(ai_afe = fe_mg_fort) %>% 
                   left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm2,bio_avail = 5) %>% 
      rename(fe_inad_both = prev_inad),
    by = 'subpopulation'
  ) %>% 
  left_join(
    fe_full_prob(iron_intake(1, ind_fort_spec) %>% 
                   rename(ai_afe = fe_mg) %>% 
                   left_join(ind_hh_info %>% select(hhid, adm1,adm2),by = "hhid"), adm2,bio_avail = 5) %>% 
      rename(fe_inad_base = prev_inad),
    by = 'subpopulation'
  ) %>% 
  rename(adm1 = subpopulation)



iron_inad_maps <- function( adm, micronutrient){
  #creates inadequacy maps at either adm1 or adm2
  
  
  # print(inad_prev_adm)
  if(adm == 'adm1'){
    shapefile = fe_adm1 %>%
      mutate(adm1 = as.numeric(adm1)) %>% 
      left_join(india_adm1, by = c('adm1'='State_code'))
  }
  if(adm == 'adm2'){
    shapefile = fe_adm2 %>%
      mutate(adm1 = as.numeric(adm1)) %>% 
      left_join(india_adm2, by = c('adm1'='adm2_code'))
  }
  
  shapefile %>%
    st_as_sf() %>%
    tm_shape() +
    tm_fill(col = {{micronutrient}}, style = "cont",
            breaks = seq(0,100,by=10),
            palette = (wesanderson::wes_palette("Zissou1Continuous")),
            title = "" ,
            legend.is.portrait = FALSE
    ) +
    tm_layout(main.title = , frame = F,
              main.title.size = 0.8,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35
    ) +
    tm_borders(col = "black", lwd = 0) +
    tm_shape(india_adm1) +
    # tm_fill(col = "state") +
    tm_borders(col = "black", lwd = 1.5)+
    tm_legend(show = F)
  
  
}

fe_pds_both_adm2 <- iron_inad_maps("adm2", "fe_inad_both")
fe_pds_rice_adm2<- iron_inad_maps("adm2", "fe_inad_rice")
fe_pds_wf_adm2<- iron_inad_maps("adm2", "fe_inad_wf")
fe_pds_base_adm2<- iron_inad_maps("adm2", "fe_inad_base")
fe_pds_both_adm1 <- iron_inad_maps("adm1", "fe_inad_both")
fe_pds_rice_adm1<- iron_inad_maps("adm1", "fe_inad_rice")
fe_pds_wf_adm1<- iron_inad_maps("adm1", "fe_inad_wf")
fe_pds_base_adm1<- iron_inad_maps("adm1", "fe_inad_base")




# by sep quintile

inad_prev_resqui <- ind_pds_prev_inad %>% 
  srvyr::group_by(res,res_quintile) %>% 
  srvyr::summarise(
    srvyr::across(
      ends_with("_inad"),
      ~round(srvyr::survey_mean(
        .x == 1, proportion = TRUE
      )*100,2)
    )
  ) %>% 
  mutate(
    res_quintile = paste(res, res_quintile)
  )%>% 
  bind_rows(
    ind_pds_prev_inad %>% 
      srvyr::group_by(res) %>% 
      srvyr::summarise(
        srvyr::across(
          ends_with("_inad"),
          ~round(srvyr::survey_mean(
            .x == 1, proportion = TRUE
          )*100,2)
        )
      ) %>% 
      mutate(
        res_quintile = paste(res, "Total")
      )
  ) %>% 
  select(
    res_quintile,vita_inad,vita_fort_inad, thia_inad,thia_fort_inad,
    ribo_inad, ribo_fort_inad, niac_inad,niac_fort_inad,vitb6_inad,
    vitb6_fort_inad, vitb6_inad, folate_inad,folate_fort_inad, 
    vitb12_inad, vitb12_fort_inad,zn_inad, zn_fort_inad
  )




# by STATE 


#### TO FIX ####

create_hh_inad(1, ind_fort_spec) %>% 
  srvyr::group_by(adm1) %>% 
  srvyr::summarise(
    srvyr::across(
      ends_with("_inad"),
      ~round(srvyr::survey_mean(
        .x == 1, proportion = TRUE
      )*100,2)
    )
  ) %>% 
  rename_with(
    .fn = function(x) paste0(x, "_rice"),
    .cols = contains("fort")
  ) %>% 
  select(
    -contains("_se")
  )%>% 
  left_join(
    create_hh_inad(2) %>% 
      srvyr::group_by(adm1) %>% 
      srvyr::summarise(
        srvyr::across(
          ends_with("_inad"),
          ~round(srvyr::survey_mean(
            .x == 1, proportion = TRUE
          )*100,2)
        )
      ) %>% 
      rename_with(
        .fn = function(x) paste0(x, "_wf"),
        .cols = contains("fort")
      ) %>% 
      select(
        adm1,
        contains("fort")
      ) %>% 
      select(
        -contains("_se_")
      )
    ,
    by = "adm1"
  ) %>% 
  left_join(
    create_hh_inad(3) %>% 
      srvyr::group_by(adm1) %>% 
      srvyr::summarise(
        srvyr::across(
          ends_with("_inad"),
          ~round(srvyr::survey_mean(
            .x == 1, proportion = TRUE
          )*100,2)
        )
      ) %>% 
      rename_with(
        .fn = function(x) paste0(x, "_both"),
        .cols = contains("fort")
      ) %>% 
      select(
        adm1,
        contains("fort")
      ) %>% 
      select(
        -contains("_se_")
      )
    ,
    by = "adm1"
  )



# difference in mean adequacy ratio
create_mar_diff <- function(...){
  
  
  mar_diff <- create_hh_inad(1, ind_fort_spec) %>% 
    srvyr::group_by(...) %>% 
    srvyr::summarise(
      srvyr::across(
        starts_with("niac"),
        ~round(srvyr::survey_mean(
          .x == 1, proportion =TRUE
        )*100,2)
      )
    ) %>%
    rename_with(
      .fn = function(x) paste0(x, "_rice"),
      .cols = contains("fort")
    ) %>%
    select(
      -contains("_se")
    )  %>% 
    left_join(
      create_hh_inad(2, ind_fort_spec) %>% 
        srvyr::group_by(...) %>% 
        srvyr::summarise(
          srvyr::across(
            starts_with("niac"),
            ~round(srvyr::survey_mean(
              .x == 1, proportion =TRUE
            )*100,2)
          )
        ) %>%
        rename_with(
          .fn = function(x) paste0(x, "_wf"),
          .cols = contains("fort")
        ) %>%
        select(
          -contains("_se"), -mar
        ) 
      ,
      by = c("res",'res_quintile')
    )%>% 
    left_join(
      create_hh_inad(3, ind_fort_spec) %>% 
        srvyr::group_by(...) %>% 
        srvyr::summarise(
          srvyr::across(
            starts_with("niac"),
            ~round(srvyr::survey_mean(
              .x == 1, proportion =TRUE
            )*100,2)
          )
        ) %>%
        rename_with(
          .fn = function(x) paste0(x, "_both"),
          .cols = contains("fort")
        ) %>%
        select(
          -contains("_se"), -mar
        ), by= c("res", "res_quintile")
      
    )
  return(mar_diff)
}

# rice_rec_pds_hh <- ind_recieved_pds %>% 
#   filter(item_code == 101) %>% 
#   group_by(hhid,adm2, survey_wgt, adm1,afe) %>% 
#   summarise(
#     recieved_pds = ifelse(sum(recieved_pds)>0, 1,0),
#     quantity_g = sum(quantity_g)
#   ) %>%
#   ungroup() %>% 
#   select(hhid, recieved_pds)



niac_rice_fort = create_hh_inad(1, ind_fort_spec) %>% 
  srvyr::group_by(res,res_quintile) %>% 
  filter(recieved_pds == 1) %>%
  select(hhid, starts_with("niac")) %>% 
  srvyr::summarise(
    srvyr::across(
      ends_with("inad"),
      ~round(srvyr::survey_mean(
        .x == 1, proportion =TRUE
      )*100,2)
    )
  ) %>%
  rename_with(
    .fn = function(x) paste0(x, "_rice"),
    .cols = contains("fort")
  ) %>%
  select(
    -contains("_se")
  ) %>% 
  mutate(res_quintile = as.character(res_quintile) )%>% 
  bind_rows(
    create_hh_inad(1, ind_fort_spec) %>% 
      srvyr::group_by(res) %>% 
      filter(recieved_pds == 1) %>% 
      select(starts_with("niac")) %>% 
      
      srvyr::summarise(
        srvyr::across(
          ends_with("inad"),
          ~round(srvyr::survey_mean(
            .x == 1, proportion =TRUE
          )*100,2)
        )
      ) %>%
      rename_with(
        .fn = function(x) paste0(x, "_rice"),
        .cols = contains("fort")
      ) %>%
      select(
        -contains("_se")
      ) %>% 
      mutate(res_quintile = "Total")
  )





# mar_diff_quintile <- create_mar_diff(res, res_quintile)
# mar_diff_nat <- create_mar_diff(res)

# create a dumbell plot with difference in MAR

# mar_db <- zinc_rice_fort_pds %>% 
#   pivot_longer(
#     cols = c(zn_inad , zn_fort_inad_rice)
#   )
# 
# mar_range <- mar_db %>% 
#   group_by(res,res_quintile,name) %>% 
#   summarise(
#     mar_low = min(value),
#     mar_high = max(value)
#   )


# niacin_db_plot <- mar_db %>% 
#   left_join(mar_range) %>% 
#   mutate(
#     Scenario = case_when(
#       name == "zn_inad" ~ "Base",
#       name == "zn_fort_inad_rice" ~ "Rice fortification"
#     ),
#     res_quintile = paste(res, res_quintile)
#   
#   ) %>% 
#   ggplot() +
#   geom_pointrange(aes(x = res_quintile, y = value, 
#                       ymin = mar_low, ymax = mar_high, 
#                       colour = Scenario,
#                       shape = Scenario
#   ))+ 
#   # geom_line(ymax = mar_high, ymin = mar_low)+
#   theme_bw() +
#   #scale_x_discrete(limits = rev(ethfort_dumbell$subpop_lab)) +
#   coord_flip(ylim = c(0, 100)) +
#   ylim(0,100)+
#   theme(legend.position = "bottom")+
#   ylab("Risk of inadequacy")+
#   xlab("Weath quintile")+
#   labs(
#     title = "PDS fortification - Zinc  (Total population)"
#   )
# 
# ggsave(paste0(path_to_save, "mar_db_plot_elig_s1.png") ,plot = mar_db_plot,
#        width = 8, height = 5, units = "in", dpi = 600)

# write_csv(inad_prev_resqui, paste0(path_to_save, "ind_res_quint_s1.csv"))


################################################################################
################################################################################
################################################################################
#        MAR  


# MAR MAPS #####################################

#create a summary for MAR
inad_mar_adm2 <- create_hh_inad(1,ind_fort_spec) %>% 
  group_by(adm2) %>% 
  srvyr::summarise(
    mar = survey_mean(mar_mand),
    mar_fort = survey_mean(mar_mand_fort)
  )

# 
# # create base MAR map
# inad_mar_adm2_base <- base_pds %>% 
#   group_by(adm2) %>% 
#   # filter(eligible ==1) %>% 
#   srvyr::summarise(
#     mar = survey_mean(mar)
#   )


pds_mar <- inad_mar_adm2 %>% 
  left_join(india_adm2, by = c('adm2' = 'adm2_code')) %>% 
  st_as_sf() %>% 
  tm_shape()+
  tm_fill(col = "mar", style = "cont",
          breaks = seq(0.6,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Mean Adeuacy Ratio" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "No fortification", frame = F,
            main.title.size = 0.8,
            main.title.position = 'center',
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(india_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)


path_to_save <- "../Presentations/inda_lsff/figures/pds_fort/"
tmap_save(pds_mar, paste0(path_to_save, "pds_no_fort_base.png"),
          width = 6, height = 6, units = "in", dpi = 600)




pds_mar_fort <- inad_mar_adm2 %>% 
  left_join(india_adm2, by = c('adm2' = 'adm2_code')) %>% 
  st_as_sf() %>% 
  tm_shape()+
  tm_fill(col = "mar_fort", style = "cont",
          breaks = seq(0.6,1,by=.10),
          palette = rev(wesanderson::wes_palette("Zissou1Continuous")),
          title = "Mean Adeuacy Ratio" ,
          legend.is.portrait = FALSE
  ) +
  tm_layout(main.title = "PDS Fortification of Rice - India current standards", frame = F,
            main.title.position = 'center',
            main.title.size = 0.8,
            legend.outside.position = "bottom",
            legend.outside.size = 0.35
  ) +
  tm_borders(col = "black", lwd = 0) +
  tm_shape(india_adm1) +
  # tm_fill(col = "state") +
  tm_borders(col = "black", lwd = 1.5)+
  tm_legend(show = F)
# tm_text("NAME_1", remove.overlap = TRUE, bg.color = "white", bg.alpha = 0.8)



path_to_save <- "../Presentations/inda_lsff/figures/pds_fort/"
tmap_save(pds_mar_fort, paste0(path_to_save, "pds_rice_ind.png"),
          width = 9, height = 9, units = "in", dpi = 600)



inad_mar_adm2 %>% 
  left_join(india_adm2, by = c('adm2' = 'adm2_code')) %>% 
  st_as_sf() %>% 
  tm_shape() + 
  tm_fill(col = "mar_fort",
          palette =  rev(wesanderson::wes_palette("Zissou1Continuous")), 
          breaks = seq(0.6,1,by=0.1),
          style = "cont",
          textNA = "Missing Data",
          title = "Mean Adequacy Ratio", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))

################################################################################
################################################################################
# upper limits


create_hh_inad(3, who_fort_spec) %>% 
  group_by(adm1) %>% 
  summarise(
    sum(vita_above),
    sum(niac_above),
    sum(vitb6_above),
    sum(folate_above),
    sum(fe_above),
    sum(zn_above),
    n()
  )

################################################################################
################################################################################
# Nutrient density



ind_pds_nut_dens <- ind_pds_fort_intake %>% 
  mutate(
    across(
      -c(hhid,eligible,energy_kcal),
      ~.x/(energy_kcal/1000)
      
    )
    
  ) %>% 
  left_join(ind_hh_info %>% 
              select(hhid,survey_wgt,adm2, res,sep_quintile,res_quintile), by = 'hhid') %>% 
  as_survey_design(ids = hhid, weights = survey_wgt, strata = adm2) 


x <- ind_pds_nut_dens %>% 
  group_by(eligible) %>% 
  summarise(
    across(
      -c(hhid,res,adm2,survey_wgt,res_quintile,energy_kcal),
      ~srvyr::survey_mean(.x, na.rm =TRUE  )
    )
  )

x %>% 
  ggplot(aes(x = eligible, y = vita_rae_mcg_fort))+
  geom_point()



# 
# nga_fort_rp_sep_sw_prev_2 <- nga_fort_rp_sep_sw %>% 
#   group_by(res) %>% 
#   srvyr::summarise(
#     across(
#       c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_min_yn,vita_rae_mcg_tot_fort_max_yn),
#       ~survey_mean(.x == 1, proportion = TRUE, na.rm = TRUE)
#     )) %>% 
#   mutate(res_quintile = paste(res, "Total"))
# 
# nga_fort_rp_sep_sw_prev <- rbind(nga_fort_rp_sep_sw_prev,nga_fort_rp_sep_sw_prev_2)
# 
# 
# nga_fort <- nga_fort_rp_sep_sw_prev %>% 
#   pivot_longer(cols =  c(vita_rae_mcg_yn,vita_rae_mcg_tot_fort_min_yn,vita_rae_mcg_tot_fort_max_yn)) %>% 
#   select(res_quintile,name,value) %>% 
#   mutate(subpop_lab = case_when(res_quintile=="Rural Total" ~ "Rural 6 (Total)",
#                                 res_quintile=="Rural 1" ~ "Rural 1 (Poorest)",
#                                 res_quintile=="Rural 2" ~ "Rural 2 (Poor)",
#                                 res_quintile=="Rural 3" ~ "Rural 3 (Neither poor nor wealthy)",
#                                 res_quintile=="Rural 4" ~ "Rural 4 (Wealthy)",
#                                 res_quintile=="Rural 5" ~ "Rural 5 (Wealthiest)",
#                                 res_quintile=="Urban Total" ~ "Urban 6 (Total)",
#                                 res_quintile=="Urban 1" ~ "Urban 1 (Poorest)",
#                                 res_quintile=="Urban 2" ~ "Urban 2 (Poor)",
#                                 res_quintile=="Urban 3" ~ "Urban 3 (Neither poor nor wealthy)",
#                                 res_quintile=="Urban 4" ~ "Urban 4 (Wealthy)",
#                                 res_quintile=="Urban 5" ~ "Urban 5 (Wealthiest)")) %>% 
#   ungroup() %>% 
#   mutate(scenario = case_when(name == "vita_rae_mcg_yn" ~ "No Fortification",
#                               name == "vita_rae_mcg_tot_fort_min_yn" ~ "Standard",
#                               name == "vita_rae_mcg_tot_fort_max_yn" ~ "Standard + 40%"
#   ))
# 

inind_ai %>% 
  left_join(ind_hh_info, by = "hhid") %>% 
  filter(adm1 %in% c(2,3,6,8,9,10,19,20,21,22,23,28)) %>% 
  filter(energy_kcal <= quantile(energy_kcal,0.99)) %>% 
  left_join(india_adm1, by = c("adm1" = "State_code")) %>% 
  ggplot(aes(x = energy_kcal)) +
  geom_histogram()+
  theme_bw()+
  xlab("Energy intake (kcal)")+
  facet_wrap("NAME_1")




x <- ind_ai %>% 
  left_join(ind_hh_info) %>% 
  filter(adm2 == 2218)
