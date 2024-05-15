#########################################
#          Lentil request               #
#########################################


# Author: Gabriel Battcock
# Created: 14 May 24
# Last updated: 14 May 24

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

hices_full_item <- full_item_list("eth_hices1516")
hices_hh_info <- household_data("eth_hices1516")

eth_adm1 <- st_read("../MIMI_data/Ethiopia/gadm41_ETH_shp/gadm41_ETH_1.shp")


eth_adm1 <- eth_adm1 %>% 
  mutate(
    adm1 = case_when(
      NAME_1 == "Tigray" ~ "Tigray",
      NAME_1 == "Afar" ~ "Afar",
      NAME_1 == "Amhara" ~ "Amhara",
      NAME_1 == "Oromia"~ "Oromiya",
      NAME_1 == "Somali" ~ "Somali",
      NAME_1 == "Benshangul-Gumaz" ~ "Benshangul",
      NAME_1 == "Southern Nations, Nationalities" ~ "SNNP",
      NAME_1 == "Gambela Peoples" ~ "Gambella",
      NAME_1 == "Harari People" ~ "Harari", 
      NAME_1 == "Addis Abeba" ~ "Addis Ababa", 
      NAME_1 == "Dire Dawa" ~ "Dire Dawa"
    )
  ) %>% 
  select(adm1, geometry)




reach_consumption <- function(full_item, hh_data,  food_name){

  x <- full_item %>% 
    select(hhid,item_code, item_name,quantity_g, quantity_100g) %>% 
    filter(item_code %in% c(food_name )) %>% 
    mutate(consumed = ifelse(quantity_g>0, 1,0)) %>% 
    group_by(hhid) %>% 
    mutate(consumed = ifelse(sum(consumed)>0,1,0)) %>% 
    slice(1) %>% 
    left_join(hh_data %>% 
                select(hhid, adm1,res,survey_wgt) %>% 
                distinct(hhid, .keep_all = TRUE), by = "hhid") %>% 
    as_survey_design(ids = hhid, strata = res, weights = survey_wgt) %>% 
    srvyr::group_by(adm1) %>% 
    mutate(quantity_g = ifelse(quantity_g == 0, NA, quantity_g)) %>% 
    srvyr::summarise(
      reach = srvyr::survey_mean( consumed == 1, proportion = TRUE,na.rm = T)*100,
      per_capita_consumption = survey_mean(quantity_g, na.rm = T)
    )
  
  return(x)
  
}


### Hicews

hices_full_item <- full_item_list("eth_hices1516")
hices_hh_info <- household_data("eth_hices1516")

split_lentil = "Lentils, split (kik)"
hices_lentils <- reach_consumption(hices_full_item,hices_hh_info, split_lentil)  

whole_grain_lentil = "Lentils, whole grain"
reach_consumption(hices_full_item,hices_hh_info, whole_grain_lentil) 

all_lentils <- c("Lentils, whole grain","Lentils, split (kik)")
hices_lentils <- reach_consumption(hices_full_item,hices_hh_info, all_lentils) 

## ESS

ess_full_item <- full_item_list("eth_ess1819")
ess_hh_info <- household_data("eth_ess1819")

lentils <-  204
ess_lentil <- reach_consumption(ess_full_item, ess_hh_info, lentils) %>% 
  mutate(
    adm1 = case_when(
      adm1 == 1 ~ "Tigray",
      adm1 == 2 ~ "Afar",
      adm1 == 3 ~ "Amhara",
      adm1 == 4 ~ "Oromiya",
      adm1 == 5 ~ "Somali",
      adm1 == 6 ~ "Benshangul",
      adm1 == 7 ~ "SNNP",
      adm1 == 12 ~ "Gambella",
      adm1 == 13 ~ "Harari", 
      adm1 == 14 ~ "Addis Ababa", 
      adm1 == 15 ~ "Dire Dawa"
    )
  )


nga_full_item <- full_item_list("nga_lss1819")
nga_hh_info = household_data("nga_lss1819")

reach_consumption(nga_full_item, nga_hh_info, lentils) 


# HICES lentils maps 

hices_lentils_sp <- hices_lentils %>% 
  left_join(eth_adm1,by ="adm1") %>% 
  st_as_sf()

hices_lentils_map <- tm_shape(hices_lentils_sp) +
  tm_fill(col = "reach", breaks = c(0,5,10,15,20,30,40, 50,60),
          palette = RColorBrewer::brewer.pal(7, "Blues"),
          title = "Reach (%)" ,
          legend.is.portrait = TRUE,
          
  ) +
  tm_layout(main.title = "Reach of lentils", frame = F,
            main.title.size = 1.2,
            legend.outside.position = "bottom",
            legend.outside.size = 0.5
  ) +
  tm_borders(col = "black", lwd = 0.2)  +
  tm_credits("Source: THE 2015/16 ETHIOPIAN HOUSEHOLD CONSUMPTION – \n EXPENDITURE (HCE) SURVEY",
             position = 'left',
             size = 0.5)

tmap_save(hices_lentils_map, here::here("data_rich/data_requests/bmfg_lentils_202405/hices_lentils.png"), width = 1110, height = 710)

# create a table with the data

hices_lentil_gt <- hices_lentils %>% 
  gt() %>% 
  fmt_number(
    columns = c('reach', 'reach_se','per_capita_consumption',"per_capita_consumption_se"),
    decimals = 1
  ) %>% 
  cols_label(
    adm1 = "Zone",
    reach = "Reach (%)",
    reach_se = "Reach SE (%)",
    per_capita_consumption = "Mean consumption (g)",
    per_capita_consumption_se = "Mean consumptionm SE (g)",
  ) %>% 
  tab_header(
    title = md("Reach and per-capita consumption of lentils in Ethiopia"),
    subtitle = md("Source: THE 2015/16 ETHIOPIAN HOUSEHOLD CONSUMPTION – \n EXPENDITURE (HCE) SURVEY")
  )
