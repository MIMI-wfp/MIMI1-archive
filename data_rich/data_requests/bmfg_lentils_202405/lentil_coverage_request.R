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

reach_consumption <- function(full_item, hh_data,  food_name,  ...){
  
  x <- full_item %>% 
    select(hhid,item_code, item_name,quantity_g, quantity_100g) %>% 
    filter(item_code %in% c(food_name )) %>% 
    mutate(consumed = ifelse(quantity_g>0, 1,0)) %>% 
    group_by(hhid) %>% 
    mutate(consumed = ifelse(sum(consumed)>0,1,0), 
           quantity_g = sum(quantity_g)) %>% 
    slice(1) %>% 
    left_join(hh_data %>% 
                select(hhid, adm1,res,survey_wgt) %>% 
                distinct(hhid, .keep_all = TRUE), by = "hhid") %>% 
    as_survey_design(ids = hhid, strata = res, weights = survey_wgt) %>% 
    srvyr::group_by(...) %>% 
    mutate(quantity_g = ifelse(quantity_g == 0, NA, quantity_g)) %>% 
    srvyr::summarise(
      reach = srvyr::survey_mean( consumed == 1, proportion = TRUE,na.rm = T)*100,
      per_capita_consumption = survey_mean(quantity_g, na.rm = T)
    )
  
  return(x)
  
}



#### ETHIOPIA #####

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


#### India ######


# bihar = 10, UP = 09, chat = 22, odisha = 21, mp = 23, jk = 20, wb = 19, , hp = 02 ap = 28

file_path <- here::here("data_rich/India/data/processed/lsff/")
ind_all_items <- full_item_list("ind_nss1112",file_path)
ind_hh_info <- household_data("ind_nss1112",file_path)

india_adm1 <- st_read(here::here("data_rich/India/data/processed/state_shape.shp"))


# map function 

ind_map_lentils <- function(lentil_item){
  
  ind_lentil_adm1 <- reach_consumption(ind_all_items,ind_hh_info,lentil_item, adm1)
  
  ind_lentil_adm1_sp <- ind_lentil_adm1 %>% 
    left_join(india_adm1, by = c("adm1" = "State_code")) %>% 
    st_as_sf() 
  
  
  x <- tm_shape(ind_lentil_adm1_sp) +
    tm_fill(col = 'reach', style = "cont",breaks = seq(0,100,by=10),
            palette = RColorBrewer::brewer.pal(7, "Blues"),
            title = "Reach (%)" ,
            
            legend.is.portrait = FALSE
    ) +
    tm_layout(main.title = , frame = F,
              main.title.size = 0.8,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35
    )+ 
    tm_text("NAME_1", size = 1/2, remove.overlap = TRUE) +
    tm_legend(show = F)+
    tm_credits("Source: India - Household Consumer Expenditure, NSS 68th Round, Sch 1, Type 1",
               position = 'left',
               size = 0.5)
  
  return(x)
}


# tur: 140, gram, split: 141, gram, whole:142, moong:143,masur:144, urad:145,

ind_map_tur <- ind_map_lentils(140) +
  tm_layout(main.title = "Tur")

ind_map_split <- ind_map_lentils(141) +
  tm_layout(main.title = "Gram, split")

ind_map_whole <- ind_map_lentils(142) +
  tm_layout(main.title = "Gram, whole")

ind_map_moong <- ind_map_lentils(143) +
  tm_layout(main.title = "Moong")

ind_map_masur <- ind_map_lentils(144) +
  tm_layout(main.title = "Masur")

ind_map_urad <- ind_map_lentils(145) +
  tm_layout(main.title = "Urad")

all_lentils <- c(140,141,142,143,144,145)

ind_map_all_lentils <- ind_map_lentils(all_lentils) +
  tm_layout(main.title = "All lentils")


# create legend

ind_lentil_adm1_legend <- reach_consumption(ind_all_items,ind_hh_info,140, adm1)

ind_lentil_adm1_legend_sp <- ind_lentil_adm1_legend %>% 
  left_join(india_adm1, by = c("adm1" = "State_code")) %>% 
  st_as_sf() 

legend <- tm_shape(ind_lentil_adm1_legend_sp) + 
  tm_fill(col = "reach",style = "cont", breaks = seq(0,100,by=10),
          palette = RColorBrewer::brewer.pal(7, "Blues"),
          title = "Reach of lentils (%)" ,
          legend.is.portrait = FALSE
  ) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))



file_path = here::here("data_rich/data_requests/bmfg_lentils_202405/ind_lentils_20240520/")

tmap_save(ind_map_tur, paste0(file_path, "ind_tur.png"),
          width = 8, height = 8, units = "in", dpi = 600)
tmap_save(ind_map_split, paste0(file_path, "ind_split.png"),
          width = 8, height = 8, units = "in", dpi = 600)
tmap_save(ind_map_whole, paste0(file_path, "ind_whole.png"),
          width = 8, height = 8, units = "in", dpi = 600)
tmap_save(ind_map_moong, paste0(file_path, "ind_moong.png"),
          width = 8, height = 8, units = "in", dpi = 600)
tmap_save(ind_map_masur, paste0(file_path, "ind_masur.png"),
          width = 8, height = 8, units = "in", dpi = 600)
tmap_save(ind_map_urad, paste0(file_path, "ind_urad.png"),
          width = 8, height = 8, units = "in", dpi = 600)
tmap_save(ind_map_all_lentils, paste0(file_path, "ind_all.png"),
          width = 8, height = 8, units = "in", dpi = 600)
tmap_save(legend,paste0(file_path, "legend.png"),
          width = 4, height = 2, units = "in", dpi = 600)

# india tables

ind_table <- function(lentil_type, title){
  
country <- reach_consumption(ind_all_items,ind_hh_info,lentil_type, res) %>% 
  mutate(adm1 = "National")
  
adm1 <- reach_consumption(ind_all_items,ind_hh_info,lentil_type, res, adm1) %>% 
  left_join(india_adm1 %>% st_drop_geometry() %>% select(State_code, NAME_1),
            by = c('adm1' = "State_code")) %>% 
  select(-adm1) %>% 
  rename(adm1 = NAME_1) %>% 
  arrange(adm1)

#create a tabke
table <- bind_rows(country, adm1)

output <- table %>% 
  gt() %>% 
  fmt_number(
    columns = c('reach', 'reach_se','per_capita_consumption',"per_capita_consumption_se"),
    decimals = 1
  ) %>% 
  cols_move_to_start(adm1) %>% 
  cols_label(
    adm1 = "Area",
    res = "Residence",
    reach = "Reach (%)",
    reach_se = "Reach SE (%)",
    per_capita_consumption = "Mean consumption (g)",
    per_capita_consumption_se = "Mean consumption SE (g)",
  ) %>% 
  tab_header(
    title = paste("Reach and per-capita consumption of",title, "in India"),
    subtitle = md("Source: India - Household Consumer Expenditure, \n NSS 68th Round, Sch 1, Type 1")
  )
return(output)
}

# tur: 140, gram, split: 141, gram, whole:142, moong:143,masur:144, urad:145,
ind_tur_table <- ind_table(140, "Tur")
gtsave(ind_tur_table, paste0(file_path, "ind_tur.pdf"))

ind_split_table <-  ind_table(141, "Gram - split")
gtsave(ind_split_table, paste0(file_path, "ind_split.pdf"))

ind_whole_table <-  ind_table(142, "Gram - whole")
gtsave(ind_whole_table, paste0(file_path, "ind_whole.pdf"))

ind_moong_table <-  ind_table(143, "Moong")
gtsave(ind_moong_table, paste0(file_path, "ind_moong.pdf"))

ind_masur_table <- ind_table(144, "Masur")
gtsave(ind_masur_table, paste0(file_path, "ind_masur.pdf"))

ind_urad_table <- ind_table(145, "Urad")
gtsave(ind_urad_table, paste0(file_path, "ind_urad.pdf"))

ind_all_lentil_table <- ind_table(all_lentils, "all lentils")
gtsave(ind_all_lentil_table, paste0(file_path, "ind_all_lentil.pdf"))
