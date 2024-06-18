#########################################
#          Reach maps ETH ALL COMSUMptION             #
#########################################


# Author: Gabriel Battcock
# Created: 13 June 24
# Last updated: 13 June 24

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

# Read in data ----------------------------------------------------------------


eth_hices_hh_info <- household_data("eth_hices1516")
eth_hices_all_vehicles <- full_item_list("eth_hices1516")
eth_adm2 <- st_read("~/../General - MIMI Project/Nutrition analysis/shapefiles/eth_hices1516_adm2.shp")


# only select the items with fortifiable vehicles (FROM MO'S WORk)
hices_all_fort_vehicles <-  eth_hices_all_vehicles  %>%
   mutate(item_code = gsub(" ", "", eth_hices_all_vehicles$item_code)) %>% 
   mutate(item_code = dplyr::case_when(
  item_code == "Wheatwhite,flour" ~ "Wheat flour",
  item_code == "Wheatmixed,flour" ~ "Wheat flour",
  item_code == "Wheatblack,flour" ~ "Wheat flour",
  item_code == "Wheat&Barley(Duragna),flour" ~ "Wheat flour",
  item_code == "Wheat&othercereals,flour" ~ "Wheat flour",
  item_code == "Flour,factoryproduct,mainlyofwheat" ~ "Wheat flour",
  item_code == "Maize,flour" ~ "Maize flour",
  item_code == "Rice" ~ "Rice",
  item_code == "Bread(Dufo,Anbashaetc),Wheat-homemade" ~ "Wheat flour",
  item_code == "Bread,wheat-bakery" ~ "Wheat flour",
  item_code == "Donat/bombolino" ~ "Wheat flour",
  item_code == "Boresh(Dolchi)" ~ "Wheat flour",
  item_code == "Pizzas" ~ "Wheat flour",
  item_code == "Cakes" ~ "Wheat flour",
  item_code == "Biscuits" ~ "Wheat flour",
  item_code == "Baqlaba/Mushebek" ~ "Wheat flour",
  item_code == "Edibleoil,local" ~ "Edible oil",
  item_code == "Edibleoil,imported" ~ "Edible oil",
  item_code == "Sugar" ~ "Sugar",
  item_code == "Salt" ~ "Salt",
  item_code == "Sandwitch,meat/egg/vegetable,normal" ~ "Wheat flour",
  item_code == "Burger/clubsandwich" ~ "Wheat flour",
  item_code == "Breadoranypastryproductswithhotdrinks" ~ "Wheat flour",
  item_code == "BreadoranypastryproductsandJuice" ~ "Wheat flour",
  item_code == "Othersn.e.c." ~ "Wheat flour",
  item_code == "Rice_duplicated_11202123" ~ "Rice"
)) %>% filter(
  item_code %in% c("Wheat flour", "Edible oil")
) %>% 
   group_by(hhid, item_code) %>% 
   summarise(quantity_g = sum(quantity_g)) %>% 
   mutate(consumed = ifelse(quantity_g>0, 1, 0)) %>% 
   ungroup() 

# create a shape file for each vehicle -----------------------------------------

eth_hices_reach_sp_edible_oil <- hices_all_fort_vehicles %>% 
  filter(item_code == "Edible oil") %>% 
  left_join(eth_hices_hh_info %>% 
              distinct(hhid, .keep_all = TRUE), by = "hhid") %>% 
  as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::group_by(adm2) %>% 
  srvyr::summarise(
    edibleoil = srvyr::survey_mean(consumed == 1, proportion = TRUE,na.rm = T)*100,
  ) %>% 
  full_join(eth_adm2 %>% distinct(dstrct_,.keep_all = T), by = c("adm2" = "dstrct_")) %>% 
  st_as_sf()


# wheat flour
eth_hices_reach_sp_wf <- hices_all_fort_vehicles %>% 
  filter(item_code == "Wheat flour") %>% 
  left_join(eth_hices_hh_info %>% 
              distinct(hhid, .keep_all = TRUE), by = "hhid") %>% 
  as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::group_by(adm2) %>% 
  srvyr::summarise(
    wheatflour = srvyr::survey_mean(consumed == 1, proportion = TRUE,na.rm = T)*100,
  ) %>% 
  full_join(eth_adm2 %>% distinct(dstrct_,.keep_all = T), by = c("adm2" = "dstrct_")) %>% 
  st_as_sf()


# MAPS -------------------------------------------------------------------------

eth_wf <- tm_shape(eth_hices_reach_sp_wf) +
  tm_fill(col = "wheatflour",
          title = "Reach (%)", 
          breaks = seq(0,100,by=10),
          # palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data",
          legend.is.portrait = F) + 
  tm_layout(main.title = "Wheat Flour", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0) + 
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)
file_path = "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/data_requests/JG_JUN_2024_ETH/"
tmap_save(eth_wf, paste0(file_path, "eth_wf_all_consumption.png"),
          width = 9, height = 9, units = "in", dpi = 600)


eth_eo <- tm_shape( eth_hices_reach_sp_edible_oil) +
  tm_fill(col = "edibleoil",
          title = "Reach (%)", 
          breaks = seq(0,100,by=10),
          # palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data",
          legend.is.portrait = F) + 
  tm_layout(main.title = "Edible oil", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.2) + 
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)

tmap_save(eth_eo, paste0(file_path, "eth_oil_all_consumption.png"),
          width = 9, height = 9, units = "in", dpi = 600)



legend_reach <- tm_shape(eth_hices_reach_sp_edible_oil) + 
  tm_fill(col = "edibleoil",
          # palette = viridis(10, direction = -1), 
          breaks = seq(0,100,by=10),
          style = "cont",
          textNA = "Missing Data",
          title = "All consumption coverage (%)", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))
tmap_save(legend_reach, paste0(file_path, "eth_legend_reach.png"),
          width = 9, height = 9, units = "in", dpi = 600)



# Tabular ---------------------------------------------------------------------

eth_reach_tab <- eth_hices_reach_sp_edible_oil %>% 
  st_drop_geometry() %>% 
  select(NAME_1,adm2, edibleoil,edibleoil_se, NAME_2) %>% 
  left_join(
    eth_hices_reach_sp_wf %>% 
      st_drop_geometry() %>% 
      select(adm2, wheatflour,wheatflour_se), 
    by = 'adm2'
  ) %>% 
  # mutate(adm2 = ifelse(NAME_2 == "Addis Abeba", adm2, NAME_2)) %>% 
  # select(-NAME_2) %>% 
  filter(!is.na(edibleoil))

writexl::write_xlsx(eth_reach_tab,paste(file_path, "eth_vehilces_tab.xlsx"))

########################## END OF SCRIPT #######################################