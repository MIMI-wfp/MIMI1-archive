#########################################
#          Reach maps ETH               #
#########################################


# Author: Gabriel Battcock
# Created: 29 May 24
# Last updated: 29 May 24

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

eth_vehicles <- read.csv("~/../General - MIMI Project/Nutrition analysis/fortification scenarios/data/current/eth_hices1516_vehicle_quantities.csv")
eth_hices_hh_info <- household_data("eth_hices1516")
eth_adm2 <- st_read("~/../General - MIMI Project/Nutrition analysis/shapefiles/eth_hices1516_adm2.shp")



eth_hices_reach_sp <- eth_vehicles %>% 
  left_join(eth_hices_hh_info %>% 
              distinct(hhid, .keep_all = TRUE), by = "hhid") %>% 
  as_survey_design(ids = hhid, weights = survey_wgt) %>% 
  srvyr::group_by(adm2) %>% 
  srvyr::summarise(
    wheatflour = srvyr::survey_mean(wheatflour == "Yes", proportion = TRUE,na.rm = T)*100,
    edible_oil = srvyr::survey_mean(edible_oil == "Yes", proportion = TRUE,na.rm = T)*100
  ) %>% 
  full_join(eth_adm2, by = c("adm2" = "dstrct_")) %>% 
  st_as_sf()





eth_wf <- tm_shape(eth_hices_reach_sp) +
  tm_fill(col = "wheatflour",
          title = "Reach (%)", 
          breaks = seq(0,100,by=10),
          # palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data",
          legend.is.portrait = F) + 
  tm_layout(main.title = "Wheat Flour", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.2) + 
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)
  # tm_text("adm2", size = 0.5, remove.overlap = TRUE, size.lim = c(0.5,0.6))
file_path = "C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/data_requests/AP_May_2024_ETH_REACH/"
tmap_save(eth_wf, paste0(file_path, "eth_reach_wf.png"),
          width = 9, height = 9, units = "in", dpi = 600)
          
eth_oil <- tm_shape(eth_hices_reach_sp) +
  tm_fill(col = "edible_oil",
          title = "Reach (%)", 
          breaks = seq(0,100,by=10),
          # palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data",
          legend.is.portrait = F) + 
  tm_layout(main.title = "Edible Oil", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.2) + 
  tm_legend(show = F) +
  tm_credits("Source: Ethiopian Household Consumption Expenditure Survey 2015-16",
             position = 'left',
             size = 0.5)
# tm_text("adm2", size = 0.5, remove.overlap = TRUE, size.lim = c(0.5,0.6))
tmap_save(eth_oil, paste0(file_path, "eth_reach_edible_oil.png"),
          width = 9, height = 9, units = "in", dpi = 600)

legend_reach <- tm_shape(eth_hices_reach_sp) + 
  tm_fill(col = "edible_oil",
          # palette = viridis(10, direction = -1), 
          breaks = seq(0,100,by=10),
          style = "cont",
          textNA = "Missing Data",
          title = "Fortification vehicle coverage (%)", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1,
            title.position =c(0.5, 0.5))
tmap_save(legend_reach, paste0(file_path, "eth_legend_reach.png"),
          width = 9, height = 9, units = "in", dpi = 600)


