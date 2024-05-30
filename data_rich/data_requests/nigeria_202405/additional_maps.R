################################################################################
######################### NIGERIA MAPS - NFA MEETING ###########################
################################################################################

# Author: Mo Osman
# Date created: 24-May-2024
# Last edited: 

# This script is to create additional maps that may be required when presenting
# at the NFA meeting: 
# - Coverage of fortifiable food vehicles at zone and ADM1 level.
# - Inadequate intake of micronutrients at zone, ADM1 and ADM2 level.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("tidyverse", "sf", "tmap", "readr", "rmapshaper", "raster",
                 "ggplot2", "ggspatial", "cowplot", "tmaptools", "terra", 
                 "gridExtra", "srvyr", "viridis")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA AND SHAPEFILES: 

# Read in shapefiles for mapping: 
nigeria_0 <- st_read("map_data/nga/new_shapefiles/nigeria_0")
nigeria_zone <- st_read("map_data/nga/new_shapefiles/nigeria_zone")
nigeria_1 <- st_read("map_data/nga/new_shapefiles/nigeria_1")
nigeria_2 <- st_read("map_data/nga/new_shapefiles/nigeria_2")

# Check polygons: 
plot(nigeria_0$geometry)
plot(nigeria_zone$geometry)
plot(nigeria_1$geometry)
plot(nigeria_2$geometry)

# Read in household info: 
nga_hh_info <- read_csv("survey_data/nga/secta_cover.csv") %>% 
  dplyr::select(hhid, zone, state, lga)

# Rename entries in "zone": 
nga_hh_info$zone <- case_when(
  nga_hh_info$zone == 1 ~ "North Central",
  nga_hh_info$zone == 2 ~ "North East",
  nga_hh_info$zone == 3 ~ "North West",
  nga_hh_info$zone == 4 ~ "South East",
  nga_hh_info$zone == 5 ~ "South South",
  nga_hh_info$zone == 6 ~ "South West"
)

# Read in Nigeria analysis data: 

nga_analysis_df <- read_csv("training/nigeria/data/nga_lss1819_analysis_df.csv")

# Left join zone to analysis data:
nga_analysis_df <- nga_analysis_df %>% 
  dplyr::left_join(nga_hh_info %>% dplyr::select(hhid, zone), by = "hhid")

rm(nga_hh_info)

#-------------------------------------------------------------------------------

# VEHICLE REACH: 

# Calculate survey weighted reach of vehicles at zone and state level: 

# Creat svy_tbl object: 
svy_nga_analysis <- nga_analysis_df %>% 
  as_survey_design(weights = survey_wgt)

# Zone: 
zone_reach <- svy_nga_analysis %>% 
  dplyr::group_by(zone) %>% 
  dplyr::summarise(reach_rice = round(survey_mean(rice, na.rm = T, vartype = NULL), 
                                      digits = 6),
                   reach_wheatflour = round(survey_mean(wheatflour, na.rm = T, vartype = NULL),
                                            digits = 6),
                   reach_maizeflour = round(survey_mean(maizeflour, na.rm = T, vartype = NULL), 
                                            digits = 6),
                   reach_edible_oil = round(survey_mean(edible_oil, na.rm = T, vartype = NULL),
                                            digits = 6),
                   reach_sugar = round(survey_mean(sugar, na.rm = T, vartype = NULL),
                                       digits = 6)) %>% 
  ungroup()

# Multiply all numeric values by 100 (percentages): 
zone_reach <- zone_reach %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# Left join shapefile: 
zone_reach <- zone_reach %>% 
  dplyr::left_join(nigeria_zone, by = "zone")

# Convert to shapefile: 
zone_reach <- st_as_sf(zone_reach)

# State:
state_reach <- svy_nga_analysis %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarise(reach_rice = round(survey_mean(rice, na.rm = T, vartype = NULL), 
                                      digits = 6),
                   reach_wheatflour = round(survey_mean(wheatflour, na.rm = T, vartype = NULL),
                                            digits = 6),
                   reach_maizeflour = round(survey_mean(maizeflour, na.rm = T, vartype = NULL), 
                                            digits = 6),
                   reach_edible_oil = round(survey_mean(edible_oil, na.rm = T, vartype = NULL),
                                            digits = 6),
                   reach_sugar = round(survey_mean(sugar, na.rm = T, vartype = NULL),
                                       digits = 6)) %>% 
  ungroup()

# Multiply all numeric values by 100 (percentages):
state_reach <- state_reach %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# Left join to shapefile: 
state_reach <- state_reach %>% 
  dplyr::left_join(nigeria_1, by = "state")

# Convert to shapefile:
state_reach <- st_as_sf(state_reach)

#-------------------------------------------------------------------------------

# MAP COVERAGE: 

# Create function to produce maps: 
plot_map <- function(data, col, title, metric,level, caption) {
  
  # Create a map: 
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 100, by = 10),
            textNA = "Missing Data",
            legend.is.portrait = F) + 
    tm_layout(main.title = title, frame = F, main.title.size = 0.8, 
              main.title.position = "center", legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_text(level, size = 0.5, along.lines = TRUE) +
    tm_borders(lwd = 0.2) + 
    tm_legend(show = F) + 
    tm_credits(caption, position = c("right", "bottom"), size = 0.5)
  
  return(map)
}

# Change name of "South south" in zone_reach: 
zone_reach$zone[zone_reach$zone == "South South"] <- "South South                                               "

# Wheat flour (zone)
wf_zone_reach <- plot_map(data = zone_reach, 
                          col = "reach_wheatflour", 
                          title = "Wheat flour",
                          metric = "Reach (%)",
                          level = "zone",
                          caption = "Source: Nigeria Living Standards Survey 2018-2019")

wf_zone_reach

# tmap_save(wf_zone_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/zone/wheat_flour.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(wf_zone_reach)

# Wheat flour (state)
wf_state_reach <- plot_map(data = state_reach, 
                           col = "reach_wheatflour", 
                           title = "Wheat flour",
                           metric = "Reach (%)",
                           level = "state",
                           caption = "Source: Nigeria Living Standards Survey 2018-2019")

wf_state_reach

# tmap_save(wf_state_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/state/wheat_flour.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(wf_state_reach)

# Rice (zone)
rice_zone_reach <- plot_map(data = zone_reach, 
                            col = "reach_rice", 
                            title = "Rice",
                            metric = "Reach (%)",
                            level = "zone",
                            caption = "Source: Nigeria Living Standards Survey 2018-2019")

rice_zone_reach

# tmap_save(rice_zone_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/zone/rice.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(rice_zone_reach)

# Rice (state)
rice_state_reach <- plot_map(data = state_reach, 
                             col = "reach_rice", 
                             title = "Rice",
                             metric = "Reach (%)",
                             level = "state",
                             caption = "Source: Nigeria Living Standards Survey 2018-2019")

rice_state_reach

# tmap_save(rice_state_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/state/rice.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(rice_state_reach)

# Maize flour (zone)
mf_zone_reach <- plot_map(data = zone_reach, 
                          col = "reach_maizeflour", 
                          title = "Maize flour",
                          metric = "Reach (%)",
                          level = "zone",
                          caption = "Source: Nigeria Living Standards Survey 2018-2019")

mf_zone_reach

# tmap_save(mf_zone_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/zone/maize_flour.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(mf_zone_reach)

# Maize flour (state)
mf_state_reach <- plot_map(data = state_reach, 
                           col = "reach_maizeflour", 
                           title = "Maize flour",
                           metric = "Reach (%)",
                           level = "state",
                           caption = "Source: Nigeria Living Standards Survey 2018-2019")

mf_state_reach

# tmap_save(mf_state_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/state/maize_flour.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(mf_state_reach)

# Edible oil (zone)
eo_zone_reach <- plot_map(data = zone_reach, 
                          col = "reach_edible_oil", 
                          title = "Edible oil",
                          metric = "Reach (%)",
                          level = "zone",
                          caption = "Source: Nigeria Living Standards Survey 2018-2019")

eo_zone_reach

# tmap_save(eo_zone_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/zone/edible_oil.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(eo_zone_reach)

# Edible oil (state)
eo_state_reach <- plot_map(data = state_reach, 
                           col = "reach_edible_oil", 
                           title = "Edible oil",
                           metric = "Reach (%)",
                           level = "state",
                           caption = "Source: Nigeria Living Standards Survey 2018-2019")

eo_state_reach

# tmap_save(eo_state_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/state/edible_oil.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(eo_state_reach)

# Sugar (zone)
sugar_zone_reach <- plot_map(data = zone_reach, 
                             col = "reach_sugar", 
                             title = "Sugar",
                             metric = "Reach (%)",
                             level = "zone",
                             caption = "Source: Nigeria Living Standards Survey 2018-2019")

sugar_zone_reach

# tmap_save(sugar_zone_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/zone/sugar.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(sugar_zone_reach)

# Sugar(state)
sugar_state_reach <- plot_map(data = state_reach, 
                              col = "reach_sugar", 
                              title = "Sugar",
                              metric = "Reach (%)",
                              level = "state",
                              caption = "Source: Nigeria Living Standards Survey 2018-2019")

sugar_state_reach

# tmap_save(sugar_state_reach, "data_rich/data_requests/nigeria_202405/vehicle_coverage/state/sugar.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(sugar_state_reach, zone_reach, state_reach, plot_map)

#-------------------------------------------------------------------------------

# MICRONUTRIENT ADEQUACY




