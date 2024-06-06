################################################################################
######################### NIGERIA MAPS - NFA MEETING ###########################
################################################################################

# Author: Mo Osman
# Date created: 24-May-2024
# Last edited: 05-Jun-2024

# This script is to create additional maps that may be required when presenting
# at the NFA meeting: 
# - Coverage of fortifiable food vehicles at zone and ADM1 level.
# - Inadequate intake of micronutrients at zone, ADM1 and ADM2 level.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("tidyverse", "sf", "tmap", "readr", "rmapshaper", "raster",
                 "ggplot2", "ggspatial", "cowplot", "tmaptools", "terra", 
                 "gridExtra", "srvyr", "viridis", "wesanderson")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# SOURCE REQUIRED FUNCTIONS: 
source("universal_functions/iron_full_probability/src/iron_inad_prev.R")

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
plot_map <- function(data, col, title, metric, level, caption) {
  
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

rm(sugar_state_reach, zone_reach, state_reach, plot_map, svy_nga_analysis)

#-------------------------------------------------------------------------------

# MICRONUTRIENT ADEQUACY

# Firstly define harmonised average requirements: 
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
  zn_mg_ref = 7.5, #refined
  zn_mg_su = 8.9,#semi unrefined
  zn_mg_u = 10.2
)

# Binarise household as adequate = 0, inadequate = 1 based on their apparent intake: 
nga_adequacy <- nga_analysis_df %>% 
  dplyr::select(hhid, vita_rae_mcg, thia_mg, ribo_mg, vitb12_mcg, zn_mg, 
                ca_mg, state, lga, zone, survey_wgt) %>% 
  mutate(vita = ifelse(vita_rae_mcg < allen_har$vita_rae_mcg, 1, 0),
         thia = ifelse(thia_mg < allen_har$thia_mg, 1, 0),
         ribo = ifelse(ribo_mg < allen_har$ribo_mg, 1, 0),
         vitb12 = ifelse(vitb12_mcg < allen_har$vitb12_mcg, 1, 0),
         zn = ifelse(zn_mg < allen_har$zn_mg_u, 1, 0),
         # Used alternative H-AR for refined diet for alignment with the NFCMS 
         # (as estimates will be compared): 
         zn_ref = ifelse(zn_mg < allen_har$zn_mg_ref, 1, 0),
         ca = ifelse(ca_mg < allen_har$ca_mg, 1, 0)) %>%
  dplyr::select(-vita_rae_mcg, -thia_mg, -ribo_mg, -vitb12_mcg, -zn_mg, -ca_mg)

# Create tbl_svy object from nga_adequacy:
svy_nga_adequacy <- nga_adequacy %>% 
  as_survey_design(weights = survey_wgt)

# Calculate survey weighted prevalence of households with inadequate intake (zone): 
zone_adequacy <- svy_nga_adequacy %>% 
  group_by(zone) %>% 
  dplyr::summarise(vita = round(survey_mean(vita, na.rm = T, vartype = NULL), digits = 6),
                   thia = round(survey_mean(thia, na.rm = T, vartype = NULL), digits = 6),
                   ribo = round(survey_mean(ribo, na.rm = T, vartype = NULL), digits = 6),
                   vitb12 = round(survey_mean(vitb12, na.rm = T, vartype = NULL), digits = 6),
                   zn = round(survey_mean(zn, na.rm = T, vartype = NULL), digits = 6),
                   zn_ref = round(survey_mean(zn_ref, na.rm = T, vartype = NULL), digits = 6),
                   ca = round(survey_mean(ca, na.rm = T, vartype = NULL), digits = 6)) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# Calculate zone iron adequacy seperately using the full probability method
# (10% bioavailability):
nga_analysis_df <- nga_analysis_df %>% 
  rename(ai_afe = fe_mg)

zone_fe_adequacy <- fe_full_prob(nga_analysis_df, group1 = zone, bio_avail = 10) %>% 
  rename(zone = subpopulation, 
         fe = prev_inad)

# Left join zone_fe_adequacy to zone_adequacy:
zone_adequacy <- zone_adequacy %>% 
  left_join(zone_fe_adequacy, by = "zone")

rm(zone_fe_adequacy)

# Left join shapefile: 
zone_adequacy <- zone_adequacy %>% 
  left_join(nigeria_zone, by = "zone")

# Convert to shapefile: 
zone_adequacy <- st_as_sf(zone_adequacy)

# Calculate survey weighted prevalence of households with inadequate intake (state):
state_adequacy <- svy_nga_adequacy %>% 
  group_by(state) %>% 
  dplyr::summarise(vita = round(survey_mean(vita, na.rm = T, vartype = NULL), digits = 6),
                   thia = round(survey_mean(thia, na.rm = T, vartype = NULL), digits = 6),
                   ribo = round(survey_mean(ribo, na.rm = T, vartype = NULL), digits = 6),
                   vitb12 = round(survey_mean(vitb12, na.rm = T, vartype = NULL), digits = 6),
                   zn = round(survey_mean(zn, na.rm = T, vartype = NULL), digits = 6),
                   zn_ref = round(survey_mean(zn_ref, na.rm = T, vartype = NULL), digits = 6),
                   ca = round(survey_mean(ca, na.rm = T, vartype = NULL), digits = 6)) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# Calculate state iron adequacy seperately using the full probability method: 
state_fe_adequacy <- fe_full_prob(nga_analysis_df, group1 = state, bio_avail = 10) %>% 
  rename(state = subpopulation, 
         fe = prev_inad)

# Left join state_fe_adequacy to state_adequacy:
state_adequacy <- state_adequacy %>% 
  left_join(state_fe_adequacy, by = "state")

rm(state_fe_adequacy)

# Left join shapefile:
state_adequacy <- state_adequacy %>% 
  left_join(nigeria_1, by = "state")

# Convert to shapefile:
state_adequacy <- st_as_sf(state_adequacy)

# Calculate survey weighted prevalence of households with inadequate intake (lga):
lga_adequacy <- svy_nga_adequacy %>% 
  group_by(lga) %>% 
  dplyr::summarise(vita = round(survey_mean(vita, na.rm = T, vartype = NULL), digits = 6),
                   thia = round(survey_mean(thia, na.rm = T, vartype = NULL), digits = 6),
                   ribo = round(survey_mean(ribo, na.rm = T, vartype = NULL), digits = 6),
                   vitb12 = round(survey_mean(vitb12, na.rm = T, vartype = NULL), digits = 6),
                   zn = round(survey_mean(zn, na.rm = T, vartype = NULL), digits = 6),
                   zn_ref = round(survey_mean(zn_ref, na.rm = T, vartype = NULL), digits = 6),
                   ca = round(survey_mean(ca, na.rm = T, vartype = NULL), digits = 6)) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# Calculate lga iron adequacy seperately using the full probability method:
lga_fe_adequacy <- fe_full_prob(nga_analysis_df, group1 = lga, bio_avail = 10) %>% 
  rename(lga = subpopulation, 
         fe = prev_inad)

# Left join lga_fe_adequacy to lga_adequacy:
lga_adequacy <- lga_adequacy %>% 
  left_join(lga_fe_adequacy, by = "lga")

rm(lga_fe_adequacy)

# left join lga_adequacy to shapefile (need to do it this way round so that lga's
# with missing data are still mapped): 
lga_adequacy <- nigeria_2 %>% 
  left_join(lga_adequacy, by = "lga")

# Convert to shapefile:
lga_adequacy <- st_as_sf(lga_adequacy)

#-------------------------------------------------------------------------------

# MAP ADEQUACY 

# Change name of "South south" in zone_adequacy: 
zone_adequacy$zone[zone_adequacy$zone == "South South"] <- "South South                                               "

# Write a function to do so: 
plot_map <- function(data, col, title, metric, level, caption) {
  
  # Create a map: 
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 100, by = 10),
            textNA = "Missing Data",
            legend.is.portrait = F,
            palette = wesanderson::wes_palette("Zissou1Continuous")) + 
    tm_layout(main.title = title, frame = F, main.title.size = 0.8, 
              main.title.position = "center", legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_text(level, size = 0.5) +
    tm_borders(lwd = 0.2) + 
    tm_legend(show = F) + 
    tm_credits(caption, position = c("right", "bottom"), size = 0.5)
  
  return(map)
}

# Vitamin A (zone)
vita_zone <- plot_map(data = zone_adequacy, 
                      col = "vita", 
                      title = "Vitamin A", 
                      metric = "Prevalence of inadequate intake (%)", 
                      level = "zone", 
                      caption = "Source: Nigeria Living Standards Survey 2018-2019")

vita_zone

# tmap_save(vita_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/vita_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(vita_zone)

# Vitamin A (state)
vita_state <- plot_map(data = state_adequacy, 
                       col = "vita", 
                       title = "Vitamin A", 
                       metric = "Prevalence of inadequate intake (%)", 
                       level = "state", 
                       caption = "Source: Nigeria Living Standards Survey 2018-2019")

vita_state

# tmap_save(vita_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/vita_state.png", 
#           width = 6, height = 6, units = "in", dpi = 600)

rm(vita_state)

# LGA maps will need to be computed with a different function due to different 
# requirements due to missing data and high number of LGA's.

# Thiamine (zone)
thia_zone <- plot_map(data = zone_adequacy, 
                      col = "thia", 
                      title = "Thiamine", 
                      metric = "Prevalence of inadequate intake (%)", 
                      level = "zone", 
                      caption = "Source: Nigeria Living Standards Survey 2018-2019")

thia_zone

# tmap_save(thia_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/thia_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(thia_zone)

# Thiamine (state)
thia_state <- plot_map(data = state_adequacy, 
                       col = "thia", 
                       title = "Thiamine", 
                       metric = "Prevalence of inadequate intake (%)", 
                       level = "state", 
                       caption = "Source: Nigeria Living Standards Survey 2018-2019")

thia_state

# tmap_save(thia_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/thia_state.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(thia_state)

# Riboflavin (zone)
ribo_zone <- plot_map(data = zone_adequacy, 
                      col = "ribo", 
                      title = "Riboflavin", 
                      metric = "Prevalence of inadequate intake (%)", 
                      level = "zone", 
                      caption = "Source: Nigeria Living Standards Survey 2018-2019")

ribo_zone

# tmap_save(ribo_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/ribo_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(ribo_zone)

# Riboflavin (state)
ribo_state <- plot_map(data = state_adequacy, 
                       col = "ribo", 
                       title = "Riboflavin", 
                       metric = "Prevalence of inadequate intake (%)", 
                       level = "state", 
                       caption = "Source: Nigeria Living Standards Survey 2018-2019")

ribo_state

# tmap_save(ribo_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/ribo_state.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(ribo_state)

# Vitamin B12 (zone)
vitb12_zone <- plot_map(data = zone_adequacy, 
                        col = "vitb12", 
                        title = "Vitamin B12", 
                        metric = "Prevalence of inadequate intake (%)", 
                        level = "zone", 
                        caption = "Source: Nigeria Living Standards Survey 2018-2019")

vitb12_zone

# tmap_save(vitb12_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/vitb12_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(vitb12_zone)

# Vitamin B12 (state)
vitb12_state <- plot_map(data = state_adequacy, 
                         col = "vitb12", 
                         title = "Vitamin B12", 
                         metric = "Prevalence of inadequate intake (%)", 
                         level = "state", 
                         caption = "Source: Nigeria Living Standards Survey 2018-2019")

vitb12_state

# tmap_save(vitb12_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/vitb12_state.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(vitb12_state)

# Iron (zone)
iron_zone <- plot_map(data = zone_adequacy, 
                      col = "fe", 
                      title = "Iron", 
                      metric = "Prevalence of inadequate intake (%)", 
                      level = "zone", 
                      caption = "Source: Nigeria Living Standards Survey 2018-2019")

iron_zone

# tmap_save(iron_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/iron_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(iron_zone)

# Iron (state)
iron_state <- plot_map(data = state_adequacy, 
                       col = "fe", 
                       title = "Iron", 
                       metric = "Prevalence of inadequate intake (%)", 
                       level = "state", 
                       caption = "Source: Nigeria Living Standards Survey 2018-2019")

iron_state

# tmap_save(iron_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/iron_state.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(iron_state)

# Zinc (zone)
zn_zone <- plot_map(data = zone_adequacy, 
                    col = "zn", 
                    title = "Zinc", 
                    metric = "Prevalence of inadequate intake (%)", 
                    level = "zone", 
                    caption = "Source: Nigeria Living Standards Survey 2018-2019")

zn_zone

# tmap_save(zn_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/zn_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(zn_zone)

# Zinc (state)
zn_state <- plot_map(data = state_adequacy, 
                     col = "zn", 
                     title = "Zinc", 
                     metric = "Prevalence of inadequate intake (%)", 
                     level = "state", 
                     caption = "Source: Nigeria Living Standards Survey 2018-2019")

zn_state

# tmap_save(zn_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/zn_state.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(zn_state)

# Zinc refined (zone)
zn_ref_zone <- plot_map(data = zone_adequacy, 
                        col = "zn_ref", 
                        title = "Zinc (refined diet)", 
                        metric = "Prevalence of inadequate intake (%)", 
                        level = "zone", 
                        caption = "Source: Nigeria Living Standards Survey 2018-2019")

zn_ref_zone

# tmap_save(zn_ref_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/zn_ref_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(zn_ref_zone)

# Zinc refined (state)
zn_ref_state <- plot_map(data = state_adequacy, 
                         col = "zn_ref", 
                         title = "Zinc (refined diet)", 
                         metric = "Prevalence of inadequate intake (%)", 
                         level = "state", 
                         caption = "Source: Nigeria Living Standards Survey 2018-2019")

zn_ref_state

# tmap_save(zn_ref_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/zn_ref_state.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(zn_ref_state)

# Calcium (zone)
ca_zone <- plot_map(data = zone_adequacy, 
                    col = "ca", 
                    title = "Calcium", 
                    metric = "Prevalence of inadequate intake (%)", 
                    level = "zone", 
                    caption = "Source: Nigeria Living Standards Survey 2018-2019")

ca_zone

# tmap_save(ca_zone, "data_rich/data_requests/nigeria_202405/inadequate_intake/zone/ca_zone.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(ca_zone)

# Calcium (state)
ca_state <- plot_map(data = state_adequacy, 
                     col = "ca", 
                     title = "Calcium", 
                     metric = "Prevalence of inadequate intake (%)", 
                     level = "state", 
                     caption = "Source: Nigeria Living Standards Survey 2018-2019")

ca_state

# tmap_save(ca_state, "data_rich/data_requests/nigeria_202405/inadequate_intake/state/ca_state.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(ca_state)

#-------------------------------------------------------------------------------

# MAPPING ADEQUACY AT THE LGA LEVEL: 

# Need to edit the function that was previously used:
plot_map <- function(data, col, title, metric, level, caption) {
  
  # Create a map: 
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 100, by = 10),
            textNA = "Missing Data",
            legend.is.portrait = F,
            palette = wesanderson::wes_palette("Zissou1Continuous")) + 
    tm_layout(main.title = title, frame = F, main.title.size = 0.8, 
              main.title.position = "center", legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_borders(lwd = 0.00001) + 
    tm_legend(show = F) + 
    tm_shape(nigeria_1) +
    tm_borders(col = "black", lwd = 1) + 
    tm_credits(caption, position = c("right", "bottom"), size = 0.5)
  
  return(map)
}

# Vitamin A (lga)
va_lga <- plot_map(data = lga_adequacy, 
                   col = "vita", 
                   title = "Vitamin A", 
                   metric = "Prevalence of inadequate intake (%)", 
                   level = "lga", 
                   caption = "Source: Nigeria Living Standards Survey 2018-2019")

va_lga

# tmap_save(va_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/va_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(va_lga)

# Thiamine (lga)
thiamine_lga <- plot_map(data = lga_adequacy, 
                         col = "thia", 
                         title = "Thiamine", 
                         metric = "Prevalence of inadequate intake (%)", 
                         level = "lga", 
                         caption = "Source: Nigeria Living Standards Survey 2018-2019")

thiamine_lga

# tmap_save(thiamine_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/thiamine_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(thiamine_lga)

# Riboflavin (lga)
riboflavin_lga <- plot_map(data = lga_adequacy, 
                           col = "ribo", 
                           title = "Riboflavin", 
                           metric = "Prevalence of inadequate intake (%)", 
                           level = "lga", 
                           caption = "Source: Nigeria Living Standards Survey 2018-2019")

riboflavin_lga

# tmap_save(riboflavin_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/riboflavin_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(riboflavin_lga)

# Vitamin B12 (lga)
vitb12_lga <- plot_map(data = lga_adequacy, 
                       col = "vitb12", 
                       title = "Vitamin B12", 
                       metric = "Prevalence of inadequate intake (%)", 
                       level = "lga", 
                       caption = "Source: Nigeria Living Standards Survey 2018-2019")

vitb12_lga

# tmap_save(vitb12_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/vitb12_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(vitb12_lga)

# Iron (lga)
iron_lga <- plot_map(data = lga_adequacy, 
                     col = "fe", 
                     title = "Iron", 
                     metric = "Prevalence of inadequate intake (%)", 
                     level = "lga", 
                     caption = "Source: Nigeria Living Standards Survey 2018-2019")

iron_lga

# tmap_save(iron_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/iron_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(iron_lga)

# Zinc (lga)
zn_lga <- plot_map(data = lga_adequacy, 
                   col = "zn", 
                   title = "Zinc", 
                   metric = "Prevalence of inadequate intake (%)", 
                   level = "lga", 
                   caption = "Source: Nigeria Living Standards Survey 2018-2019")

zn_lga

# tmap_save(zn_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/zn_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(zn_lga)

# Zinc refined (lga)
zn_ref_lga <- plot_map(data = lga_adequacy, 
                      col = "zn_ref", 
                      title = "Zinc (Refined diet)", 
                      metric = "Prevalence of inadequate intake (%)", 
                      level = "lga", 
                      caption = "Source: Nigeria Living Standards Survey 2018-2019")

zn_ref_lga

# tmap_save(zn_ref_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/zn_ref_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(zn_ref_lga)

# Calcium (lga)
ca_lga <- plot_map(data = lga_adequacy, 
                   col = "ca", 
                   title = "Calcium", 
                   metric = "Prevalence of inadequate intake (%)", 
                   level = "lga", 
                   caption = "Source: Nigeria Living Standards Survey 2018-2019")

ca_lga

# tmap_save(ca_lga, "data_rich/data_requests/nigeria_202405/inadequate_intake/lga/ca_lga.png",
#           width = 6, height = 6, units = "in", dpi = 600)

rm(ca_lga)

#-------------------------------------------------------------------------------

# Clear environment: 
rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################

