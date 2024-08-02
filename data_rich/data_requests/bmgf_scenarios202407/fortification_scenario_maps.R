################################################################################
################### NIGERIA MAPS - FORTIFICATION SCENARIOS #####################
################################################################################

# Author: Mo Osman
# Collaborator: Kevin Tang
# Date created: 29-Jul-2024
# Last edited: 30-Jul-2024

# This script is to map base case adequacy of Thiamin and Riboflavin in Nigeria, 
# and also to map the impact of fortification scenarios (rice, wheat flour and 
# boullion) on the adequacy of Thiamin and Riboflavin in Nigeria.

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

# READ IN DATA: 

# Shapefile data: 
nigeria_0 <- st_read("map_data/nga/new_shapefiles/nigeria_0")
nigeria_1 <- st_read("map_data/nga/new_shapefiles/nigeria_1")
nigeria_2 <- st_read("map_data/nga/new_shapefiles/nigeria_2")

# Read in household locations: 
hh_locations <- read_csv("map_data/nga/new_shapefiles/household_locations.csv") %>% 
  dplyr::select(hhid, state, lga)

# Read in pre-processed base apparent intake and fortification data: 
apparent_intake <- read_csv("data_rich/data_requests/bmgf_scenarios202407/nga_lss1819_bmgfrequest2.csv")

# Set E-AR values: 
ear <- data_frame(nutrient = c("thia_mg", "ribo_mg"),
                  ear = c(0.9, 1.3))

#-------------------------------------------------------------------------------

# BINARISE APPARENT INTAKE: 

apparent_intake <- apparent_intake %>% 
  mutate(base_thia_inadequacy = ifelse(thia_ai <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         base_ribo_inadequacy = ifelse(ribo_ai <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         wf_thia_inadequacy = ifelse((thia_ai + thia_wheatflour) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         wf_ribo_inadequacy = ifelse((ribo_ai + ribo_wheatflour) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         rice_thia_inadequacy = ifelse((thia_ai + thia_rice) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         rice_ribo_inadequacy = ifelse((ribo_ai + ribo_rice) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         bouillon_thia_inadequacy1 = ifelse((thia_ai + thia_bouillon1) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         bouillon_ribo_inadequacy1 = ifelse((ribo_ai + ribo_bouillon1) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         bouillon_thia_inadequacy2 = ifelse((thia_ai + thia_bouillon2) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         bouillon_ribo_inadequacy2 = ifelse((ribo_ai + ribo_bouillon2) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         all_thia_inadequacy1 = ifelse((thia_ai + thia_allvehicles1) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         all_thia_inadequacy2 = ifelse((thia_ai + thia_allvehicles2) <= ear$ear[ear$nutrient == "thia_mg"], 1, 0),
         all_ribo_inadequacy1 = ifelse((ribo_ai + ribo_allvehicles1) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0),
         all_ribo_inadequacy2 = ifelse((ribo_ai + ribo_allvehicles2) <= ear$ear[ear$nutrient == "ribo_mg"], 1, 0))

# De-select variables that are no longer required: 
apparent_intake <- apparent_intake %>% 
  select(-c(thia_ai, ribo_ai, thia_wheatflour, ribo_wheatflour, thia_rice, 
            ribo_rice, thia_bouillon1, ribo_bouillon1, thia_bouillon2, 
            ribo_bouillon2, thia_allvehicles1, ribo_allvehicles1,
            thia_allvehicles2, ribo_allvehicles2, zone, adm1, adm2, 
            res, sep_quintile, res_quintile, age_head, sex_head, educ_head, 
            year, month, afe, ea))

#-------------------------------------------------------------------------------

# JOIN HOUSEHOLD LOCATIONS READY FOR MAPPING: 
apparent_intake <- apparent_intake %>% 
  left_join(hh_locations, by = "hhid")

#-------------------------------------------------------------------------------

# MN ADEQUACY CALCULATIONS AT STATE AND LGA LEVEL

# Firstly need to create tbl_svy object for survey weighted analysis: 
svy_apparent_intake <- apparent_intake %>% 
  as_survey_design(weights = survey_wgt)

# Calculate inadequacy aggregated at state and lga level for all the different 
# scenarios, and then join to shapefiles ready for mapping.

# Thiamine (state):
thia_state_inadequacy <- svy_apparent_intake %>% 
  group_by(state) %>%
  summarise(base_case = round(survey_mean(base_thia_inadequacy, na.rm = T, 
                                          vartype = NULL), 4),
            wf_fort = round(survey_mean(wf_thia_inadequacy, na.rm = T,
                                   vartype = NULL), 4),
            rice_fort = round(survey_mean(rice_thia_inadequacy, na.rm = T,
                                     vartype = NULL), 4),
            bouillon_fort1 = round(survey_mean(bouillon_thia_inadequacy1, na.rm = T,
                                         vartype = NULL), 4),
            bouillon_fort2 = round(survey_mean(bouillon_thia_inadequacy2, na.rm = T,
                                         vartype = NULL), 4),
            all_vehicles_fort1 = round(survey_mean(all_thia_inadequacy1, na.rm = T,
                                           vartype = NULL), 4),
            all_vehicles_fort2 = round(survey_mean(all_thia_inadequacy2, na.rm = T,
                                           vartype = NULL), 4)
            ) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# write_csv(thia_state_inadequacy, "data_rich/data_requests/bmgf_scenarios202407/thiamine/thia_state_inadequacy.csv")

thia_state_inadequacy <- nigeria_1 %>% 
  left_join(thia_state_inadequacy, by = c("state" = "state"))

thia_state_inadequacy <- st_as_sf(thia_state_inadequacy)

# Thiamine (lga): 
thia_lga_inadequacy <- svy_apparent_intake %>% 
  group_by(lga) %>%
  summarise(base_case = round(survey_mean(base_thia_inadequacy, na.rm = T, 
                                          vartype = NULL), 4),
            wf_fort = round(survey_mean(wf_thia_inadequacy, na.rm = T,
                                   vartype = NULL), 4),
            rice_fort = round(survey_mean(rice_thia_inadequacy, na.rm = T,
                                     vartype = NULL), 4),
            bouillon_fort1 = round(survey_mean(bouillon_thia_inadequacy1, na.rm = T,
                                         vartype = NULL), 4),
            bouillon_fort2 = round(survey_mean(bouillon_thia_inadequacy2, na.rm = T,
                                         vartype = NULL), 4),
            all_vehicles_fort1 = round(survey_mean(all_thia_inadequacy1, na.rm = T,
                                           vartype = NULL), 4),
            all_vehicles_fort2 = round(survey_mean(all_thia_inadequacy2, na.rm = T,
                                           vartype = NULL), 4)
            ) %>% 
  ungroup() %>%
  mutate(across(where(is.numeric), ~ .x * 100))

thia_lga_inadequacy <- nigeria_2 %>%
  left_join(thia_lga_inadequacy, by = c("lga" = "lga"))

thia_lga_inadequacy <- st_as_sf(thia_lga_inadequacy)

# Riboflavin (state):
ribo_state_inadequacy <- svy_apparent_intake %>% 
  group_by(state) %>%
  summarise(base_case = round(survey_mean(base_ribo_inadequacy, na.rm = T, 
                                          vartype = NULL), 4),
            wf_fort = round(survey_mean(wf_ribo_inadequacy, na.rm = T,
                                   vartype = NULL), 4),
            rice_fort = round(survey_mean(rice_ribo_inadequacy, na.rm = T,
                                     vartype = NULL), 4),
            bouillon_fort1 = round(survey_mean(bouillon_ribo_inadequacy1, na.rm = T,
                                         vartype = NULL), 4),
            bouillon_fort2 = round(survey_mean(bouillon_ribo_inadequacy2, na.rm = T,
                                         vartype = NULL), 4),
            all_vehicles_fort = round(survey_mean(all_ribo_inadequacy1, na.rm = T,
                                           vartype = NULL), 4),
            all_vehicles_fort2 = round(survey_mean(all_ribo_inadequacy2, na.rm = T,
                                           vartype = NULL), 4)
            ) %>% 
  ungroup() %>%
  mutate(across(where(is.numeric), ~ .x * 100))

# write_csv(ribo_state_inadequacy, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/ribo_state_inadequacy.csv")

ribo_state_inadequacy <- nigeria_1 %>%
  left_join(ribo_state_inadequacy, by = c("state" = "state"))

ribo_state_inadequacy <- st_as_sf(ribo_state_inadequacy)

# Riboflavin (lga):
ribo_lga_inadequacy <- svy_apparent_intake %>% 
  group_by(lga) %>%
  summarise(base_case = round(survey_mean(base_ribo_inadequacy, na.rm = T, 
                                          vartype = NULL), 4),
            wf_fort = round(survey_mean(wf_ribo_inadequacy, na.rm = T,
                                   vartype = NULL), 4),
            rice_fort = round(survey_mean(rice_ribo_inadequacy, na.rm = T,
                                     vartype = NULL), 4),
            bouillon_fort1 = round(survey_mean(bouillon_ribo_inadequacy1, na.rm = T,
                                         vartype = NULL), 4),
            bouillon_fort2 = round(survey_mean(bouillon_ribo_inadequacy2, na.rm = T,
                                         vartype = NULL), 4),
            all_vehicles_fort1 = round(survey_mean(all_ribo_inadequacy1, na.rm = T,
                                           vartype = NULL), 4),
            all_vehicles_fort2 = round(survey_mean(all_ribo_inadequacy2, na.rm = T,
                                           vartype = NULL), 4)
            ) %>% 
  ungroup() %>%
  mutate(across(where(is.numeric), ~ .x * 100))

ribo_lga_inadequacy <- nigeria_2 %>%
  left_join(ribo_lga_inadequacy, by = c("lga" = "lga"))

ribo_lga_inadequacy <- st_as_sf(ribo_lga_inadequacy)

#-------------------------------------------------------------------------------

# PRODUCE MAPS

# Write a function to do so: 
plot_map <- function(data, col, title, metric) {
  
  map <- tm_shape(data) + 
    tm_fill(col = col,
            title = metric, 
            style = "cont",
            breaks = seq(0, 100, by = 10),
            textNA = "Missing Data",
            legend.is.portrait = F,
            palette = wesanderson::wes_palette("Zissou1Continuous")) + 
    tm_layout(main.title = title, frame = F, main.title.size = 1, 
              main.title.position = "center", main.title.fontface = 2,
              legend.outside.position = "bottom",
              legend.outside.size = 0.35) +
    tm_borders(lwd = 0) + 
    tm_legend(show = F) +
    tm_shape(nigeria_1) +
    tm_borders(lwd = 0.8, col = "black")
  
  return(map)
}

# Maps for Thiamine inadequacy:

thia_base <- plot_map(data = thia_lga_inadequacy,
                      col = "base_case",
                      title = "Base inadequacy of Thiamine (Vitamin B1) - No fortification", 
                      metric = "Prevalence of inadequate Thiamine intake (%)")

thia_base

# tmap_save(thia_base, "data_rich/data_requests/bmgf_scenarios202407/thiamine/base_scenario.png",
#           width = 6, height = 6, units = "in", dpi = 600)

thia_wf <- plot_map(data = thia_lga_inadequacy,
                    col = "wf_fort",
                    title = "Inadequacy of Thiamine (Vitamin B1) - Wheat Flour fortified", 
                    metric = "Prevalence of inadequate Thiamine intake (%)")

thia_wf

# tmap_save(thia_wf, "data_rich/data_requests/bmgf_scenarios202407/thiamine/wheat_flour_scenario.png",
#           width = 6, height = 6, units = "in", dpi = 600)

thia_rice <- plot_map(data = thia_lga_inadequacy,
                      col = "rice_fort",
                      title = "Inadequacy of Thiamine (Vitamin B1) - Rice fortified", 
                      metric = "Prevalence of inadequate Thiamine intake (%)")

thia_rice

# tmap_save(thia_rice, "data_rich/data_requests/bmgf_scenarios202407/thiamine/rice_scenario.png",
#           width = 6, height = 6, units = "in", dpi = 600)

thia_bouillon1 <- plot_map(data = thia_lga_inadequacy,
                          col = "bouillon_fort1",
                          title = "Inadequacy of Thiamine (Vitamin B1) \n Bouillon fortified 15% CODEX NRV (0.18mg) per 2.5g serving", 
                          metric = "Prevalence of inadequate Thiamine intake (%)")

thia_bouillon1

# tmap_save(thia_bouillon1, "data_rich/data_requests/bmgf_scenarios202407/thiamine/bouillon_scenario1.png",
#           width = 6, height = 6, units = "in", dpi = 600)

thia_bouillon2 <- plot_map(data = thia_lga_inadequacy,
                          col = "bouillon_fort2",
                          title = "Inadequacy of Thiamine (Vitamin B1) \n Bouillon fortified 15% CODEX NRV (0.18mg) per 6.3g serving", 
                          metric = "Prevalence of inadequate Thiamine intake (%)")

thia_bouillon2

# tmap_save(thia_bouillon2, "data_rich/data_requests/bmgf_scenarios202407/thiamine/bouillon_scenario2.png",
#           width = 6, height = 6, units = "in", dpi = 600)

thia_all1 <- plot_map(data = thia_lga_inadequacy,
                     col = "all_vehicles_fort1",
                     title = "Inadequacy of Thiamine (Vitamin B1) - All vehicles fortified \n Bouillon 15% CODEX NRV (0.18mg) per 2.5g serving", 
                     metric = "Prevalence of inadequate Thiamine intake (%)")

thia_all1

# tmap_save(thia_all1, "data_rich/data_requests/bmgf_scenarios202407/thiamine/all_vehicles_scenario1.png",
#           width = 6, height = 6, units = "in", dpi = 600)

thia_all2 <- plot_map(data = thia_lga_inadequacy,
                     col = "all_vehicles_fort2",
                     title = "Inadequacy of Thiamine (Vitamin B1) - All vehicles fortified \n Bouillon 15% CODEX NRV (0.18mg) per 6.3g serving", 
                     metric = "Prevalence of inadequate Thiamine intake (%)")

thia_all2

# tmap_save(thia_all2, "data_rich/data_requests/bmgf_scenarios202407/thiamine/all_vehicles_scenario2.png",
#           width = 6, height = 6, units = "in", dpi = 600)

#-------------------------------------------------------------------------------

# Maps for Riboflavin inadequacy:

ribo_base <- plot_map(data = ribo_lga_inadequacy,
                      col = "base_case",
                      title = "Base inadequacy of Riboflavin (Vitamin B2) - No fortification", 
                      metric = "Prevalence of inadequate Riboflavin intake (%)")

ribo_base

# tmap_save(ribo_base, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/base_scenario.png",
#           width = 6, height = 6, units = "in", dpi = 600)

ribo_wf <- plot_map(data = ribo_lga_inadequacy,
                    col = "wf_fort",
                    title = "Inadequacy of Riboflavin (Vitamin B2) - Wheat Flour fortified", 
                    metric = "Prevalence of inadequate Riboflavin intake (%)")

ribo_wf

# tmap_save(ribo_wf, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/wheat_flour_scenario.png",
#           width = 6, height = 6, units = "in", dpi = 600)

ribo_rice <- plot_map(data = ribo_lga_inadequacy,
                      col = "rice_fort",
                      title = "Inadequacy of Riboflavin (Vitamin B2) - Rice fortified", 
                      metric = "Prevalence of inadequate Riboflavin intake (%)")

ribo_rice

# tmap_save(ribo_rice, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/rice_scenario.png",
#           width = 6, height = 6, units = "in", dpi = 600)

ribo_bouillon1 <- plot_map(data = ribo_lga_inadequacy,
                          col = "bouillon_fort1",
                          title = "Inadequacy of Riboflavin (Vitamin B2) \n Bouillon fortified 15% CODEX NRV (0.18mg) per 2.5g serving", 
                          metric = "Prevalence of inadequate Riboflavin intake (%)")

ribo_bouillon1

# tmap_save(ribo_bouillon1, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/bouillon_scenario1.png",
#           width = 6, height = 6, units = "in", dpi = 600)

ribo_bouillon2 <- plot_map(data = ribo_lga_inadequacy,
                          col = "bouillon_fort2",
                          title = "Inadequacy of Riboflavin (Vitamin B2) \n Bouillon fortified 15% CODEX NRV (0.18mg) per 6.3g serving", 
                          metric = "Prevalence of inadequate Riboflavin intake (%)")

ribo_bouillon2

# tmap_save(ribo_bouillon2, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/bouillon_scenario2.png",
#           width = 6, height = 6, units = "in", dpi = 600)

ribo_all1 <- plot_map(data = ribo_lga_inadequacy,
                     col = "all_vehicles_fort1",
                     title = "Inadequacy of Riboflavin (Vitamin B2) - All vehicles fortified \n Bouillon 15% CODEX NRV (0.18mg) per 2.5g serving",
                     metric = "Prevalence of inadequate Riboflavin intake (%)")

ribo_all1

# tmap_save(ribo_all1, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/all_vehicles_scenario1.png",
#           width = 6, height = 6, units = "in", dpi = 600)

ribo_all2 <- plot_map(data = ribo_lga_inadequacy,
                     col = "all_vehicles_fort2",
                     title = "Inadequacy of Riboflavin (Vitamin B2) - All vehicles fortified \n Bouillon 15% CODEX NRV (0.18mg) per 6.3g serving", 
                     metric = "Prevalence of inadequate Riboflavin intake (%)")

ribo_all2

# tmap_save(ribo_all2, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/all_vehicles_scenario2.png",
#           width = 6, height = 6, units = "in", dpi = 600)

# CREATE LEGEND: 

# Create a legend for the maps:
legend <- tm_shape(thia_lga_inadequacy) + 
  tm_fill(col = "base_case", 
          palette = wesanderson::wes_palette("Zissou1Continuous"),
          style = "cont",
          breaks = seq(0, 100, by = 10),
          textNA= "Missing Data",
          title = "Prevalence of inadequate micronutnient take (%)",
          legend.is.portrait = F) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1,
            title.position = c(0.5, 0.5))

legend

# tmap_save(legend, "data_rich/data_requests/bmgf_scenarios202407/thiamine/legend.png",
#           width = 6, height = 6, units = "in", dpi = 600)
# 
# tmap_save(legend, "data_rich/data_requests/bmgf_scenarios202407/riboflavin/legend.png",
#           width = 6, height = 6, units = "in", dpi = 600)

#-------------------------------------------------------------------------------

# Clear environment: 
rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################
