################################################################################
######################### BMGF REQUEST - NIGERIA MAPS ##########################
################################################################################

# Author: Mo Osman
# Date created: 15-May-2024
# Last edited: 23-May-2024

# This script has been created to create maps of VMD, dietary inadequacy and vehicle 
# coverage for the 6 geopolitical zones in Nigeria.

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

# READ IN REQUIRED DATA: 

# Read in estimates for Nigeria: 
nga_estimates <- read_csv("data_rich/data_requests/bmgf_202405/nigeria/nga_estimates.csv")

# Read in "cover" data, this will be used to determine which states belong to 
# which geopolitical zone: 
cover <- read_csv("survey_data/nga/secta_cover.csv") %>% 
  dplyr::select(zone, state) %>% 
  distinct()

# Read in shapefiles for mapping: 
nigeria_0 <- st_read("map_data/nga/new_shapefiles/nigeria_0")
nigeria_1 <- st_read("map_data/nga/new_shapefiles/nigeria_1")

# Check polygons: 
plot(nigeria_0$geometry)
plot(nigeria_1$geometry)

#-------------------------------------------------------------------------------

# Rename entries in "zone": 
cover$zone <- case_when(
  cover$zone == 1 ~ "North Central",
  cover$zone == 2 ~ "North East",
  cover$zone == 3 ~ "North West",
  cover$zone == 4 ~ "South East",
  cover$zone == 5 ~ "South South",
  cover$zone == 6 ~ "South West"
)

#-------------------------------------------------------------------------------

# Get "state" names and rename entries: 
household_locations <- read_csv("map_data/nga/new_shapefiles/household_locations.csv") %>% 
  dplyr::left_join(read_csv("survey_data/nga/secta_cover.csv"), by = "hhid") %>% 
  dplyr::select(state.x, state.y) %>% 
  distinct() %>% 
  rename(state = state.y,
         state_name = state.x)

# Left join state_names to cover: 
cover <- cover %>% 
  left_join(household_locations, by = "state")

rm(household_locations)

cover <- cover %>% 
  dplyr::select(-state) %>% 
  rename(state = state_name)

#-------------------------------------------------------------------------------

# JOIN ZONES TO SHAPEFILE: 

nigeria_1 <- nigeria_1 %>% 
  left_join(cover, by = "state")

# Use this to create a new shapefile - grouped by zone:
nigeria_zone <- nigeria_1 %>% 
  group_by(zone) %>% 
  mutate(geometry = sf::st_union(geometry)) %>%
  slice(1)

nigeria_zone <- nigeria_zone %>% 
  dplyr::select(-state)

# Check geometry: 
plot(nigeria_zone$geometry)

#-------------------------------------------------------------------------------

# SELECT VARIABLES FOR MAPPING - VEHICLE COVERAGE:
nga_coverage <- nga_estimates %>% 
  dplyr::select(zone, wflour_cov, rice_cov, mflour_cov, semolina_cov, oil_cov,
                sugat_cov, salt_cov, bouillon_cov) %>% 
  rename(sugar_cov = sugat_cov)

# Left join the shapefile geometry: 
nga_coverage <- nga_coverage %>% 
  left_join(nigeria_zone, by = "zone")

# Remove the national estimates: 
nga_coverage <- nga_coverage %>% 
  filter(zone != "National")

# Convert nga_coverage to "sf"
nga_coverage <- st_as_sf(nga_coverage)

# SELECT VARIABLES FOR MAPPING - VMD:
nga_vmd <- nga_estimates %>% 
  dplyr::select(zone, fe_vmd, zn_vmd, va_vmd, fol_vmd, thia_vmd, ribo_vmd, 
                vb12_vmd)

# Left join the shapefile geometry:
nga_vmd <- nga_vmd %>% 
  left_join(nigeria_zone, by = "zone")

# Remove the national estimates: 
nga_vmd <- nga_vmd %>% 
  filter(zone != "National")

# Convert nga_vmd to "sf"
nga_vmd <- st_as_sf(nga_vmd)

# SELECT VARIABLES FOR MAPPING - DIETARY INADEQUACY:
nga_inad <- nga_estimates %>% 
  dplyr::select(zone, fe_inad, zn_inad, va_inad, fol_inad, thia_inad, ribo_inad, 
                vb12_inad, ca_inad)

# Left join the shapefile geometry:
nga_inad <- nga_inad %>% 
  left_join(nigeria_zone, by = "zone")

# Remove the national estimates:
nga_inad <- nga_inad %>% 
  filter(zone != "National")

# Convert nga_inad to "sf"
nga_inad <- st_as_sf(nga_inad)

rm(nga_estimates, cover)

#-------------------------------------------------------------------------------

# PRODUCE MAPS - VEHICLE COVERAGE: 

# Create a function to plot maps:
plot_map <- function(data, col, title, metric, caption) {
  
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
    tm_text("zone", size = 0.5, along.lines = TRUE) +
    tm_borders(lwd = 0.2) + 
    tm_legend(show = F) + 
    tm_credits(caption, position = c("right", "bottom"), size = 0.5)
  
  return(map)
}

# Change name of South South in nga_coverage: 
nga_coverage$zone[nga_coverage$zone == "South South"] <- "South South                                               "

# Wheat flour: 
wf_cov <- plot_map(data = nga_coverage,
         col = "wflour_cov",
         title = "Wheat Flour ",
         metric = "Coverage (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

wf_cov

# tmap_save(wf_cov, "data_rich/data_requests/bmgf_202405/nigeria/vehicle_coverage/wflour.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(wf_cov)

# Maize flour: 
mf_cov <- plot_map(data = nga_coverage,
         col = "mflour_cov",
         title = "Maize Flour",
         metric = "Coverage (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

mf_cov

# tmap_save(mf_cov, "data_rich/data_requests/bmgf_202405/nigeria/vehicle_coverage/mflour.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(mf_cov)

# Semolina: 
sem_cov <- plot_map(data = nga_coverage,
         col = "semolina_cov",
         title = "Semolina",
         metric = "Coverage (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

sem_cov

# tmap_save(sem_cov, "data_rich/data_requests/bmgf_202405/nigeria/vehicle_coverage/semolina.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(sem_cov)

# Edible oil: 
oil_cov <- plot_map(data = nga_coverage,
         col = "oil_cov",
         title = "Edible Oil",
         metric = "Coverage (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

oil_cov

# tmap_save(oil_cov, "data_rich/data_requests/bmgf_202405/nigeria/vehicle_coverage/oil.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(oil_cov)

# Sugar: 
sugar_cov <- plot_map(data = nga_coverage,
         col = "sugar_cov",
         title = "Sugar",
         metric = "Coverage (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

sugar_cov

# tmap_save(sugar_cov, "data_rich/data_requests/bmgf_202405/nigeria/vehicle_coverage/sugar.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(sugar_cov)

# Salt: 
salt_cov <- plot_map(data = nga_coverage,
         col = "salt_cov",
         title = "Salt",
         metric = "Coverage (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

salt_cov

# tmap_save(salt_cov, "data_rich/data_requests/bmgf_202405/nigeria/vehicle_coverage/salt.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(salt_cov)

# Bouillon:
bouillon_cov <- plot_map(data = nga_coverage,
         col = "bouillon_cov",
         title = "Bouillon",
         metric = "Coverage (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

bouillon_cov

# tmap_save(bouillon_cov, "data_rich/data_requests/bmgf_202405/nigeria/vehicle_coverage/bouillon.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(list = c("bouillon_cov", "nga_coverage"))

#-------------------------------------------------------------------------------

# PRODUCE MAPS - DIETARY INADEQUACY:

# Change colour palette of the plot function: 
plot_map <- function(data, col, title, metric, caption) {
  
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
    tm_text("zone", size = 0.5) +
    tm_borders(lwd = 0.2) + 
    tm_legend(show = F) + 
    tm_credits(caption, position = c("right", "bottom"), size = 0.5)
  
  return(map)
}

nga_inad$zone[nga_inad$zone == "South South"] <- "South South                                               "

# Iron: 
fe_inad <- plot_map(data = nga_inad,
         col = "fe_inad",
         title = "Iron",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

fe_inad

# tmap_save(fe_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/iron.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(fe_inad)

# Zinc: 
zn_inad <- plot_map(data = nga_inad,
         col = "zn_inad",
         title = "Zinc",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

zn_inad

# tmap_save(zn_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/zinc.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(zn_inad)

# Vitamin A:
va_inad <- plot_map(data = nga_inad,
         col = "va_inad",
         title = "Vitamin A",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

va_inad

# tmap_save(va_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/vitamin_a.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(va_inad)

# Folate: 
folate_inad <- plot_map(data = nga_inad,
         col = "fol_inad",
         title = "Folate",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

folate_inad

# tmap_save(folate_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/folate.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(folate_inad)

# Thiamine: 
thia_inad <- plot_map(data = nga_inad,
         col = "thia_inad",
         title = "Thiamine",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

thia_inad

# tmap_save(thia_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/thiamine.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(thia_inad)

# Riboflavin: 
ribo_inad <- plot_map(data = nga_inad,
         col = "ribo_inad",
         title = "Riboflavin",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

ribo_inad

# tmap_save(ribo_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/riboflavin.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(ribo_inad)

# Vitamin B12: 
vb12_inad <- plot_map(data = nga_inad,
         col = "vb12_inad",
         title = "Vitamin B12",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

vb12_inad

# tmap_save(vb12_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/vitamin_b12.png",
#           width = 9, height = 9, units = "in", dpi = 600)

# Calcium: 
ca_inad <- plot_map(data = nga_inad,
         col = "ca_inad",
         title = "Calcium",
         metric = "Prevalence of inadequate micronutrient intake (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

ca_inad

# tmap_save(ca_inad, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/calcium.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(ca_inad)

# Legend: 
legend <- tm_shape(nga_inad) + 
  tm_fill(col = "fe_inad", 
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
# 
# tmap_save(legend, "data_rich/data_requests/bmgf_202405/nigeria/inadequacy/legend.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(legend, vb12_inad, nga_inad)

#-------------------------------------------------------------------------------

# PRODUCE MAPS - VMD: 

nga_vmd$zone[nga_vmd$zone == "South South"] <- "South South                                               "

# Iron: 
fe_vmd <- plot_map(data = nga_vmd,
         col = "fe_vmd",
         title = "Iron",
         metric = "Prevalence of vitamin and mineral deficiency (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

fe_vmd

# tmap_save(fe_vmd, "data_rich/data_requests/bmgf_202405/nigeria/vmd/iron.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(fe_vmd)

# Zinc:
zn_vmd <- plot_map(data = nga_vmd,
         col = "zn_vmd",
         title = "Zinc",
         metric = "Prevalence of vitamin and mineral deficiency (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

zn_vmd

# tmap_save(zn_vmd, "data_rich/data_requests/bmgf_202405/nigeria/vmd/zinc.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(zn_vmd)

# Vitamin A:
va_vmd <- plot_map(data = nga_vmd,
         col = "va_vmd",
         title = "Vitamin A",
         metric = "Prevalence of vitamin and mineral deficiency (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

va_vmd

# tmap_save(va_vmd, "data_rich/data_requests/bmgf_202405/nigeria/vmd/vitamin_a.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(va_vmd)

# Folate:
folate_vmd <- plot_map(data = nga_vmd,
         col = "fol_vmd",
         title = "Folate",
         metric = "Prevalence of vitamin and mineral deficiency (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

folate_vmd

# tmap_save(folate_vmd, "data_rich/data_requests/bmgf_202405/nigeria/vmd/folate.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(folate_vmd)

# Thiamine:
thia_vmd <- plot_map(data = nga_vmd,
         col = "thia_vmd",
         title = "Thiamine",
         metric = "Prevalence of vitamin and mineral deficiency (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

thia_vmd

# tmap_save(thia_vmd, "data_rich/data_requests/bmgf_202405/nigeria/vmd/thiamine.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(thia_vmd)

# Riboflavin:
ribo_vmd <- plot_map(data = nga_vmd,
         col = "ribo_vmd",
         title = "Riboflavin",
         metric = "Prevalence of vitamin and mineral deficiency (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

ribo_vmd

# tmap_save(ribo_vmd, "data_rich/data_requests/bmgf_202405/nigeria/vmd/riboflavin.png",
#           width = 9, height = 9, units = "in", dpi = 600)

rm(ribo_vmd)

# Vitamin B12:
vb12_vmd <- plot_map(data = nga_vmd,
         col = "vb12_vmd",
         title = "Vitamin B12",
         metric = "Prevalence of vitamin and mineral deficiency (%)", 
         caption = "  \n
         \n
         \n
         Source: The Nigerian National Food Consumption and Micronutrient Survey 2021")

vb12_vmd

# tmap_save(vb12_vmd, "data_rich/data_requests/bmgf_202405/nigeria/vmd/vitamin_b12.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

# Legend: 
legend <- tm_shape(nga_vmd) + 
  tm_fill(col = "vb12_vmd", 
          palette = wesanderson::wes_palette("Zissou1Continuous"),
          style = "cont",
          breaks = seq(0, 100, by = 10),
          textNA= "Missing Data",
          title = "Prevalence of vitamin and mineral deficiency (%)",
          legend.is.portrait = F) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1,
            legend.height = 1,
            title.position = c(0.5, 0.5))

legend

# tmap_save(legend, "data_rich/data_requests/bmgf_202405/nigeria/vmd/legend.png", 
#           width = 9, height = 9, units = "in", dpi = 600)

rm(legend, vb12_vmd, nga_vmd)

#-------------------------------------------------------------------------------

# Remove all other objects: 
rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################
