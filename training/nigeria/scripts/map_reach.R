################################################################################
###################### SCRIPT FOR MAPPING REACH OF VEHICLES ####################
################################################################################

# Author: Mo Osman
# Collaborators: Gabriel Battcock & Kevin Tang
# Date created: 05-Mar-2024
# Last edited: 

# Script for mapping reach of fortification vehicles in Nigeria.

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

# READ IN DATA:

# Read in analysis data-frame:
analysis_df <- read_csv("training/nigeria/data/nga_lss1819_analysis_df.csv")

# Read in shapefiles:
nigeria_0 <- st_read("map_data/nga/new_shapefiles/nigeria_0")
nigeria_1 <- st_read("map_data/nga/new_shapefiles/nigeria_1")
nigeria_2 <- st_read("map_data/nga/new_shapefiles/nigeria_2")

# Check polygons: 
plot(nigeria_0$geometry)
plot(nigeria_1$geometry)
plot(nigeria_2$geometry)

#-------------------------------------------------------------------------------

# REACH OF VEHICLES: 

# Filter analysis data-frame to keep required variables: 
analysis_df <- analysis_df %>% 
  dplyr::select(hhid, state, lga, survey_wgt, rice, wheatflour, maizeflour,
                edible_oil, sugar)

# Create a variable for all staple grains combined:
analysis_df <- analysis_df %>% 
  mutate(staple_grains = ifelse(rice == 1 | wheatflour == 1 | maizeflour == 1, 1, 0))


# Calculate survey weighted reach of vehicles, at the LGA level - firstly need to
# create a tbl_svy object for analysis:
svy_analysis <- analysis_df %>% 
  as_survey_design(weights = survey_wgt)

# Calculate the reach of vehicles at the LGA level:
reach_lga <- svy_analysis %>% 
  group_by(lga) %>% 
  summarise(reach_rice = round(survey_mean(rice, na.rm = T, vartype = NULL), 
                               digits = 6),
            reach_wheatflour = round(survey_mean(wheatflour, na.rm = T, vartype = NULL),
                                     digits = 6),
            reach_maizeflour = round(survey_mean(maizeflour, na.rm = T, vartype = NULL), 
                                     digits = 6),
            reach_staple_grains = round(survey_mean(staple_grains, na.rm = T, vartype = NULL),
                                        digits = 6),
            reach_edible_oil = round(survey_mean(edible_oil, na.rm = T, vartype = NULL),
                                     digits = 6),
            reach_sugar = round(survey_mean(sugar, na.rm = T, vartype = NULL),
                                digits = 6)) %>% 
  ungroup()

# Multiply all numeric values by 100 to get percentages:
reach_lga <- reach_lga %>% 
  mutate(across(where(is.numeric), ~ .x * 100))

# Merge reach data to shapefiles: 
nigeria2_reach <- dplyr::left_join(nigeria_2, reach_lga, by = "lga")

#-------------------------------------------------------------------------------

# MAPPING

# Rice: 
rice <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_rice",
          title = "Rice", 
          palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data") + 
  tm_layout(main.title = "Rice", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.00001) + 
  tm_legend(show = F) +
  tm_shape(nigeria_1) +
  tm_borders(col = "black", lwd = 1)

# Wheatflour:
wheatflour <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_wheatflour",
          title = "Wheatflour", 
          palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data") + 
  tm_layout(main.title = "Wheatflour", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.00001) + 
  tm_legend(show = F) +
  tm_shape(nigeria_1) +
  tm_borders(col = "black", lwd = 1)

# Maizeflour:
maizeflour <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_maizeflour",
          title = "Maizeflour", 
          palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data") + 
  tm_layout(main.title = "Maizeflour", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.00001) + 
  tm_legend(show = F) +
  tm_shape(nigeria_1) +
  tm_borders(col = "black", lwd = 1)

# Staple Grains:
staple_grains <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_staple_grains",
          title = "Staple Grains", 
          palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data") + 
  tm_layout(main.title = "Staple Grains \n (combined)", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.00001) + 
  tm_legend(show = F) +
  tm_shape(nigeria_1) +
  tm_borders(col = "black", lwd = 1)

# Edible Oil:
edible_oil <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_edible_oil",
          title = "Edible Oil", 
          palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data") + 
  tm_layout(main.title = "Edible Oil", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.00001) + 
  tm_legend(show = F) +
  tm_shape(nigeria_1) +
  tm_borders(col = "black", lwd = 1)

# Sugar:
sugar <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_sugar",
          title = "Sugar", 
          palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data") + 
  tm_layout(main.title = "Sugar", frame = F, main.title.size = 0.8, 
            main.title.position = "center") +
  tm_borders(lwd = 0.00001) + 
  tm_legend(show = F) +
  tm_shape(nigeria_1) +
  tm_borders(col = "black", lwd = 1)

# Legend: 
legend <- tm_shape(nigeria2_reach) + 
  tm_fill(col = "reach_rice",
          palette = viridis(10, direction = -1), 
          style = "cont",
          textNA = "Missing Data",
          title = "Reach (% of households that consumed market acquired vehicle)", 
          legend.is.portrait = FALSE) + 
  tm_layout(legend.only = T,
            legend.position = c("center", "center"),
            legend.width = 1, 
            legend.height = 1)

# Store maps in a list:
reach_adm2 <- list(rice, wheatflour, maizeflour, staple_grains, edible_oil, sugar, legend)

# Combine maps, positioning the legend in the center of the last row:
reach_adm2 <- tmap_arrange(reach_adm2, ncol = 3, nrow = 3)

# Display map:
reach_adm2

# Save:
# tmap_save(reach_adm2, filename = "training/nigeria/reach.png", width = 9, height = 6, units = "in", dpi = 600

# Clear environment: 
rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################
